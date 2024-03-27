//! Intermediate value representation
use super::*;
use crate::reader::Offset;
use crate::utils;
use crate::value::{Object, RefValue, Value};
use crate::Error;
use indexmap::IndexMap;
use num::ToPrimitive;
use std::cell::RefCell;
use std::rc::Rc;

/** Intermediate value

Intermediate values are value descriptors that result during the compile process based on current
information from the syntax tree and symbol table information..

These can be memory locations of variables, static values, parselets or values whose definition is
still pending. As some intermediate values consist of other intermediate values, they are being
modified and resolved during the compilation process.
*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::compiler) enum ImlValue {
    Shared(Rc<RefCell<ImlValue>>), // Shared ImlValues are used for later resolve
    SelfValue,                     // self-reference (value)
    SelfToken,                     // Self-reference (consuming)
    VoidToken,                     // Void (consuming)
    Value(RefValue),               // Static value
    Parselet(ImlParselet),         // Parselet
    Variable {
        // Resolved variable
        offset: Option<Offset>, // Source offset
        name: String,           // Name
        is_global: bool,        // Global
        addr: usize,            // Address
    },
    Generic {
        // Generic placeholder
        offset: Option<Offset>, // Source offset
        name: String,           // Identifier
    },
    Name {
        // Unresolved name
        offset: Option<Offset>, // Source offset
        name: String,           // Identifier
    },
    Instance {
        // Unresolved parselet instance definition
        offset: Option<Offset>,                              // Source offset
        target: Box<ImlValue>,                               // Instance target
        args: Vec<(Option<Offset>, ImlValue)>,               // Sequential generic args
        nargs: IndexMap<String, (Option<Offset>, ImlValue)>, // Named generic args
        severity: Option<u8>,                                // optional desired severity
        is_generated: bool,
    },
}

impl ImlValue {
    /**
    Helper function, which creates an instance definition from self,
    by turning self into name<self>.

    This is used internally to implement `Kle<P>` from `P*` syntax
    during the AST traversal.
    */
    pub fn into_generic(self, name: &str, severity: Option<u8>, offset: Option<Offset>) -> Self {
        Self::Instance {
            offset: None,
            target: Box::new(ImlValue::Name {
                offset: None,
                name: name.to_string(),
            }),
            args: vec![(offset, self)],
            nargs: IndexMap::new(),
            severity,
            is_generated: true,
        }
    }

    /// Returns the value's definition offset, if available
    pub fn offset(&self) -> Option<Offset> {
        match self {
            Self::Shared(value) => value.borrow().offset(),
            Self::Name { offset, .. } | Self::Instance { offset, .. } => offset.clone(),
            _ => None,
        }
    }

    /// Try to resolve immediatelly, otherwise push shared reference to compiler's unresolved ImlValue.
    pub fn try_resolve(mut self, scope: &Scope) -> Self {
        self = self.resolve(scope);

        match &self {
            Self::Name { .. } | Self::Instance { .. } => {
                // Create a new shared ImlValue
                let shared = Self::Shared(Rc::new(RefCell::new(self)));
                scope.usages.borrow_mut().push(shared.clone());
                shared
            }
            Self::Shared(_) => {
                // Insert an already shared ImlValue into usages
                scope.usages.borrow_mut().push(self.clone());
                self
            }
            _ => self,
        }
    }

    /**
    Internal resolving function, which returns the resolved value of an ImlValue, when possible.

    - ImlValue::Shared are being followed
    - ImlValue::Name are being resolved by the compiler's symbol table
    - ImlValue::Instance are being recursively resolved to produce an ImlValue::Parselet

    Returns Some(value) with the resolved value on success, None otherwise.
    */
    fn resolve(self, scope: &Scope) -> ImlValue {
        match self {
            Self::Shared(rc) => {
                match Rc::try_unwrap(rc) {
                    Ok(value) => {
                        value.into_inner().resolve(scope)
                    }
                    Err(rc) => {
                        let resolved = rc.borrow().clone().resolve(scope);

                        if matches!(resolved, Self::Name { .. } | Self::Instance { .. }) {
                            ImlValue::Shared(rc)
                        } else {
                            let mut value = rc.borrow_mut();
                            *value = resolved.clone();
                            resolved
                        }
                    }
                }
            }
            Self::Name { offset, ref name } => {
                scope.resolve_name(offset.clone(), &name).unwrap_or(self)
            }
            Self::Instance {
                offset,
                target,
                mut args,
                mut nargs,
                severity,
                is_generated,
            } => {
                let target = target.resolve(scope);

                // Wie wär's, wenn eine Instance bis zum compilen eine Instance bleibt?

                if let ImlValue::Parselet(parselet) = &target {
                    let parselet = parselet.borrow();
                    let mut generics = IndexMap::new();

                    // Map args and nargs to generics of this parselet
                    // println!("--- map {:?} ---", parselet.name);

                    for (name, default) in parselet.generics.iter() {
                        // Take arguments by sequence first
                        let arg = if !args.is_empty() {
                            let arg = args.remove(0);
                            (arg.0, Some(arg.1.resolve(scope)))
                        }
                        // Otherwise, take named arguments
                        else if let Some(narg) = nargs.shift_remove(name) {
                            (narg.0, Some(narg.1.resolve(scope)))
                        }
                        // Otherwise, use default
                        else {
                            (offset.clone(), default.clone())
                        };

                        // println!(" {:?}", arg);

                        // Check integrity of constant names
                        if let (offset, Some(value)) = &arg {
                            if value.is_consuming() {
                                if !utils::identifier_is_consumable(name) {
                                    scope.error(
                                        *offset,
                                        format!(
                                            "Cannot assign consumable {} to non-consumable generic '{}'",
                                            value, name
                                        )
                                    );
                                }
                            } else if utils::identifier_is_consumable(name) {
                                scope.error(
                                    *offset,
                                    format!(
                                        "Cannot assign non-consumable {} to consumable generic {} of {}",
                                        value, name, parselet
                                    )
                                );
                            }
                        } else {
                            scope
                                .error(arg.0, format!("Expecting argument for generic '{}'", name));
                        }

                        generics.insert(name.clone(), arg.1);
                    }

                    // Report any errors for unconsumed generic arguments.
                    if !args.is_empty() {
                        scope.error(
                            args[0].0, // report first parameter
                            format!(
                                "{} got too many generic arguments ({} given, {} expected)",
                                target,
                                generics.len() + args.len(),
                                generics.len()
                            ),
                        );
                    }

                    for (name, (offset, _)) in nargs {
                        if generics.get(&name).is_some() {
                            scope.error(
                                offset,
                                format!("{} already got generic argument '{}'", target, name),
                            );
                        } else {
                            scope.error(
                                offset,
                                format!(
                                    "{} does not accept generic argument named '{}'",
                                    target, name
                                ),
                            );
                        }
                    }

                    // Make a parselet instance from the instance definition;
                    // This can be the final parselet instance, but constants
                    // might contain generic references as well, which are being
                    // resolved during further compilation and derivation.
                    return ImlValue::from(ImlParselet::new(ImlParseletInstance {
                        model: parselet.model.clone(),
                        generics,
                        offset: parselet.offset.clone(),
                        name: parselet.name.clone(),
                        severity: severity.unwrap_or(parselet.severity),
                        is_generated,
                    }));
                }

                Self::Instance {
                    offset,
                    target: Box::new(target),
                    args,
                    nargs,
                    severity,
                    is_generated,
                }
            }
            _ => self,
        }
    }

    /// Conert ImlValue into RefValue
    pub fn unwrap(self) -> RefValue {
        match self {
            Self::Value(value) => value,
            _ => unreachable!("{:?} cannot be unwrapped", self),
        }
    }

    /// Check whether intermediate value represents callable,
    /// and when its callable if with or without arguments.
    pub fn is_callable(&self, without_arguments: bool) -> bool {
        match self {
            Self::Shared(value) => value.borrow().is_callable(without_arguments),
            Self::SelfValue | Self::SelfToken => true, // fixme?
            Self::Value(value) => value.is_callable(without_arguments),
            Self::Parselet(parselet) => {
                let parselet = parselet.borrow();
                let parselet = parselet.model.borrow();

                if without_arguments {
                    parselet.signature.len() == 0
                        || parselet.signature.iter().all(|arg| arg.1.is_some())
                } else {
                    true
                }
            }
            Self::Instance { .. } => true,
            _ => false,
        }
    }

    /// Check whether intermediate value represents consuming
    pub fn is_consuming(&self) -> bool {
        match self {
            Self::Shared(value) => value.borrow().is_consuming(),
            Self::SelfValue => false,
            Self::SelfToken | Self::VoidToken => true,
            Self::Value(value) => value.is_consuming(),
            Self::Parselet(parselet) => parselet.borrow().model.borrow().is_consuming,
            Self::Name { name, .. } | Self::Generic { name, .. } => {
                crate::utils::identifier_is_consumable(name)
            }
            Self::Instance { target, .. } => target.is_consuming(),
            _ => false,
        }
    }

    /** Compile a resolved intermediate value into VM code or register it as a static.

    The function will panic when the value is not resolved.
    */
    pub fn compile(
        &self,
        program: &mut ImlProgram,
        current: (&ImlParselet, usize),
        offset: &Option<Offset>,
        call: Option<Option<(usize, bool)>>,
        ops: &mut Vec<Op>,
    ) {
        if let Some(offset) = offset {
            ops.push(Op::Offset(Box::new(*offset)));
        }

        // Remember current ops start
        let start = ops.len();

        match self {
            ImlValue::Shared(value) => {
                return value.borrow().compile(program, current, offset, call, ops)
            }
            ImlValue::VoidToken => ops.push(Op::Next),
            ImlValue::Value(value) => match &*value.borrow() {
                Value::Void => ops.push(Op::PushVoid),
                Value::Null => ops.push(Op::PushNull),
                Value::True => ops.push(Op::PushTrue),
                Value::False => ops.push(Op::PushFalse),
                Value::Int(i) => match i.to_i32() {
                    Some(0) => ops.push(Op::Push0),
                    Some(1) => ops.push(Op::Push1),
                    _ => {}
                },
                _ => {}
            },
            ImlValue::Variable {
                addr, is_global, ..
            } => {
                if *is_global {
                    ops.push(Op::LoadGlobal(*addr))
                } else {
                    ops.push(Op::LoadFast(*addr))
                }
            }
            ImlValue::Generic { name, .. } => {
                return current.0.borrow().generics[name]
                    .as_ref()
                    .unwrap()
                    .compile(program, current, offset, call, ops)
            }
            ImlValue::SelfValue | ImlValue::SelfToken | ImlValue::Parselet(_) => {}
            _ => unreachable!("{}", self),
        }

        // Check if something has been pushed before.
        if start == ops.len() {
            let idx = match self {
                ImlValue::SelfValue | ImlValue::SelfToken => current.1, // use current index
                ImlValue::Parselet(parselet) => match parselet.derive(current.0) {
                    Ok(parselet) => program.register(&ImlValue::Parselet(parselet)),
                    Err(msg) => {
                        program.errors.push(Error::new(offset.clone(), msg));
                        return;
                    }
                },
                resolved => program.register(resolved),
            };

            match call {
                // Load
                None => ops.push(Op::LoadStatic(idx)),
                // Call or load
                Some(None) => {
                    if self.is_callable(true) {
                        ops.push(Op::CallStatic(idx));
                    } else {
                        ops.push(Op::LoadStatic(idx));
                    }
                }
                // Call (qualified)
                Some(Some((0, false))) => ops.push(Op::CallStatic(idx)),
                Some(Some((args, false))) => ops.push(Op::CallStaticArg(Box::new((idx, args)))),
                Some(Some((args, true))) => ops.push(Op::CallStaticArgNamed(Box::new((idx, args)))),
            }
        } else {
            match call {
                // Load (already done previously)
                None => {}
                // Call or load
                Some(None) => ops.push(Op::CallOrCopy),
                // Call (qualified)
                Some(Some((0, false))) => ops.push(Op::Call),
                Some(Some((args, false))) => ops.push(Op::CallArg(args)),
                Some(Some((args, true))) => ops.push(Op::CallArgNamed(args)),
            }
        }
    }
}

impl std::fmt::Display for ImlValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Shared(value) => value.borrow().fmt(f),
            Self::SelfValue => write!(f, "self"),
            Self::SelfToken => write!(f, "Self"),
            Self::VoidToken => write!(f, "Void"),
            Self::Value(value) => write!(f, "{}", value.repr()),
            Self::Parselet(parselet) => {
                write!(
                    f,
                    "{}",
                    parselet
                        .borrow()
                        .name
                        .as_deref()
                        .unwrap_or("<anonymous parselet>")
                )
            }
            Self::Variable {
                name, is_global, ..
            } if *is_global => write!(f, "{}", name),
            Self::Variable { name, .. } => write!(f, "{}", name),
            Self::Name { name, .. } => write!(f, "{}", name),
            Self::Generic { name, .. } => write!(f, "{}", name),
            Self::Instance {
                target,
                args,
                nargs,
                ..
            } => {
                write!(f, "{}", target)?;

                write!(f, "<")?;
                let mut first = true;

                for arg in args {
                    write!(f, "{}{}", if !first { ", " } else { "" }, arg.1)?;
                    first = false;
                }

                for narg in nargs.keys() {
                    write!(
                        f,
                        "{}{}:{}",
                        if !first { ", " } else { "" },
                        narg,
                        nargs[narg].1
                    )?;
                    first = false;
                }

                write!(f, ">")
            }
        }
    }
}

impl std::hash::Hash for ImlValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Shared(value) => value.borrow().hash(state),
            Self::VoidToken => state.write_u8('V' as u8),
            Self::Value(value) => {
                state.write_u8('v' as u8);
                value.hash(state)
            }
            Self::Parselet(parselet) => {
                state.write_u8('p' as u8);
                parselet.hash(state);
            }
            Self::SelfValue => state.write_u8('s' as u8),
            Self::SelfToken => state.write_u8('S' as u8),
            other => unreachable!("{:?} is unhashable", other),
        }
    }
}

impl From<RefValue> for ImlValue {
    fn from(value: RefValue) -> Self {
        Self::Value(value)
    }
}
