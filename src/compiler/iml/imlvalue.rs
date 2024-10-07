//! Intermediate value representation
use super::*;
use crate::reader::Offset;
use crate::utils;
use crate::value::{Object, RefValue, Value};
use indexmap::IndexMap;
use log;
use num::ToPrimitive;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::compiler) struct ImlInstance {
    pub offset: Option<Offset>,                              // Source offset
    pub target: Box<ImlValue>,                               // Instance target
    pub args: Vec<(Option<Offset>, ImlValue)>,               // Sequential generic args
    pub nargs: IndexMap<String, (Option<Offset>, ImlValue)>, // Named generic args
    pub severity: Option<u8>,                                // optional desired severity
    pub is_generated: bool, // flag for generated parselet (e.g. using modifier)
}

impl ImlInstance {}

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
    Parselet(ImlRefParselet),      // Parselet
    Instance(ImlInstance),         // Instance
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
}

impl ImlValue {
    /**
    Helper function, which creates an instance definition from self,
    by turning self into name<self>.

    This is used internally to implement `Kle<P>` from `P*` syntax
    during the AST traversal.
    */
    pub fn into_generic(self, name: &str, severity: Option<u8>, offset: Option<Offset>) -> Self {
        Self::Instance(ImlInstance {
            offset: None,
            target: Box::new(ImlValue::Name {
                offset: None,
                name: name.to_string(),
            }),
            args: vec![(offset, self)],
            nargs: IndexMap::new(),
            severity,
            is_generated: true,
        })
    }

    /// Returns the value's definition offset, if available
    pub fn offset(&self) -> Option<Offset> {
        match self {
            Self::Shared(value) => value.borrow().offset(),
            Self::Name { offset, .. } | Self::Instance(ImlInstance { offset, .. }) => {
                offset.clone()
            }
            _ => None,
        }
    }

    /// Try to resolve, otherwise push a shared reference to the compiler's unresolved usages pool.
    pub fn try_resolve(mut self, scope: &Scope) -> Self {
        log::trace!("try_resolve {:?}", self);

        self = self.resolve(scope);

        match &self {
            Self::Name { .. } | Self::Instance { .. } => {
                log::trace!("Inserting new shared usage into scope");
                let shared = Self::Shared(Rc::new(RefCell::new(self)));
                scope.usages.borrow_mut().push(shared.clone());
                shared
            }
            Self::Shared(_) => {
                log::trace!("Re-inserting shared usage into scope (remains unresolved)");
                scope.usages.borrow_mut().push(self.clone());
                self
            }
            _ => self,
        }
    }

    /**
    Internal resolving function, which returns the resolved value of an ImlValue, when possible.

    - ImlValue::Shared are being followed and the innerst ImlValue is returned, either
      - by substituting the share when the share is on its own
      - returning a clone of the inner value, to keep the shares in place.
    - ImlValue::Name are being resolved by the compiler's symbol table
    */
    fn resolve(self, scope: &Scope) -> ImlValue {
        match self {
            Self::Shared(rc) => match Rc::try_unwrap(rc) {
                Ok(value) => {
                    log::trace!("resolving unwrapped shared");
                    value.into_inner().resolve(scope)
                }
                Err(rc) => {
                    log::trace!("resolving still wrapped shared");
                    let resolved = rc.borrow().clone().resolve(scope);

                    if matches!(resolved, Self::Name { .. } | Self::Instance { .. }) {
                        ImlValue::Shared(rc)
                    } else {
                        let mut value = rc.borrow_mut();
                        *value = resolved.clone();
                        resolved
                    }
                }
            },
            Self::Name { offset, ref name } => {
                let found = scope.resolve_name(offset.clone(), &name);
                log::trace!("resolving name {:?} to {:?}", name, found);
                found.unwrap_or(self)
            }
            /*
            Self::Instance(instance) => {
                log::trace!("resolving instance");
                log::trace!("  target = {:?}", instance.target);
                log::trace!("  args = {:?}", instance.args);
                log::trace!("  nargs = {:?}", instance.nargs);

                instance.target = Box::new(instance.target.resolve(scope));
                instance.args = instance
                    .args
                    .into_iter()
                    .map(|(offset, arg)| (offset, arg.resolve(scope)))
                    .collect();
                instance.nargs = instance
                    .nargs
                    .into_iter()
                    .map(|(name, (offset, narg))| (name, (offset, narg.resolve(scope))))
                    .collect();

                ImlValue::Instance(instance)
            }
            */
            Self::Instance(mut instance) => {
                let target = instance.target.resolve(scope);

                if let ImlValue::Parselet(parselet) = &target {
                    let parselet = parselet.borrow();
                    let mut generics = IndexMap::new();

                    // Map args and nargs to generics of this parselet
                    for (name, default) in parselet.generics.iter() {
                        // Take arguments by sequence first
                        let arg = if !instance.args.is_empty() {
                            let arg = instance.args.remove(0);
                            (arg.0, Some(arg.1.resolve(scope)))
                        }
                        // Otherwise, take named arguments
                        else if let Some(narg) = instance.nargs.shift_remove(name) {
                            (narg.0, Some(narg.1.resolve(scope)))
                        }
                        // Otherwise, use default
                        else {
                            (instance.offset.clone(), default.clone())
                        };

                        // Check integrity of constant names
                        if let (offset, Some(value)) = &arg {
                            if value.is_consuming() {
                                if !utils::identifier_is_consumable(name) {
                                    scope.push_error(
                                        *offset,
                                        format!(
                                            "Cannot assign consumable {} to non-consumable generic '{}'",
                                            value, name
                                        )
                                    );
                                }
                            } else if utils::identifier_is_consumable(name) {
                                scope.push_error(
                                    *offset,
                                    format!(
                                        "Cannot assign non-consumable {} to consumable generic {} of {}",
                                        value, name, parselet
                                    )
                                );
                            }
                        } else {
                            scope.push_error(
                                arg.0,
                                format!("Expecting argument for generic '{}'", name),
                            );
                        }

                        generics.insert(name.clone(), arg.1);
                    }

                    // Report any errors for remaining generic arguments.
                    if !instance.args.is_empty() {
                        scope.push_error(
                            instance.args[0].0, // report first parameter
                            format!(
                                "{} got too many generic arguments ({} given, {} expected)",
                                target,
                                generics.len() + instance.args.len(),
                                generics.len()
                            ),
                        );
                    }

                    for (name, (offset, _)) in instance.nargs {
                        if generics.get(&name).is_some() {
                            scope.push_error(
                                offset,
                                format!("{} already got generic argument '{}'", target, name),
                            );
                        } else {
                            scope.push_error(
                                offset,
                                format!(
                                    "{} does not accept generic argument named '{}'",
                                    target, name
                                ),
                            );
                        }
                    }

                    log::trace!("creating instance from {}", parselet);
                    for (k, v) in &generics {
                        log::trace!("  {} => {:?}", k, v);
                    }

                    // Make a parselet instance from the instance definition;
                    // This can be the final parselet instance, but constants
                    // might contain generic references as well, which are being
                    // resolved during further compilation and derivation.
                    let parselet = ImlRefParselet::new(ImlParselet {
                        model: parselet.model.clone(),
                        generics,
                        offset: instance.offset,
                        name: parselet.name.clone(),
                        severity: instance.severity.unwrap_or(parselet.severity),
                        is_generated: instance.is_generated,
                    });

                    log::info!("instance {} created", parselet);

                    return ImlValue::from(parselet);
                }

                instance.target = Box::new(target);
                Self::Instance(instance)
            }
            _ => self,
        }
    }

    /// Convert ImlValue into RefValue
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
            Self::SelfValue | Self::SelfToken | Self::VoidToken => true,
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
            Self::Instance(_) => true,
            _ => false,
        }
    }

    /// Check whether intermediate value represents consuming
    pub fn is_consuming(&self) -> bool {
        match self {
            Self::Shared(value) => value.borrow().is_consuming(),
            Self::SelfToken | Self::VoidToken => true,
            Self::Value(value) => value.is_consuming(),
            Self::Parselet(parselet) => parselet.borrow().model.borrow().is_consuming,
            Self::Name { name, .. } | Self::Generic { name, .. } => {
                crate::utils::identifier_is_consumable(name)
            }
            Self::Instance(instance) => instance.target.is_consuming(),
            _ => false,
        }
    }

    /** Compile a resolved intermediate value into VM code or register it as a static.

    The function will panic when the value is not resolved.
    */
    pub fn compile(
        &self,
        program: &mut ImlProgram,
        current: (&ImlRefParselet, usize),
        offset: &Option<Offset>,
        call: Option<Option<(usize, bool)>>,
        ops: &mut Vec<Op>,
    ) {
        if let Some(offset) = offset {
            ops.push(Op::Offset(Box::new(*offset)));
        }

        // First, try to push some Op for the value
        let op = match self {
            Self::Shared(value) => {
                return value.borrow().compile(program, current, offset, call, ops)
            }
            Self::Generic {
                name,
                offset: generic_offset,
            } => {
                return current.0.borrow().generics[name].as_ref().unwrap().compile(
                    program,
                    current,
                    &generic_offset.or(*offset),
                    call,
                    ops,
                )
            }
            Self::VoidToken => Some(Op::Next),
            Self::Value(value) => match &*value.borrow() {
                Value::Void => Some(Op::PushVoid),
                Value::Null => Some(Op::PushNull),
                Value::True => Some(Op::PushTrue),
                Value::False => Some(Op::PushFalse),
                Value::Int(i) => match i.to_i32() {
                    Some(0) => Some(Op::Push0),
                    Some(1) => Some(Op::Push1),
                    _ => None,
                },
                _ => None,
            },
            Self::Variable {
                is_global, addr, ..
            } => {
                if *is_global {
                    Some(Op::LoadGlobal(*addr))
                } else {
                    Some(Op::LoadFast(*addr))
                }
            }
            _ => None,
        };

        // Determine push or static load/call
        if let Some(op) = op {
            ops.push(op); // Push the op

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
        } else {
            // Register new static
            let idx = match self {
                ImlValue::Parselet(parselet) => match parselet.derive(current.0) {
                    Ok(parselet) => program.register(&ImlValue::Parselet(parselet)),
                    Err(msg) => {
                        program.push_error(offset.clone(), msg);
                        return;
                    }
                },
                ImlValue::Value(_) => program.register(self),
                ImlValue::SelfToken | ImlValue::SelfValue => current.1,
                _ => unreachable!("Can't compile {:?}", self),
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
            Self::Parselet(parselet) => write!(f, "{}", parselet),
            Self::Variable {
                name, is_global, ..
            } if *is_global => write!(f, "{}", name),
            Self::Variable { name, .. } => write!(f, "{}", name),
            Self::Name { name, .. } => write!(f, "{}", name),
            Self::Generic { name, .. } => write!(f, "{}", name),
            Self::Instance(instance) => {
                write!(f, "{}", instance.target)?;
                write!(f, "<")?;
                let mut first = true;

                for arg in &instance.args {
                    write!(f, "{}{}", if !first { ", " } else { "" }, arg.1)?;
                    first = false;
                }

                for narg in instance.nargs.keys() {
                    write!(
                        f,
                        "{}{}:{}",
                        if !first { ", " } else { "" },
                        narg,
                        instance.nargs[narg].1
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
                parselet.borrow().id().hash(state);
            }
            Self::SelfToken => state.write_u8('S' as u8),
            Self::SelfValue => state.write_u8('s' as u8),
            other => unreachable!("{:?} is unhashable", other),
        }
    }
}

impl From<RefValue> for ImlValue {
    fn from(value: RefValue) -> Self {
        Self::Value(value)
    }
}
