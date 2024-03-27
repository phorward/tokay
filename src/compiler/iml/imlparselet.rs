//! Intermediate representation of a parselet
use super::*;
use crate::reader::Offset;
use crate::value::Parselet;
use indexmap::IndexMap;
use log;
use std::cell::RefCell;
use std::rc::Rc;

// ImlParseletModel
// ----------------------------------------------------------------------------

/** Intermediate parselet model.

The model defines the signature, local variables and code of a function or
parselet. It is shared by several parselet instances.

The struct was designed to be used for parselet construction during compilation,
therefore it provides an interface to define named and temporary variables
during compilation.
*/
#[derive(Debug, Clone)]
pub(in crate::compiler) struct ImlParseletModel {
    pub is_consuming: bool, // Flag if parselet is consuming
    pub locals: usize,      // Total number of local variables present (including arguments)
    pub signature: IndexMap<String, Option<ImlValue>>, // Arguments signature with default values
    pub variables: IndexMap<String, usize>, // Named local variables
    pub temporaries: Vec<usize>, // Unnamed temporary variables
    pub begin: ImlOp,       // Begin intermediate operations
    pub end: ImlOp,         // End intermediate operations
    pub body: ImlOp,        // Body intermediate Operations
}

impl ImlParseletModel {
    pub fn new(signature: Option<IndexMap<String, Option<ImlValue>>>) -> Self {
        let signature = signature.unwrap_or(IndexMap::new());
        // Generate variables from signature, addresses are enumerated!
        let variables = signature
            .keys()
            .enumerate()
            .map(|(index, key)| (key.to_string(), index))
            .collect();

        Self {
            is_consuming: false,
            locals: signature.len(),
            signature,
            variables,
            temporaries: Vec::new(),
            begin: ImlOp::Nop,
            end: ImlOp::Nop,
            body: ImlOp::Nop,
        }
    }

    // Return unique memory address of this model
    pub fn id(&self) -> usize {
        self as *const ImlParseletModel as usize
    }

    /// Allocate new variable
    fn allocate(&mut self) -> usize {
        let addr = self.locals;
        self.locals += 1;
        addr
    }

    /// Declare new or return address of named variables
    pub fn get_named(&mut self, name: &str) -> usize {
        match self.variables.get(name) {
            Some(addr) => *addr,
            None => {
                let addr = self.allocate();
                self.variables.insert(name.to_string(), addr);
                addr
            }
        }
    }

    /// Claim temporary (unnamed) variable.
    /// The variable is either being reused or freshly allocated.
    /// After use of the temporary address, return_temporary should be called.
    pub fn claim_temp(&mut self) -> usize {
        match self.temporaries.pop() {
            Some(addr) => addr,
            None => self.allocate(),
        }
    }

    // Returns a temporary variable address for (eventual) reuse later.
    pub fn return_temp(&mut self, addr: usize) {
        self.temporaries.push(addr)
    }
}

// ImlParseletInstance
// ----------------------------------------------------------------------------

/** Intermediate parselet instance.

A parselet instance is a model with as given generics definition.
The generics definition needs to be resolved first, before a parselet instance
is turned into a executable parselet.
*/
#[allow(dead_code)]
#[derive(Debug)]
pub(in crate::compiler) struct ImlParseletInstance {
    pub model: Rc<RefCell<ImlParseletModel>>, // Parselet base model
    pub generics: IndexMap<String, Option<ImlValue>>, // Generic signature with default configuration
    pub offset: Option<Offset>,                       // Offset of definition
    pub name: Option<String>,                         // Assigned name from source (for debugging)
    pub severity: u8,                                 // Capture push severity
    pub is_generated: bool,                           // Flag if parselet instance is auto-generated
}

/** Representation of parselet instance in intermediate code. */
impl ImlParseletInstance {
    pub fn new(
        model: Option<ImlParseletModel>,
        generics: Option<IndexMap<String, Option<ImlValue>>>,
        offset: Option<Offset>,
        name: Option<String>,
        severity: u8,
        is_generated: bool,
    ) -> Self {
        Self {
            model: Rc::new(RefCell::new(model.unwrap_or(ImlParseletModel::new(None)))),
            generics: generics.unwrap_or(IndexMap::new()),
            offset,
            name,
            severity,
            is_generated,
        }
    }

    pub fn id(&self) -> usize {
        self as *const ImlParseletInstance as usize
    }
}

impl std::fmt::Display for ImlParseletInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.name.as_deref().unwrap_or("<anonymous parselet>")
        )?;

        if !self.generics.is_empty() {
            write!(f, "<")?;
            for (i, (name, value)) in self.generics.iter().enumerate() {
                if let Some(value) = value {
                    write!(f, "{}{}:{}", if i > 0 { ", " } else { "" }, name, value)?;
                } else {
                    write!(f, "{}{}", if i > 0 { ", " } else { "" }, name)?;
                }
            }
            write!(f, ">")?;
        }

        Ok(())
    }
}

impl std::cmp::PartialEq for ImlParseletInstance {
    // It satisfies to just compare the parselet's memory address for equality
    fn eq(&self, other: &Self) -> bool {
        self.model.borrow().id() == other.model.borrow().id() && self.generics == other.generics
    }
}

impl Eq for ImlParseletInstance {}

impl std::hash::Hash for ImlParseletInstance {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let model = &*self.model.borrow();
        (model as *const ImlParseletModel as usize).hash(state);
        self.generics.iter().collect::<Vec<_>>().hash(state);
    }
}

impl std::cmp::PartialOrd for ImlParseletInstance {
    // It satisfies to just compare the parselet's memory address for equality
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id().partial_cmp(&other.id())
    }
}

// ImlParselet
// ----------------------------------------------------------------------------

/// Shared ImlParseletInstance
#[derive(Clone, Eq, PartialEq)]
pub(in crate::compiler) struct ImlParselet {
    instance: Rc<RefCell<ImlParseletInstance>>,
}

impl ImlParselet {
    pub fn new(parselet: ImlParseletInstance) -> Self {
        Self {
            instance: Rc::new(RefCell::new(parselet)),
        }
    }

    /** Derives an intermediate parselet by another intermediate parselet (`from`).

    The namespace defines the constant configuration of a surrounding parselet (`from`),
    and extends the intermediate parselet's constant configuration, making it a derivation.

    ```tokay
    A<X>: X           # intermediate generic parselet A
    B<Y>: 'x' A<Y>    # intermediate generic parselet B using a parselet instance of A

    B<'m'> B<'n'>     # parselet instances, construct the final parselets: B<'m'>, A<'m'>, B<'n'> A<'n'>
    ```

    The function either returns a derived parselet in case it was derived, otherwise it returns
    a clone of itself.
    */
    pub fn derive(&self, from: &ImlParselet) -> Result<Self, String> {
        let instance = self.instance.borrow();
        let mut generics = instance.generics.clone();
        let mut changes = false;
        let mut required = Vec::new();

        for (name, value) in generics.iter_mut() {
            // Replace any generics until no more are open
            while let Some(ImlValue::Generic { name, .. }) = value {
                *value = from.borrow().generics.get(name).unwrap().clone();
                changes = true;
            }

            if value.is_none() {
                required.push(name.to_string());
            }
        }

        // Check for accepted constant configuration
        if !required.is_empty() {
            return Err(format!(
                "{} requires assignment of generic argument {}",
                instance.name.as_deref().unwrap_or("<anonymous parselet>"),
                required.join(", ")
            ));
        }

        // When there is no change, there is no derivation
        if !changes {
            return Ok(self.clone());
        }

        Ok(Self::new(ImlParseletInstance {
            model: instance.model.clone(),
            generics,
            offset: instance.offset.clone(),
            name: instance.name.clone(),
            severity: instance.severity,
            is_generated: instance.is_generated,
        }))
    }

    /** Compiles an intermediate parselet into a compiled VM parselet,
    which is part of the provided `program` and indexed by `this`. */
    pub fn compile(&self, program: &mut ImlProgram, this: usize) -> Parselet {
        let instance = self.instance.borrow();
        let model = instance.model.borrow();

        log::debug!("compiling {}", instance);

        Parselet::new(
            Some(format!("{}", instance)),
            None,
            instance.severity,
            model
                .signature
                .iter()
                .map(|var_value| {
                    (
                        // Copy parameter name
                        var_value.0.clone(),
                        // Register default value, if any
                        var_value
                            .1
                            .as_ref()
                            .and_then(|value| Some(program.register(value))),
                    )
                })
                .collect(),
            model.locals,
            model.begin.compile_to_vec(program, (self, this)),
            model.end.compile_to_vec(program, (self, this)),
            model.body.compile_to_vec(program, (self, this)),
        )
    }
}

impl std::hash::Hash for ImlParselet {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.instance.borrow().hash(state);
    }
}

impl std::fmt::Debug for ImlParselet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.instance.borrow())
        // Avoid endless recursion in case of recursive parselets
        /*
        if self.0.try_borrow_mut().is_ok() {
            self.0.borrow().fmt(f)
        } else {
            write!(f, "{} (recursive)", self.0.borrow())
        }
        */
    }
}

impl std::fmt::Display for ImlParselet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.instance.borrow())
    }
}

impl std::ops::Deref for ImlParselet {
    type Target = Rc<RefCell<ImlParseletInstance>>;

    fn deref(&self) -> &Self::Target {
        &self.instance
    }
}

impl std::ops::DerefMut for ImlParselet {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.instance
    }
}

impl From<ImlParselet> for ImlValue {
    fn from(parselet: ImlParselet) -> Self {
        ImlValue::Parselet(parselet)
    }
}
