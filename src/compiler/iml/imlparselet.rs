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
    pub fn var(&mut self, name: &str) -> usize {
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
    pub fn temp(&mut self) -> usize {
        match self.temporaries.pop() {
            Some(addr) => addr,
            None => self.allocate(),
        }
    }

    // Returns a temporary variable address for (eventual) reuse later.
    pub fn untemp(&mut self, addr: usize) {
        self.temporaries.push(addr)
    }
}

// ImlParselet
// ----------------------------------------------------------------------------

/** Intermediate parselet.

An intermediate parselet is a parselet model with as given generics definition.
The generics definition needs to be resolved first, before an intermediate parselet
can be turned into a executable parselet.
*/
#[allow(dead_code)]
#[derive(Debug)]
pub(in crate::compiler) struct ImlParselet {
    pub model: Rc<RefCell<ImlParseletModel>>, // Parselet base model
    pub generics: IndexMap<String, Option<ImlValue>>, // Generic signature with default configuration
    pub offset: Option<Offset>,                       // Offset of definition
    pub name: Option<String>,                         // Assigned name from source (for debugging)
    pub severity: u8,                                 // Capture push severity
    pub is_generated: bool,                           // Flag if parselet is auto-generated
}

/** Representation of parselet parselet in intermediate code. */
impl ImlParselet {
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
        self as *const ImlParselet as usize
    }
}

impl std::fmt::Display for ImlParselet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.name.as_deref().unwrap_or("__AnonymousParselet__")
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

impl std::cmp::PartialEq for ImlParselet {
    // It satisfies to just compare the parselet's memory address for equality
    fn eq(&self, other: &Self) -> bool {
        self.model.borrow().id() == other.model.borrow().id() && self.generics == other.generics
    }
}

impl Eq for ImlParselet {}

impl std::hash::Hash for ImlParselet {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Hash is generated from the model and the generics configuration
        let model = &*self.model.borrow();
        (model as *const ImlParseletModel as usize).hash(state);
        self.generics.iter().collect::<Vec<_>>().hash(state);
    }
}

impl std::cmp::PartialOrd for ImlParselet {
    // It satisfies to just compare the parselet's memory address for equality
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id().partial_cmp(&other.id())
    }
}

// ImlRefParselet
// ----------------------------------------------------------------------------

/// Shared ImlParselet
#[derive(Clone, Eq, PartialEq)]
pub(in crate::compiler) struct ImlRefParselet {
    parselet: Rc<RefCell<ImlParselet>>,
}

impl ImlRefParselet {
    pub fn new(parselet: ImlParselet) -> Self {
        Self {
            parselet: Rc::new(RefCell::new(parselet)),
        }
    }

    /** Derives an intermediate parselet instance from the view of
    another intermediate parselet instance (`from`).

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
    pub fn derive(&self, from: &ImlRefParselet) -> Result<Self, String> {
        let parselet = self.parselet.borrow();

        // Fast track
        if parselet.generics.is_empty() {
            return Ok(self.clone());
        }

        let mut generics = parselet.generics.clone();
        let mut changes = false;
        let mut required = Vec::new();

        log::debug!("  deriving {} from {}", self, from);

        for (name, value) in generics.iter_mut() {
            // Replace any generics until no more are open;
            // need to do it in a loop, as generics can reference other generics.
            while let Some(ImlValue::Generic { name, .. }) = value {
                if name == "Self" || name == "self" {
                    *value = Some(ImlValue::Parselet(from.clone()));
                } else {
                    *value = if let Some(value) = from.borrow().generics.get(name) {
                        value.clone()
                    } else {
                        panic!("name = {:?} self {:?} from {:?}", name, self, from);
                    };
                }

                changes = true;
            }

            // Generics pointing to ImlValue::SelfToken/SelfValue must be replaced, too
            if changes && matches!(value, Some(ImlValue::SelfToken | ImlValue::SelfValue)) {
                *value = Some(ImlValue::Parselet(from.clone()));
            }

            if value.is_none() {
                required.push(name.to_string());
            }
        }

        // Check for accepted constant configuration
        if !required.is_empty() {
            return Err(format!(
                "'{}' requires assignment of generic argument '{}'",
                parselet.name.as_deref().unwrap_or("__AnonymousParselet__"),
                required.join(", ")
            ));
        }

        // When there is no change, there is no derivation
        if !changes {
            log::debug!("  no derivation");
            // log::warn!("  {} => {}", self, self);
            return Ok(self.clone());
        }

        // Create new derivative parselet
        let derived = Self::new(ImlParselet {
            model: parselet.model.clone(),
            generics,
            offset: parselet.offset.clone(),
            name: parselet.name.clone(),
            severity: parselet.severity,
            is_generated: parselet.is_generated,
        });

        log::debug!("  derived = {}", derived);
        // log::warn!("* {} => {}", self, derived);
        Ok(derived)
    }

    /** Compiles an intermediate parselet into a compiled VM parselet,
    which is part of the provided `program` and indexed by `index`. */
    pub fn compile(&self, program: &mut ImlProgram, index: usize) -> Parselet {
        let parselet = self.parselet.borrow();
        let model = parselet.model.borrow();

        log::debug!("compiling {}", parselet);

        Parselet::new(
            Some(format!("{}", parselet)),
            None,
            parselet.severity,
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
                            .and_then(|value| Some(program.register(value).unwrap_or_default())),
                    )
                })
                .collect(),
            model.locals,
            model.begin.compile_to_vec(program, (self, index)),
            model.end.compile_to_vec(program, (self, index)),
            model.body.compile_to_vec(program, (self, index)),
        )
    }
}

impl std::hash::Hash for ImlRefParselet {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.parselet.borrow().hash(state);
    }
}

impl std::fmt::Debug for ImlRefParselet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.parselet.borrow())
    }
}

impl std::fmt::Display for ImlRefParselet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Avoid endless recursion in case of recursive parselets
        if self.parselet.try_borrow_mut().is_ok() {
            self.parselet.borrow().fmt(f)
        } else {
            write!(
                f,
                "{}",
                self.parselet
                    .borrow()
                    .name
                    .as_deref()
                    .unwrap_or("__AnonymousParselet__")
            )
        }
    }
}

impl std::ops::Deref for ImlRefParselet {
    type Target = Rc<RefCell<ImlParselet>>;

    fn deref(&self) -> &Self::Target {
        &self.parselet
    }
}

impl std::ops::DerefMut for ImlRefParselet {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.parselet
    }
}

impl From<ImlRefParselet> for ImlValue {
    fn from(parselet: ImlRefParselet) -> Self {
        ImlValue::Parselet(parselet)
    }
}
