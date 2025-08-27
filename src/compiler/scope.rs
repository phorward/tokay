/** Compiler symbolic scopes.

In Tokay code, this relates to any block.
Parselets introduce new variable scopes.
Loops introduce a new loop scope.
*/
use super::*;
use crate::builtin::Builtin;
use crate::error::Error;
use crate::reader::*;
use crate::value::{RefValue, Token};
use indexmap::IndexMap;
use std::cell::RefCell;

pub(super) enum ScopeLevel {
    Parselet(ImlRefParselet), // parselet level (refers to currently constructed parselet)
    Block,                    // block level (constants can be defined here)
    Loop,                     // loop level (allows the use of break & continue)
}

pub(super) struct Scope<'compiler, 'parent> {
    pub compiler: &'compiler Compiler, // reference to compiler
    pub level: ScopeLevel,             // Scope level
    parent: Option<&'parent Scope<'compiler, 'parent>>, // Previous scope
    pub constants: RefCell<IndexMap<String, ImlValue>>, // Symbol table of named constants
    pub usages: RefCell<Vec<ImlValue>>, // Unresolved usages within scope
    pub errors: RefCell<Vec<Error>>,   // Errors raised
}

impl<'compiler, 'parent> Scope<'compiler, 'parent> {
    /** Create a new scope in compiler, with scopelevel level, and optional parent.

    Use self.shadow() for simpler scope creation. */
    pub fn new(
        compiler: &'compiler Compiler,
        level: ScopeLevel,
        parent: Option<&'parent Scope<'compiler, 'parent>>,
    ) -> Self {
        let scope = Self {
            compiler,
            level,
            parent,
            constants: RefCell::new(IndexMap::new()),
            usages: RefCell::new(Vec::new()),
            errors: RefCell::new(Vec::new()),
        };

        // Register standard whitespace
        if scope.parent.is_none() {
            scope.define_constant(
                "_",
                RefValue::from(Token::builtin("Whitespaces").unwrap()).into(),
            );
        }

        scope
    }

    /// Create a new scope with given level and and shadow self until the new scope outlives
    pub fn shadow(&'parent self, level: ScopeLevel) -> Self {
        Self::new(self.compiler, level, Some(self))
    }

    /// Check if self is the global scope
    pub fn is_global(&self) -> bool {
        match self.level {
            ScopeLevel::Parselet(_) => self.parent.is_none(),
            _ => self.parent.as_ref().unwrap().is_global(),
        }
    }

    /// Check if scope is a loop-scope.
    pub fn is_loop(&self) -> bool {
        match self.level {
            ScopeLevel::Loop => true,
            ScopeLevel::Parselet(_) => false,
            _ => self.parent.as_ref().unwrap().is_loop(),
        }
    }

    /// Retrieve current outer parselet; In global scope, this is the __main__ parselet.
    pub fn parselet(&self) -> ImlRefParselet {
        match &self.level {
            ScopeLevel::Parselet(parselet) => parselet.clone(),
            _ => self.parent.as_ref().unwrap().parselet(),
        }
    }

    /// Register variable with name
    pub fn register_variable(&self, name: &str) {
        self.parselet().borrow().model.borrow_mut().var(name);
    }

    /// Define constant value to name in current scope.
    pub fn define_constant(&self, name: &str, mut value: ImlValue) {
        /*
            Special meaning for whitespace constants names "_" and "__".

            When set, the corresponding consumable Value becomes the following:

            - `__ : Value+`
            - `_ : __?`

            This is always the case whenever "_" or "__" is set.
            Fallback defaults to `Value : Whitespace`, handled in get_constant().
        */
        let mut secondary = None;

        if name == "_" || name == "__" {
            // `__` becomes `Value+`
            value = value.into_generic("Pos", Some(0), None).try_resolve(self);
            secondary = Some(("__", value.clone()));

            // ...and then in-place "_" is defined as `_ : __?`
            value = value.into_generic("Opt", Some(0), None).try_resolve(self);
        }

        // Insert constant into current scope
        let mut constants = self.constants.borrow_mut();

        if let Some((name, value)) = secondary {
            constants.insert(name.to_string(), value);
        }

        constants.insert(name.to_string(), value);
    }

    /// Resolve a name starting from the current scope.
    pub fn resolve_name(&self, offset: Option<Offset>, name: &str) -> Option<ImlValue> {
        let mut top = Some(self);
        let mut top_parselet = true;

        while let Some(scope) = top {
            // Check constants of scope
            if let Some(value) = scope.constants.borrow().get(name) {
                return Some(value.clone());
            }

            if let ScopeLevel::Parselet(parselet) = &scope.level {
                if top_parselet
                    && (parselet.borrow().generics.get(name).is_some()
                        || name == "Self"
                        || name == "self")
                {
                    return Some(ImlValue::Generic {
                        offset,
                        name: name.to_string(),
                    });
                }

                // Check for variable only in first or global scope
                if scope.parent.is_none() || top_parselet {
                    let parselet = parselet.borrow();

                    if let Some(addr) = parselet.model.borrow().variables.get(name) {
                        return Some(ImlValue::Variable {
                            offset,
                            name: name.to_string(),
                            is_global: scope.parent.is_none(),
                            addr: *addr,
                        });
                    };
                }

                top_parselet = false;
            }

            top = scope.parent.as_deref();
        }

        // Check for a builtin function
        if let Some(builtin) = Builtin::get(name) {
            return Some(RefValue::from(builtin).into());
        }

        // Check for built-in token
        if let Some(value) = Token::builtin(name) {
            return Some(RefValue::from(value).into());
        }

        None
    }

    /// Resolve any open usages within the current scope.
    pub fn resolve_usages(&self) {
        let resolve: Vec<ImlValue> = self.usages.borrow_mut().drain(..).collect();

        // Try to resolve open usages, keep then when they are still unresolved
        for value in resolve.into_iter() {
            value.try_resolve(self);
        }
    }

    /// Push an Error to the scope's error log, with given offset and msg.
    pub fn push_error(&self, offset: Option<Offset>, msg: String) {
        self.errors.borrow_mut().push(Error::new(offset, msg))
    }
}

impl<'compiler, 'parent> Drop for Scope<'compiler, 'parent> {
    fn drop(&mut self) {
        self.resolve_usages();

        match &mut self.parent {
            Some(parent) => {
                parent
                    .usages
                    .borrow_mut()
                    .extend(self.usages.borrow_mut().drain(..));
                parent
                    .errors
                    .borrow_mut()
                    .extend(self.errors.borrow_mut().drain(..));
            }
            None => return,
        }
    }
}
