/**
 * This is a temporary solution for Tokay v0.7 which implements "dynamic builtin objects".
 *
 * It shall be replaced by a more generic solution later and merge Builtin and DynBuiltin
 * together. It is currently required by tokay-wasm to overwrite the default print-function.
 */
use crate::{Accept, Context, Dict, Object, RefValue, Reject, Value};

/// Dynamic abstraction of built-in functions.
pub struct DynBuiltin {
    pub name: &'static str,
    pub func:
        Box<dyn Fn(Option<&mut Context>, Vec<RefValue>, Option<Dict>) -> Result<Accept, Reject>>,
}

#[derive(Clone)]
pub struct DynBuiltinRef(std::rc::Rc<DynBuiltin>);

impl Object for DynBuiltinRef {
    fn name(&self) -> &'static str {
        self.0.name
    }

    fn repr(&self) -> String {
        self.0.name.to_string()
    }

    fn is_callable(&self, _without_arguments: bool) -> bool {
        true // Always callable, arguments are being checked by the function.
    }

    fn is_consuming(&self) -> bool {
        crate::utils::identifier_is_consumable(self.0.name)
    }

    fn call(
        &self,
        context: Option<&mut Context>,
        args: Vec<RefValue>,
        nargs: Option<Dict>,
    ) -> Result<Accept, Reject> {
        (self.0.func)(context, args, nargs)
    }

    fn call_direct(
        &self,
        context: &mut Context,
        args: usize,
        nargs: Option<Dict>,
    ) -> Result<Accept, Reject> {
        let args = context.drain(args);
        (self.0.func)(Some(context), args, nargs)
    }
}

impl PartialEq for DynBuiltinRef {
    fn eq(&self, other: &Self) -> bool {
        self.0.name == other.0.name
    }
}

impl PartialOrd for DynBuiltinRef {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.name.partial_cmp(&other.0.name)
    }
}

impl std::fmt::Debug for DynBuiltinRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.name)
    }
}

impl From<DynBuiltin> for RefValue {
    fn from(func: DynBuiltin) -> Self {
        Value::Object(Box::new(DynBuiltinRef(std::rc::Rc::new(func)))).into()
    }
}
