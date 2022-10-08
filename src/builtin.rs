//! Tokay built-in functions
use crate::_builtins::BUILTINS;
use crate::value;
use crate::value::{Dict, Object, RefValue, Value};
use crate::{Accept, Context, Reject};
extern crate self as tokay;
use std::io::{self, Write};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use tokay_macros::tokay_function;

// Abstraction of a built-in function
pub struct Builtin {
    pub name: &'static str, // Function's external name
    pub func: fn(Option<&mut Context>, Vec<RefValue>, Option<Dict>) -> Result<Accept, Reject>, // Function
}

impl Builtin {
    /// Retrieve builtin by name
    pub fn get(ident: &str) -> Option<&'static Builtin> {
        for builtin in &BUILTINS {
            if builtin.name == ident {
                return Some(builtin);
            }
        }

        None
    }

    /** Checks for a method on a value given by value type and method name.

    Methods are currently only native Rust functions provided via builtins.

    A method follows the naming convention <type>_<method>, so that the
    calls `"hello".upper()` and `str_upper("hello")` are calls to the
    same function.
    */
    pub fn get_method(type_name: &str, method_name: &str) -> Result<&'static Builtin, String> {
        for builtin in &BUILTINS {
            // todo: This stupid stuff finds the method name without allocating a string.
            // I'm sure this could be done better in some way...
            if builtin.name.starts_with(type_name)
                && builtin.name.ends_with(method_name)
                && builtin.name.len() == type_name.len() + method_name.len() + 1
                && builtin.name.chars().nth(type_name.len()) == Some('_')
            {
                return Ok(builtin);
            }
        }

        Err(format!("Method '{}_{}' not found", type_name, method_name))
    }

    /// Directly call builtin without context and specified parameters.
    pub fn call(
        &self,
        context: Option<&mut Context>,
        args: Vec<RefValue>,
    ) -> Result<Option<RefValue>, String> {
        // Call the builtin directly.
        match (self.func)(context, args, None) {
            Ok(Accept::Next | Accept::Hold) => Ok(None),
            Ok(Accept::Push(capture)) => Ok(Some(capture.get_value())),
            Err(Reject::Error(error)) => Err(error.message),
            other => Err(format!("Cannot handle {:?} on direct call", other)),
        }
    }
}

#[derive(Clone)]
pub struct BuiltinRef(pub &'static Builtin);

impl Serialize for BuiltinRef {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        todo!()
    }
}

impl<'de> Deserialize<'de> for BuiltinRef {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        todo!()
    }
}

impl Object for BuiltinRef {
    fn name(&self) -> &'static str {
        "builtin"
    }

    fn repr(&self) -> String {
        format!("<{} {}>", self.name(), self.0.name)
    }

    fn is_callable(&self, _without_arguments: bool) -> bool {
        true // Always callable, arguments are being checked by the function.
    }

    fn is_consuming(&self) -> bool {
        crate::utils::identifier_is_consumable(self.0.name)
    }

    fn call(
        &self,
        context: &mut Context,
        args: usize,
        nargs: Option<Dict>,
    ) -> Result<Accept, Reject> {
        let args = context.drain(args);
        (self.0.func)(Some(context), args, nargs)
    }
}

impl PartialEq for BuiltinRef {
    fn eq(&self, other: &Self) -> bool {
        self.0.name == other.0.name
    }
}

impl PartialOrd for BuiltinRef {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.name.partial_cmp(&other.0.name)
    }
}

impl std::fmt::Debug for BuiltinRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "<Builtin {}>", self.name)
        write!(f, "{}", self.0.name)
    }
}

impl From<&'static Builtin> for RefValue {
    fn from(builtin: &'static Builtin) -> Self {
        Value::Object(Box::new(BuiltinRef(builtin))).into()
    }
}

// Global built-ins

tokay_function!("chr(i)", {
    RefValue::from(format!(
        "{}",
        std::char::from_u32(i.to_usize()? as u32).unwrap()
    ))
    .into()
});

#[test]
fn test_chr() {
    assert_eq!(
        crate::run("i = ord(\"€\"); i chr(i)", ""),
        Ok(Some(value![[(8364 as usize), "€"]]))
    );
}

tokay_function!("ord(c)", {
    let c = c.to_string();
    if c.chars().count() != 1 {
        Err(format!(
            "{} expects a single character, but received string of length {}",
            __function,
            c.len()
        )
        .into())
    } else {
        RefValue::from(c.chars().next().unwrap() as usize).into()
    }
});

#[test]
fn test_ord() {
    assert_eq!(
        crate::run("ord(\"12\")", ""),
        Err(
            "Line 1, column 1: ord() expects a single character, but received string of length 2"
                .to_string()
        )
    );

    assert_eq!(
        crate::run("ord(\"\")", ""),
        Err(
            "Line 1, column 1: ord() expects a single character, but received string of length 0"
                .to_string()
        )
    );
}

tokay_function!("print(*args)", {
    if args.len() == 0 && context.is_some() {
        if let Some(capture) = context.unwrap().get_capture(0) {
            print!("{}", capture.to_string());
        }
    } else {
        for i in 0..args.len() {
            if i > 0 {
                print!(" ");
            }

            print!("{}", args[i].to_string());
        }
    }

    print!("\n");
    io::stdout().flush().unwrap();

    value!(void).into()
});

tokay_function!("repr(value)", value!(value.repr()).into());

#[test]
fn test_repr() {
    assert_eq!(
        crate::run("repr(\"Hello World\")", ""),
        Ok(Some(value!("\"Hello World\"")))
    );
}

tokay_function!("type(value)", value!(value.name()).into());

#[test]
fn test_type() {
    assert_eq!(
        crate::run(
            "type(void) type(true) type(1) type(23.5) type(\"hello\") type((1,2))",
            ""
        ),
        Ok(Some(value!([
            "void", "bool", "int", "float", "str", "list"
        ])))
    );
}

#[test]
fn test_buildin_call_error_reporting() {
    // Tests for calling functions with wrong parameter counts
    for (call, msg) in [
        (
            "str_replace()",
            "Line 1, column 1: str_replace() expected argument 's'",
        ),
        (
            "str_replace(1, 2, 3, 4, 5)",
            "Line 1, column 1: str_replace() expected at most 4 arguments (5 given)",
        ),
        (
            "str_replace(1, 2, x=3)",
            "Line 1, column 1: str_replace() doesn't accept named argument 'x'",
        ),
        (
            "str_replace(1, 2, x=3, y=4)",
            "Line 1, column 1: str_replace() doesn't accept named arguments (2 given)",
        ),
    ] {
        assert_eq!(crate::run(&call, ""), Err(msg.to_owned()));
    }
}
