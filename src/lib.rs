// Tokay
// Copyright © 2025 by Jan Max Meyer, Phorward Software Technologies.
// Licensed under the MIT license. See LICENSE for more information.

/*! Tokay

    A programming language designed for ad-hoc parsing.

    Visit [https://tokay.dev](https://tokay.dev) for more information.
*/

mod _builtins; // Generated builtin registry
pub mod builtin;
pub mod compiler;
pub mod error;
pub mod reader;
#[cfg(test)]
pub mod test;
mod utils;
pub mod value;
pub mod vm;

pub use compiler::Compiler;
pub use error::Error;
pub use reader::Reader;
pub use value::{Dict, List, Object, RefValue, Str, Value};
pub use vm::{Accept, Capture, Context, Program, Reject};

/** Compile and evaluate a piece of Tokay code on optional input.

`code` is the actual code being compiled and executed.
`input` is an optional input to work on; This can be an empty str.
`vars` is a way to optionally hand global variables over into the program.

Returns Ok(RefValue) with a given result value, or Err(Error) when something went wrong.
 */
pub fn eval(
    code: &str,
    input: &str,
    vars: Option<std::collections::HashMap<String, RefValue>>,
) -> Result<RefValue, Error> {
    let mut compiler = Compiler::new();

    if let Some(vars) = vars {
        for (key, value) in vars.into_iter() {
            compiler.global(&key, value);
        }
    }

    match compiler.compile_from_str(code) {
        Ok(None) => Ok(RefValue::from(Value::Void)),
        Ok(Some(program)) => {
            let mut reader = Reader::new(None, Box::new(std::io::Cursor::new(input.to_string())));
            let mut thread = vm::Thread::new(&program, vec![&mut reader]);

            match thread.run() {
                Ok(Some(value)) => Ok(value),
                Err(error) => Err(error),
                _ => Ok(RefValue::from(Value::Void)),
            }
        }
        Err(mut errors) => Err(errors.remove(0)),
    }
}

#[test]
// Simple testcase for eval
fn test_eval() {
    assert_eq!(
        eval(
            "2 * a + Int",
            "23",
            Some(std::collections::HashMap::from([(
                "a".to_string(),
                value!(5)
            )]))
        ),
        Ok(value!(33))
    );
}
