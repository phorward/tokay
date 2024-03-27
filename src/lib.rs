// Tokay
// Copyright © 2024 by Jan Max Meyer, Phorward Software Technologies.
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
pub use utils::run;
pub use value::{Dict, List, Object, RefValue, Str, Value};
pub use vm::{Accept, Capture, Context, Program, Reject};
