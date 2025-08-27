//! Tokay value and object representation
pub mod dict;
mod dynbuiltin;
pub mod iter;
pub mod list;
mod method;
mod object;
mod parselet;
mod refvalue;
pub mod str;
pub mod token;
pub mod value;

pub use self::str::Str;
pub use dict::Dict;
pub use dynbuiltin::DynBuiltin;
pub use iter::*;
pub use list::List;
pub use method::Method;
pub use object::{BoxedObject, Object};
pub(crate) use parselet::{Parselet, ParseletRef};
pub use refvalue::RefValue;
pub use token::Token;
pub use value::Value;

/** Value construction macro

value!() is used to easily construct Tokay values and objects from Rust natives.

Examples:
```
use tokay::value;

let i = value!(1);  // int
let s = value!("String");  // str
let l = value!([1, 2, 3]);  // list of int
let d = value!(["a" => 1, "b" => 2, "c" => 3]);  // dict
```
*/
#[macro_export]
macro_rules! value {
    ( [ $( $key:literal => $value:tt ),* ] ) => {
        {
            let mut dict = $crate::value::Dict::new();
            $( dict.insert_str($key, $crate::value!($value)); )*
            $crate::RefValue::from(dict)
        }
    };

    ( [ $( $value:tt ),* ] ) => {
        {
            let mut list = $crate::value::List::new();
            $( list.push($crate::value!($value)); )*
            $crate::RefValue::from(list)
        }
    };

    ( void ) => {
        $crate::RefValue::from($crate::value::Value::Void)
    };

    ( null ) => {
        $crate::RefValue::from($crate::value::Value::Null)
    };

    ( $value:expr ) => {
        $crate::RefValue::from($value)
    }
}
