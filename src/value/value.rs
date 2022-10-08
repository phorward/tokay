//! Tokay value
use super::{BoxedObject, Dict, Object, RefValue};
use crate::{Accept, Context, Reject};
use tokay_macros::tokay_method;
extern crate self as tokay;
use num::{ToPrimitive, Zero};
use num_bigint::BigInt;
use serde::{Deserialize, Serialize};
use std::any::Any;
use std::cmp::Ordering;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    // Atomics
    Void,  // void
    Null,  // null
    True,  // true
    False, // false

    // Numerics
    Int(BigInt), // int
    Float(f64),  // float

    // Objects
    Object(BoxedObject), // object
}

impl Value {
    /// Return reference to object of type T.
    pub fn object<T: Any>(&self) -> Option<&T> {
        if let Self::Object(o) = self {
            return o.as_any().downcast_ref::<T>();
        }

        None
    }

    /// Return mutable reference to object of type T.
    pub fn object_mut<T: Any>(&mut self) -> Option<&mut T> {
        if let Self::Object(o) = self {
            return o.as_any_mut().downcast_mut::<T>();
        }

        None
    }

    /// Extract object of type T from Val.
    pub fn into_object<T: Any>(self) -> Option<T> {
        if let Self::Object(o) = self {
            if let Ok(inner) = o.into_any().downcast::<T>() {
                return Some(*inner);
            }
        }

        None
    }

    // Constructors
    tokay_method!("bool(value)", Ok(RefValue::from(value.is_true())));
    tokay_method!("int(value)", Ok(RefValue::from(value.to_bigint()?)));
    tokay_method!("float(value)", Ok(RefValue::from(value.to_f64()?)));

    // float methods
    tokay_method!(
        "float_ceil(float)",
        Ok(RefValue::from(float.to_f64()?.ceil()))
    );
    tokay_method!(
        "float_trunc(float)",
        Ok(RefValue::from(float.to_f64()?.trunc()))
    );
    tokay_method!(
        "float_fract(float)",
        Ok(RefValue::from(float.to_f64()?.fract()))
    );
}

impl Object for Value {
    fn severity(&self) -> u8 {
        match self {
            Self::Int(_) => 1,
            Self::Float(_) => 2,
            Self::Object(o) => o.severity(),
            _ => 0,
        }
    }

    fn name(&self) -> &'static str {
        match self {
            Self::Void => "void",
            Self::Null => "null",
            Self::True | Self::False => "bool",
            Self::Int(_) => "int",
            Self::Float(_) => "float",
            Self::Object(o) => o.name(),
        }
    }

    fn repr(&self) -> String {
        match self {
            Self::True => "true".to_string(),
            Self::False => "false".to_string(),
            Self::Int(i) => format!("{}", i),
            Self::Float(f) => format!("{}", f),
            Self::Object(o) => o.repr(),
            _ => self.name().to_string(),
        }
    }

    fn is_void(&self) -> bool {
        matches!(self, Value::Void)
    }

    fn is_true(&self) -> bool {
        match self {
            Self::True => true,
            Self::Int(i) => !i.is_zero(),
            Self::Float(f) => *f != 0.0,
            Self::Object(o) => o.is_true(),
            _ => false,
        }
    }

    fn to_i64(&self) -> Result<i64, String> {
        match self {
            Self::True => Ok(1),
            Self::Int(i) => Ok(i.to_i64().or(Some(0)).unwrap()),
            Self::Float(f) => Ok(*f as i64),
            Self::Object(o) => o.to_i64(),
            _ => Ok(0),
        }
    }

    fn to_f64(&self) -> Result<f64, String> {
        match self {
            Self::True => Ok(1.0),
            Self::Int(i) => Ok(i.to_f64().or(Some(0.0)).unwrap()),
            Self::Float(f) => Ok(*f),
            Self::Object(o) => o.to_f64(),
            _ => Ok(0.0),
        }
    }

    fn to_usize(&self) -> Result<usize, String> {
        match self {
            Self::True => Ok(1),
            Self::Int(i) => Ok(i.to_usize().or(Some(0)).unwrap()),
            Self::Float(f) => Ok(*f as usize),
            Self::Object(o) => o.to_usize(),
            _ => Ok(0),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Self::Void => "".to_string(),
            Self::Object(o) => o.to_string(),
            _ => self.repr(),
        }
    }

    fn to_bigint(&self) -> Result<BigInt, String> {
        match self {
            Self::True => Ok(BigInt::from(1)),
            Self::Int(i) => Ok(i.clone()),
            Self::Float(f) => Ok(BigInt::from(*f as i64)),
            Self::Object(o) => o.to_bigint(),
            _ => Ok(BigInt::from(0)),
        }
    }

    fn is_callable(&self, without_arguments: bool) -> bool {
        if let Self::Object(object) = self {
            object.is_callable(without_arguments)
        } else {
            false
        }
    }

    fn is_consuming(&self) -> bool {
        if let Self::Object(object) = self {
            object.is_consuming()
        } else {
            false
        }
    }

    fn is_nullable(&self) -> bool {
        if let Self::Object(object) = self {
            object.is_nullable()
        } else {
            false
        }
    }

    fn is_mutable(&self) -> bool {
        if let Self::Object(object) = self {
            object.is_mutable()
        } else {
            false
        }
    }

    fn call(
        &self,
        context: &mut Context,
        args: usize,
        nargs: Option<Dict>,
    ) -> Result<Accept, Reject> {
        if let Value::Object(object) = self {
            object.call(context, args, nargs)
        } else {
            Err(format!("'{}' object is not callable", self.name()).into())
        }
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Object(i), Self::Object(j)) => i.partial_cmp(j),
            (Self::Object(_), _) => Some(Ordering::Greater),
            (_, Self::Object(_)) => Some(Ordering::Less),

            (Self::Float(i), j) => i.partial_cmp(&j.to_f64().ok()?),
            (i, Self::Float(j)) => i.to_f64().ok()?.partial_cmp(j),

            (Self::Int(i), j) => i.partial_cmp(&j.to_bigint().ok()?),
            (i, j) => i.to_bigint().ok()?.partial_cmp(&j.to_bigint().ok()?),
        }
    }
}

impl From<bool> for RefValue {
    fn from(value: bool) -> Self {
        RefValue::from(if value { Value::True } else { Value::False })
    }
}

impl From<BigInt> for RefValue {
    fn from(int: BigInt) -> Self {
        RefValue::from(Value::Int(int))
    }
}

impl From<i64> for RefValue {
    fn from(int: i64) -> Self {
        RefValue::from(Value::Int(BigInt::from(int)))
    }
}

impl From<i32> for RefValue {
    fn from(int: i32) -> Self {
        RefValue::from(Value::Int(BigInt::from(int)))
    }
}

impl From<usize> for RefValue {
    fn from(addr: usize) -> Self {
        RefValue::from(Value::Int(BigInt::from(addr)))
    }
}

impl From<f64> for RefValue {
    fn from(float: f64) -> Self {
        RefValue::from(Value::Float(float))
    }
}

impl From<f32> for RefValue {
    fn from(float: f32) -> Self {
        RefValue::from(float as f64)
    }
}

#[test]
fn builtin_value_constructors() {
    assert_eq!(crate::run("bool(0)", ""), Ok(Some(crate::value!(false))));
    assert_eq!(crate::run("bool(1)", ""), Ok(Some(crate::value!(true))));

    assert_eq!(crate::run("int(13.37)", ""), Ok(Some(crate::value!(13))));
    assert_eq!(crate::run("int(\"42\")", ""), Ok(Some(crate::value!(42))));
    assert_eq!(crate::run("int(true)", ""), Ok(Some(crate::value!(1))));

    assert_eq!(crate::run("float(1)", ""), Ok(Some(crate::value!(1.0))));
    assert_eq!(
        crate::run("float(\"42.5\")", ""),
        Ok(Some(crate::value!(42.5)))
    );
    assert_eq!(crate::run("float(true)", ""), Ok(Some(crate::value!(1.0))));
}

#[test]
fn builtin_float_methods() {
    assert_eq!(crate::run("12.5.ceil()", ""), Ok(Some(crate::value!(13.0))));
    assert_eq!(
        crate::run("12.5.trunc()", ""),
        Ok(Some(crate::value!(12.0)))
    );
    assert_eq!(crate::run("12.5.fract()", ""), Ok(Some(crate::value!(0.5))));
}
