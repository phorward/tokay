//! Tokay value
use super::{BoxedObject, Dict, Object, RefValue};

use crate::{Accept, Context, Error, Reject};
use tokay_macros::tokay_method;
extern crate self as tokay;
use num::{ToPrimitive, Zero};
use num_bigint::BigInt;
use std::any::Any;
use std::cmp::Ordering;

#[cfg(feature = "serde")]
use super::{List, ParseletRef, Str, Token};
#[cfg(feature = "serde")]
use crate::builtin::BuiltinRef;
#[cfg(feature = "serde")]
use serde::{self, Deserialize, Serialize, ser::SerializeMap};

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "lowercase"))]
pub enum Value {
    // Atomics
    Void,  // void
    Null,  // null
    True,  // true
    False, // false

    // Numerics
    #[cfg_attr(
        feature = "serde",
        serde(
            serialize_with = "serialize_int_to_i64",
            deserialize_with = "deserialize_int_from_i64"
        )
    )]
    Int(BigInt), // int

    Float(f64), // float

    // Objects
    #[cfg_attr(feature = "serde", serde(
        untagged,  // https://play.rust-lang.org/?version=stable&mode=debug&edition=2024&gist=bf4276c00019146d787ffb5b710e31fb
        serialize_with = "serialize_object",
        deserialize_with = "deserialize_object"
    ))]
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
    tokay_method!("bool : @value", Ok(RefValue::from(value.is_true())));
    tokay_method!("int : @value", {
        if let Ok(value) = value.to_bigint() {
            Ok(RefValue::from(value))
        } else {
            Err(Error::from(format!(
                "`{}` cannot be converted to int",
                value.name()
            )))
        }
    });
    tokay_method!("float : @value", {
        if let Ok(value) = value.to_f64() {
            Ok(RefValue::from(value))
        } else {
            Err(Error::from(format!(
                "`{}` cannot be converted to float",
                value.name()
            )))
        }
    });

    // float methods
    tokay_method!(
        "float_ceil : @float",
        Ok(RefValue::from(float.to_f64()?.ceil()))
    );
    tokay_method!(
        "float_trunc : @float",
        Ok(RefValue::from(float.to_f64()?.trunc()))
    );
    tokay_method!(
        "float_fract : @float",
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
            Self::Float(f) => {
                if f.fract() == 0.0 {
                    format!("{}.0", f)
                } else {
                    format!("{}", f)
                }
            }
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
            Self::Int(i) => i
                .to_usize()
                .ok_or("Cannot convert BigInt to usize".to_string()),
            Self::Float(f) => Ok(*f as usize),
            Self::Object(o) => o.to_usize(),
            _ => Ok(0),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Self::Void => "".to_string(),
            Self::Float(f) => format!("{}", f),
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

    fn is_hashable(&self) -> bool {
        match self {
            Self::Void => false,
            Self::Object(object) => object.is_hashable(),
            _ => true,
        }
    }

    fn call(
        &self,
        context: Option<&mut Context>,
        args: Vec<RefValue>,
        nargs: Option<Dict>,
    ) -> Result<Accept, Reject> {
        if let Value::Object(object) = self {
            object.call(context, args, nargs)
        } else {
            Err(format!("'{}' is not callable", self.name()).into())
        }
    }

    fn call_direct(
        &self,
        context: &mut Context,
        args: usize,
        nargs: Option<Dict>,
    ) -> Result<Accept, Reject> {
        if let Value::Object(object) = self {
            object.call_direct(context, args, nargs)
        } else {
            Err(format!("'{}' is not callable", self.name()).into())
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

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.partial_cmp(other) {
            Some(ordering) => ordering,
            None => self.id().cmp(&other.id()),
        }
    }
}

// Serialization for Int
#[cfg(feature = "serde")]
fn serialize_int_to_i64<S>(value: &BigInt, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let i = value
        .to_i64()
        .ok_or_else(|| serde::ser::Error::custom("BigInt too big"))?;
    i.serialize(serializer)
}

#[cfg(feature = "serde")]
fn deserialize_int_from_i64<'de, D>(deserializer: D) -> Result<BigInt, D::Error>
where
    D: serde::Deserializer<'de>,
{
    Ok(BigInt::from(i64::deserialize(deserializer)?))
}

// Serialization for Object
#[cfg(feature = "serde")]
fn serialize_object<S>(value: &BoxedObject, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    macro_rules! downcast_serializer_to_type {
        () => {
            unimplemented!("Serializer for '{}' not specified", value.name())
        };

        ($type:ty $(, $rest:ty)*) => {
            if let Some(object) = value.as_any().downcast_ref::<$type>() {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry(object.name(), object)?;
                map.end()
            }
            else {
                downcast_serializer_to_type!($($rest),*)
            }
        };
    }

    downcast_serializer_to_type!(Str, List, Dict, ParseletRef, BuiltinRef, Token)
}

// Deserialization for Object
#[cfg(feature = "serde")]
fn deserialize_object<'de, D>(deserializer: D) -> Result<BoxedObject, D::Error>
where
    D: serde::Deserializer<'de>,
{
    struct ObjectVisitor;

    impl<'de> serde::de::Visitor<'de> for ObjectVisitor {
        type Value = BoxedObject;

        fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.write_str("map with one key")
        }

        fn visit_map<V>(self, mut map: V) -> Result<BoxedObject, V::Error>
        where
            V: serde::de::MapAccess<'de>,
        {
            match map.next_key::<&str>()? {
                Some("str") => Ok(Box::new(map.next_value::<Str>()?)),
                Some("list") => Ok(Box::new(map.next_value::<List>()?)),
                Some("dict") => Ok(Box::new(map.next_value::<Dict>()?)),
                Some("parselet") => Ok(Box::new(map.next_value::<ParseletRef>()?)),
                Some("builtin") => Ok(Box::new(map.next_value::<BuiltinRef>()?)),
                Some("token") => Ok(Box::new(map.next_value::<Token>()?)),
                Some(k) => Err(serde::de::Error::unknown_field(
                    k,
                    &["str", "list", "dict", "parselet", "builtin", "token"],
                )),
                None => Err(serde::de::Error::custom("expected a single-key map")),
            }
        }
    }

    deserializer.deserialize_map(ObjectVisitor)
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

impl From<u64> for RefValue {
    fn from(int: u64) -> Self {
        RefValue::from(Value::Int(BigInt::from(int)))
    }
}

impl From<i32> for RefValue {
    fn from(int: i32) -> Self {
        RefValue::from(Value::Int(BigInt::from(int)))
    }
}

impl From<u32> for RefValue {
    fn from(int: u32) -> Self {
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
