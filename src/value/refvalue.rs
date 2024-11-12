use super::{BoxedObject, Dict, Method, Object, Str, Token, Value};
use crate::builtin::{Builtin, BuiltinRef};
use crate::value;
use crate::{Accept, Context, Error, Reject};
use num::{ToPrimitive, Zero};
use num_bigint::BigInt;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct RefValue {
    value: Rc<RefCell<Value>>,
}

impl RefValue {
    /** Either creates a copy of a value or a reference, which is configured by the
    is_mutable() function of the underlying object. */
    pub fn ref_or_copy(self) -> Self {
        if self.is_mutable() {
            self
        } else {
            RefValue::from(self.borrow().clone())
        }
    }

    /** Creates a callable Method object from a RefValue and a given method name. */
    pub fn create_method(&self, method_name: &str) -> Result<RefValue, Error> {
        let builtin = Builtin::get_method(self.name(), method_name)?;
        return Ok(RefValue::from(Method {
            object: self.clone(),
            method: RefValue::from(builtin),
        }));
    }

    /** Performs a direct method call on a value.

    This function is designed to invoke methods on values directly from Rust code. */
    pub fn call_method(
        &self,
        name: &str,
        context: Option<&mut Context>,
        mut args: Vec<RefValue>,
    ) -> Result<Option<RefValue>, String> {
        let builtin = Builtin::get_method(self.name(), name)?;

        // Inject own value as first parameter.
        args.insert(0, self.clone());

        // Call the builtin directly.
        builtin.call(context, args)
    }

    pub fn unary_op(self, op: &str) -> Result<RefValue, String> {
        let name = {
            let this = &mut *self.borrow_mut();

            match this {
                Value::Object(this) => this.name(),
                Value::Float(float) => {
                    // float fast lane neg, iinc, idec
                    match op {
                        "neg" => return Ok(value!(-*float)),
                        "not" => return Ok(value!(*float == 0.0)),
                        "iinc" => {
                            *float += 1.0;
                            return Ok(self.clone());
                        }
                        "idec" => {
                            *float -= 1.0;
                            return Ok(self.clone());
                        }
                        _ => "float",
                    }
                }
                Value::Int(int) => {
                    // int fast lane neg, iinc, idec
                    match op {
                        "neg" => return Ok(value!(-int.clone())),
                        "not" => return Ok(value!(int.is_zero())),
                        "iinc" => {
                            *int += 1;
                            return Ok(self.clone());
                        }
                        "idec" => {
                            *int -= 1;
                            return Ok(self.clone());
                        }
                        _ => "int",
                    }
                }
                _ => "int", // fallback for any other type (void, null, bool)
            }
        };

        match Builtin::get_method(name, op) {
            Ok(builtin) => Ok(builtin.call(None, vec![self])?.unwrap()),
            Err(notfound) => match op {
                // default fallback for not
                "not" => Ok(value!(!self.is_true())),
                // default fallback for inline inc is an inline add by 1
                "iinc" if name == "int" => Ok(self.binary_op(value!(1 as i64), "iadd")?),
                // default fallback for inline dec is an inline sub by 1
                "idec" if name == "int" => Ok(self.binary_op(value!(1 as i64), "isub")?),
                _ => Err(notfound),
            },
        }
    }

    pub fn binary_op(self, operand: RefValue, op: &str) -> Result<RefValue, String> {
        let name = {
            // Operations starting with "i" are inline
            if op.starts_with("i") {
                // For fast-lane inline operations, self must be borrowed mutable.
                let mut this = self.borrow_mut();

                // In case the operand cannot be borrowed, self and operand might be the same.
                if let Ok(that) = operand.try_borrow() {
                    match (&mut *this, &*that) {
                        // Object wins by severity.
                        (Value::Object(_), _) | (_, Value::Object(_)) => {
                            if that.severity() > this.severity() {
                                Some(that.name())
                            } else {
                                Some(this.name())
                            }
                        }

                        // Float inline fast-lane
                        (Value::Float(float), _) => match op {
                            "iadd" => {
                                *float += that.to_f64()?;
                                return Ok(self.clone());
                            }
                            "imul" => {
                                *float *= that.to_f64()?;
                                return Ok(self.clone());
                            }
                            "isub" => {
                                *float -= that.to_f64()?;
                                return Ok(self.clone());
                            }
                            _ => None,
                        },

                        // Int inline fast-lane
                        (Value::Int(int), _) => match op {
                            "iadd" => {
                                *int += that.to_i64()?;
                                return Ok(self.clone());
                            }
                            "imul" => {
                                *int *= that.to_i64()?;
                                return Ok(self.clone());
                            }
                            "isub" => {
                                *int -= that.to_i64()?;
                                return Ok(self.clone());
                            }
                            _ => None,
                        },

                        _ => None,
                    }
                } else {
                    None
                }
            } else {
                let this = &*self.borrow();
                let that = &*operand.borrow();

                // Try to match operation
                match op {
                    "eq" => return Ok(value!(this == that)),
                    "neq" => return Ok(value!(this != that)),
                    "lt" => return Ok(value!(this < that)),
                    "lteq" => return Ok(value!(this <= that)),
                    "gt" => return Ok(value!(this > that)),
                    "gteq" => return Ok(value!(this >= that)),
                    _ => {}
                }

                // Try to match operation by type
                match (this, that) {
                    // Object wins by severity.
                    (Value::Object(_), _) | (_, Value::Object(_)) => {
                        if that.severity() > this.severity() {
                            Some(that.name())
                        } else {
                            Some(this.name())
                        }
                    }

                    (Value::Float(_), _) | (_, Value::Float(_)) if op != "divi" => match op {
                        "add" => return Ok(value!(this.to_f64()? + that.to_f64()?)),
                        "mul" => return Ok(value!(this.to_f64()? * that.to_f64()?)),
                        "sub" => return Ok(value!(this.to_f64()? - that.to_f64()?)),
                        "div" | "mod" => {
                            let dividend = this.to_f64()?;
                            let divisor = that.to_f64()?;

                            if divisor == 0.0 {
                                if op == "mod" {
                                    return Err(String::from("Modulo by zero"));
                                } else {
                                    return Err(String::from("Division by zero"));
                                }
                            }

                            if op == "mod" {
                                return Ok(value!(dividend % divisor));
                            } else {
                                return Ok(value!(dividend / divisor));
                            }
                        }
                        _ => None,
                    },

                    (_, _) => match op {
                        "add" => return Ok(value!(this.to_bigint()? + that.to_bigint()?)),
                        "mul" => return Ok(value!(this.to_bigint()? * that.to_bigint()?)),
                        "sub" => return Ok(value!(this.to_bigint()? - that.to_bigint()?)),
                        "div" | "divi" | "mod" => {
                            let dividend = this.to_bigint()?;
                            let divisor = that.to_bigint()?;

                            if divisor.is_zero() {
                                if op == "mod" {
                                    return Err(String::from("Modulo by zero"));
                                } else {
                                    return Err(String::from("Division by zero"));
                                }
                            }

                            if op == "divi" {
                                return Ok(value!(dividend / divisor));
                            }

                            let modres = &dividend % &divisor;

                            // If there's no remainder, perform an integer division
                            if modres.is_zero() {
                                if op == "mod" {
                                    return Ok(value!(0));
                                } else {
                                    return Ok(value!(dividend / divisor));
                                }
                            } else if op == "mod" {
                                return Ok(value!(modres));
                            }
                            // Otherwise do a floating point division
                            else {
                                let f_dividend = dividend.to_f64();
                                let f_divisor = divisor.to_f64();

                                if f_dividend.is_none() || f_divisor.is_none() {
                                    // just do an integer division
                                    return Ok(value!(dividend / divisor));
                                }

                                let f_dividend = f_dividend.unwrap();
                                let f_divisor = f_divisor.unwrap();

                                if f_divisor == 0.0 {
                                    return Err(String::from("Division by zero"));
                                }

                                return Ok(value!(f_dividend / f_divisor));
                            }
                        }
                        _ => None,
                    },
                }
            }
        };

        // When a type name was emitted, try to call builtin-function for operation
        if let Some(name) = name {
            match Builtin::get_method(name, op) {
                Ok(builtin) => {
                    return Ok(builtin
                        .call(None, vec![self, operand.ref_or_copy()])?
                        .unwrap())
                }
                // default "inline" operation is the non-inline operation assigning the result to itself
                Err(_) if op.starts_with("i") => {}
                Err(err) => return Err(err),
            }
        }

        // Perform expensive inline operation
        assert!(op.starts_with("i"));
        let res = self.clone().binary_op(operand, &op[1..])?;
        *self.borrow_mut() = res.into();
        Ok(self)
    }
}

impl Object for RefValue {
    fn id(&self) -> usize {
        self.borrow().id()
    }

    fn severity(&self) -> u8 {
        self.borrow().severity()
    }

    fn name(&self) -> &'static str {
        self.borrow().name()
    }

    fn repr(&self) -> String {
        self.borrow().repr()
    }

    fn is_void(&self) -> bool {
        matches!(&*self.borrow(), Value::Void)
    }

    fn is_true(&self) -> bool {
        self.borrow().is_true()
    }

    fn to_i64(&self) -> Result<i64, String> {
        self.borrow().to_i64()
    }

    fn to_f64(&self) -> Result<f64, String> {
        self.borrow().to_f64()
    }

    fn to_usize(&self) -> Result<usize, String> {
        self.borrow().to_usize()
    }

    fn to_string(&self) -> String {
        self.borrow().to_string()
    }

    fn to_bigint(&self) -> Result<BigInt, String> {
        self.borrow().to_bigint()
    }

    fn is_callable(&self, without_arguments: bool) -> bool {
        self.borrow().is_callable(without_arguments)
    }

    fn is_consuming(&self) -> bool {
        self.borrow().is_consuming()
    }

    fn is_nullable(&self) -> bool {
        self.borrow().is_nullable()
    }

    fn is_mutable(&self) -> bool {
        self.borrow().is_mutable()
    }

    fn is_hashable(&self) -> bool {
        self.borrow().is_hashable()
    }

    fn call(
        &self,
        context: Option<&mut Context>,
        args: Vec<RefValue>,
        nargs: Option<Dict>,
    ) -> Result<Accept, Reject> {
        self.borrow().call(context, args, nargs)
    }

    fn call_direct(
        &self,
        context: &mut Context,
        args: usize,
        nargs: Option<Dict>,
    ) -> Result<Accept, Reject> {
        self.borrow().call_direct(context, args, nargs)
    }
}

impl Hash for RefValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &*self.borrow() {
            Value::Void => state.write_u8('V' as u8),
            Value::Null => state.write_u8('N' as u8),
            Value::True => state.write_u8('T' as u8),
            Value::False => state.write_u8('F' as u8),
            Value::Int(i) => {
                state.write_u8('i' as u8);
                i.hash(state);
            }
            Value::Float(f) => {
                state.write_u8('f' as u8);
                f.to_bits().hash(state);
            }
            // If object and is hashable, try to downcast to...
            Value::Object(o) if o.is_hashable() => {
                // ...Str
                if let Some(s) = o.as_any().downcast_ref::<Str>() {
                    state.write_u8('s' as u8);
                    s.as_str().hash(state);
                }
                // ...BuiltinRef
                else if let Some(b) = o.as_any().downcast_ref::<BuiltinRef>() {
                    state.write_u8('b' as u8);
                    b.0.name.hash(state);
                }
                // ...Token
                else if let Some(t) = o.as_any().downcast_ref::<Token>() {
                    state.write_u8('t' as u8);
                    t.hash(state);
                }
                // or otherwise use the object's id as hashable value
                else {
                    state.write_u8('o' as u8);
                    o.id().hash(state);
                }
            }
            other => panic!("unhashable type '{}'", other.name()),
        }
    }
}

impl std::ops::Deref for RefValue {
    type Target = Rc<RefCell<Value>>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl std::ops::DerefMut for RefValue {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl Default for RefValue {
    fn default() -> Self {
        Value::Null.into()
    }
}

/*
impl std::fmt::Display for RefValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.borrow().repr())
    }
}
*/

impl std::fmt::Debug for RefValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.borrow().fmt(f)
    }
}

impl From<Value> for RefValue {
    fn from(value: Value) -> Self {
        RefValue {
            value: Rc::new(RefCell::new(value)),
        }
    }
}

impl From<RefValue> for Value {
    fn from(value: RefValue) -> Self {
        match std::rc::Rc::try_unwrap(value.value) {
            Ok(value) => value.into_inner(),
            Err(value) => value.borrow().clone(),
        }
    }
}

impl From<BoxedObject> for RefValue {
    fn from(value: BoxedObject) -> Self {
        RefValue {
            value: Rc::new(RefCell::new(Value::Object(value))),
        }
    }
}
