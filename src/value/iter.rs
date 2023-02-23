//! An iterator, probably running on a given object
use super::{Object, RefValue, Value};
use crate::value;
use crate::Error;
use num_bigint::BigInt;
use std::cell::RefCell;
use std::rc::Rc;
use tokay_macros::{tokay_function, tokay_method};
extern crate self as tokay;

pub trait RefValueIter {
    fn next(&mut self) -> Option<RefValue>;
    fn repr(&self) -> String;
    fn rev(&mut self) -> Result<(), Error> {
        Err(Error::from("This iterator cannot be reversed."))
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct MethodIter {
    pub object: RefValue,
    pub object_method: &'static str,
    pub index: Option<RefValue>,
    pub index_op: &'static str,
}

impl MethodIter {
    /// Creates a new iterator on object, with default "get_item"-method and "iinc"-operation.
    pub fn new(object: RefValue) -> Iter {
        Self::new_method_iter(object, "get_item", None, "iinc")
    }

    /// Creates a new iterator on object, using item retrieval method and op operation.
    /// index can be set to an optional start value; If None, the iterator will be initialized with Some(0).
    pub fn new_method_iter(
        object: RefValue,
        object_method: &'static str,
        index: Option<RefValue>,
        index_op: &'static str,
    ) -> Iter {
        Iter {
            iter: Rc::new(RefCell::new(Self {
                object: object.clone(),
                object_method,
                index: index.or_else(|| Some(value!(0))),
                index_op,
            })),
        }
    }
}

impl RefValueIter for MethodIter {
    fn next(&mut self) -> Option<RefValue> {
        if let Some(index) = &self.index {
            match self
                .object
                .call_method(self.object_method, vec![index.clone()])
            {
                Ok(Some(next)) => {
                    // When next is not void, increment index and return next
                    if !next.is_void() {
                        self.index = Some(index.clone().unary_op(self.index_op).unwrap());
                        return Some(next);
                    }
                }
                _ => {
                    // Special case: Return the object itself once as its own iter
                    if !index.is_true() {
                        self.index = None; // Invalidate this iterator
                        return Some(self.object.clone());
                    }
                }
            }

            self.index = None; // Invalidate this iterator
        }

        None
    }

    fn repr(&self) -> String {
        "<MethodIter on {} >".to_string()
    }
}

struct RangeIter {
    next: Option<BigInt>,
    stop: Option<BigInt>,
    step: BigInt,
}

impl RefValueIter for RangeIter {
    fn next(&mut self) -> Option<RefValue> {
        if let (Some(next), Some(stop)) = (self.next.as_mut(), &self.stop) {
            if *next < *stop {
                let ret = next.clone();
                *next += &self.step;
                return Some(RefValue::from(ret));
            }

            self.next = None;
            self.stop = None;
        }

        None
    }

    fn repr(&self) -> String {
        "#todo range(...)".to_string()
    }
}

tokay_function!("range : @start, stop=void, step=1", {
    let start = if stop.is_void() {
        stop = start;
        BigInt::from(0)
    } else {
        start.to_bigint()?
    };

    let stop = stop.to_bigint()?;
    let step = step.to_bigint()?;

    RefValue::from(Iter {
        iter: Rc::new(RefCell::new(RangeIter {
            next: Some(start),
            stop: Some(stop),
            step,
        })),
    })
    .into()
});

#[derive(Clone)]
pub struct Iter {
    iter: Rc<RefCell<dyn RefValueIter>>,
}

impl Iter {
    tokay_method!("iter : @value", {
        if value.is("iter") || value.is_void() {
            Ok(value)
        }
        // Check for an available iter() method on the provided value first
        else if let Ok(Some(iter)) = value.call_method("iter", Vec::new()) {
            Ok(iter)
        }
        // Default fallback to Iter on the object
        else {
            Ok(RefValue::from(MethodIter::new(value)))
        }
    });

    tokay_method!("iter_next : @iter", {
        let mut iter = iter.borrow_mut();

        if let Some(iter) = iter.object_mut::<Iter>() {
            Ok(RefValue::from(iter.next().unwrap_or_else(|| value!(void))))
        } else {
            Err(Error::from(format!(
                "{} only accepts '{}' as parameter, not '{}'",
                __function,
                "iter",
                iter.name()
            )))
        }
    });

    tokay_method!("iter_len : @iter", {
        let mut iter = iter.borrow_mut();

        Ok(RefValue::from(
            if let Some(iter) = iter.object_mut::<Iter>() {
                iter.count()
            } else {
                1
            },
        ))
    });

    tokay_method!("iter_rev : @iter", {
        {
            let mut iter = iter.borrow_mut();

            if let Some(iter) = iter.object_mut::<Iter>() {
                iter.iter.borrow_mut().rev()?;
            } else {
                return Err(Error::from(format!(
                    "{} only accepts '{}' as parameter, not '{}'",
                    __function,
                    "iter",
                    iter.name()
                )));
            }
        }

        Ok(iter)
    });
}

impl Iterator for Iter {
    type Item = RefValue;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.borrow_mut().next()
    }
}

impl Object for Iter {
    fn name(&self) -> &'static str {
        "iter"
    }

    fn repr(&self) -> String {
        self.iter.borrow().repr()
    }
}

impl std::fmt::Debug for Iter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.repr())
    }
}

impl PartialEq for Iter {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl PartialOrd for Iter {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id().partial_cmp(&other.id())
    }
}

impl From<Iter> for RefValue {
    fn from(iter: Iter) -> Self {
        Value::Object(Box::new(iter)).into()
    }
}