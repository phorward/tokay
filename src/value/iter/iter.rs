//! Abstraction of types implementing RefValueIter into an `iter` object.
use crate::value::{MethodIter, Object, RefValue, Value};
use crate::{Context, Error};
use tokay_macros::tokay_method;
extern crate self as tokay;

// BoxedRefValueIter type
type BoxedRefValueIter = Box<dyn RefValueIter>;

/// CloneBoxedRefValueIter is used internally to allow for dyn RefValueIter + Clone
pub trait CloneBoxedRefValueIter {
    fn dyn_clone(&self) -> BoxedRefValueIter;
}

impl<T> CloneBoxedRefValueIter for T
where
    T: 'static + RefValueIter + Clone,
{
    fn dyn_clone(&self) -> BoxedRefValueIter {
        Box::new(self.clone())
    }
}

impl Clone for BoxedRefValueIter {
    fn clone(&self) -> Self {
        self.dyn_clone()
    }
}

/// RefValueIter is a trait for iterators generating RefValues, which can optionally be reversed.
pub trait RefValueIter: CloneBoxedRefValueIter {
    fn next(&mut self, context: Option<&mut Context>) -> Option<RefValue>;
    fn repr(&self) -> String;
    fn rev(&mut self) -> Result<(), Error> {
        Err(Error::from("This iterator cannot be reversed."))
    }
}

/// Iter implementing Object to be used as RefValue
#[derive(Clone)]
pub struct Iter {
    pub iter: BoxedRefValueIter,
}

impl Iter {
    pub fn new(iter: BoxedRefValueIter) -> Self {
        return Self { iter };
    }

    tokay_method!("iter : @value", {
        // If parameter is already an iterator, just return it
        if value.is("iter") {
            Ok(value)
        }
        // Check for an available iter() method on the provided value first
        else if let Ok(Some(iter)) = value.call_method("iter", context, Vec::new(), None) {
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
            Ok(RefValue::from(
                iter.iter
                    .next(context)
                    .unwrap_or_else(|| tokay::value!(void)),
            ))
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

    tokay_method!("iter_max : @iter", {
        let mut borrowed_iter = iter.borrow_mut();

        if let Some(iter) = borrowed_iter.object_mut::<Iter>() {
            Ok(iter.max().unwrap_or(RefValue::from(Value::Void)))
        } else {
            Ok(iter.clone())
        }
    });

    tokay_method!("iter_min : @iter", {
        let mut borrowed_iter = iter.borrow_mut();

        if let Some(iter) = borrowed_iter.object_mut::<Iter>() {
            Ok(iter.min().unwrap_or(RefValue::from(Value::Void)))
        } else {
            Ok(iter.clone())
        }
    });

    tokay_method!("iter_rev : @iter", {
        {
            let mut iter = iter.borrow_mut();

            if let Some(iter) = iter.object_mut::<Iter>() {
                iter.iter.rev()?;
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

    tokay_method!("iter_collect : @iter", {
        let mut iter = iter.borrow_mut();

        if let Some(iter) = iter.object_mut::<Iter>() {
            let mut context = context;
            let mut list = Vec::new();

            while let Some(item) = iter.iter.next(context.as_deref_mut()) {
                list.push(item);
            }

            Ok(RefValue::from(list))
        } else {
            return Err(Error::from(format!(
                "{} only accepts '{}' as parameter, not '{}'",
                __function,
                "iter",
                iter.name()
            )));
        }
    });
}

impl Iterator for Iter {
    type Item = RefValue;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next(None)
    }
}

impl Object for Iter {
    fn name(&self) -> &'static str {
        "iter"
    }

    fn repr(&self) -> String {
        self.iter.repr()
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
