/*!
An iterator, running on a specific object using an index, yielding items that are
returned by a specific method call with the index. The index is calculated with a
given unary operation (by default, iinc and idec)
*/
use crate::value::{Iter, Object, RefValue, RefValueIter};
use crate::{Context, Error};
extern crate self as tokay;

#[derive(Clone)]
pub struct MethodIter {
    pub object: RefValue,            // Object to iterate over
    pub object_method: &'static str, // Object method to call each iteration
    pub index: Option<RefValue>,     // Current iteration index
    pub index_op: &'static str,      // Operation on index to increase or decrease
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
        Iter::new(Box::new(Self {
            object: object.clone(),
            object_method,
            index: index.or_else(|| Some(tokay::value!(0))),
            index_op,
        }))
    }
}

impl RefValueIter for MethodIter {
    fn next(&mut self, context: Option<&mut Context>) -> Option<RefValue> {
        if let Some(index) = &self.index {
            match self
                .object
                .call_method(self.object_method, context, vec![index.clone()], None)
            {
                Ok(Some(next)) => {
                    // When next is not void, calculate index and return next
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
        if let Some(index) = &self.index {
            format!(
                "<MethodIter on {}_{} at {}, using {}>",
                self.object.name(),
                self.object_method,
                index.repr(),
                self.index_op,
            )
        } else {
            format!(
                "<MethodIter on {}_{} finished, used {}>",
                self.object.name(),
                self.object_method,
                self.index_op,
            )
        }
    }

    fn rev(&mut self) -> Result<(), Error> {
        match self.index_op {
            "iinc" => {
                self.index_op = "idec";

                // fixme: this is a (bad) hack for str, which begins at 0 and counts down when reversed.
                if self.object.is("str") {
                    self.index = Some(tokay::value!(-1));
                    Ok(())
                } else {
                    match self
                        .object
                        .call_method("len", None, Vec::new(), None)
                        .unwrap_or_else(|_| Some(tokay::value!(1)))
                    {
                        Some(len) => {
                            self.index = Some(len.unary_op(self.index_op)?);
                            Ok(())
                        }
                        None => Err(Error::from("This iterator cannot be reversed.")),
                    }
                }
            }
            "idec" => {
                self.index_op = "iinc";
                self.index = Some(tokay::value!(0));
                Ok(())
            }
            _ => Err(Error::from("This iterator cannot be reversed.")),
        }
    }
}
