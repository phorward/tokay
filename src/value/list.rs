//! List object
use super::{BoxedObject, Iter, Object, RefValue};
use crate::value;
use tokay_macros::tokay_method;
extern crate self as tokay;

/// Alias for the inner list definition
type InnerList = Vec<RefValue>;

/// List object type
#[derive(Debug, Clone, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct List {
    list: InnerList,
}

impl Object for List {
    fn severity(&self) -> u8 {
        30
    }

    fn name(&self) -> &'static str {
        "list"
    }

    fn repr(&self) -> String {
        let mut ret = "(".to_string();

        for item in self.iter() {
            if ret.len() > 1 {
                ret.push_str(", ");
            }

            ret.push_str(&item.borrow().repr());
        }

        if self.len() <= 1 {
            ret.push_str(", )");
        } else {
            ret.push_str(")");
        }

        ret
    }

    fn is_true(&self) -> bool {
        self.len() > 0
    }

    fn is_mutable(&self) -> bool {
        true
    }
}

#[allow(unused_doc_comments)]
impl List {
    pub fn new() -> Self {
        Self {
            list: InnerList::new(),
        }
    }

    pub fn with_capacity(len: usize) -> Self {
        Self {
            list: InnerList::with_capacity(len),
        }
    }

    /// Constructs a new list from all specified `args`.
    tokay_method!("list : @*args", {
        let list = if args.len() == 1 {
            // In case of an iter, use the collect-method
            if args[0].is("iter") {
                return Ok(args[0]
                    .call_method("collect", context, Vec::new(), None)?
                    .unwrap());
            }
            // Otherwise, create a list with one item
            else {
                List::from(args[0].clone())
            }
        }
        // When multiple items are provided, turn these into list items
        else {
            List { list: args }
        };

        Ok(RefValue::from(list))
    });

    /// Clone `list` into a standalone copy.
    tokay_method!("list_clone : @list", {
        let borrowed = list.borrow();

        if let Some(list) = borrowed.object::<List>() {
            Ok(RefValue::from(list.clone()))
        } else {
            Ok(RefValue::from(List {
                list: vec![list.clone()],
            }))
        }
    });

    /// Returns the length of the specified `list`.
    tokay_method!("list_len : @list", {
        let list = list.borrow();

        Ok(RefValue::from(if let Some(list) = list.object::<List>() {
            list.len()
        } else if list.is_void() {
            0
        } else {
            1
        }))
    });

    /** Retrieves item with `index` from `list`.

    When `upsert=true`, it fills the list to the specified `index` and inserts and returns the `default` value.
    A `default`-value of `void` will become `null` in upsert-mode.

    Otherwise, `default` is just returned when the specified `item` is not present.

    This method is also invoked when using the `dict` item syntax.
    */
    tokay_method!(
        "list_get_item : @list, index, default=void, upsert=false",
        {
            // In case list is not a list, make it a list.
            if !list.is("list") {
                list = Self::list(vec![list], None)?;
            }

            {
                let list = list.borrow();
                let index = index.to_usize()?;

                if let Some(value) = list.object::<List>().unwrap().get(index) {
                    return Ok(value.clone());
                }
            }

            if upsert.is_true() {
                // follow the void paradigm; void cannot be upserted, so default to null.
                if default.is_void() {
                    default = value![null];
                }

                return Self::list_set_item(vec![list, index, default], None);
            }

            Ok(default)
        }
    );

    tokay_method!("list_set_item : @list, item, value=void", {
        // In case list is not a list, make it a list.
        if !list.is("list") {
            list = Self::list(vec![list], None)?;
        }

        let mut list = list.borrow_mut();
        let list = list.object_mut::<List>().unwrap();

        let item = item.to_usize()?;
        let len = list.len();

        if item >= len {
            list.resize_with(item + 1, Default::default)
        }

        if value.is_void() {
            value = list.remove(item);
        } else {
            list[item] = value.clone();
        }

        Ok(value)
    });

    tokay_method!("list_flatten : @list", {
        if let Some(list) = list.borrow().object::<List>() {
            let mut ret = List::with_capacity(list.len());

            for item in list.iter() {
                if let Some(list) = item.borrow().object::<List>() {
                    // TODO: flatten list recursively until a parametrizable depth.
                    for item in list.iter() {
                        ret.push(item.clone());
                    }
                } else {
                    ret.push(item.clone());
                }
            }

            return Ok(RefValue::from(ret));
        }

        Ok(RefValue::from(crate::value!([list])))
    });

    tokay_method!("list_iadd : @list, append", {
        // Don't append void
        if append.is_void() {
            return Ok(list);
        }

        // In case list is not a list, make it a list.
        if !list.is("list") {
            let item = RefValue::from(list.borrow().clone());
            if item.is_void() {
                *(list.borrow_mut()) = Self::list(vec![], None)?.into();
            } else {
                *(list.borrow_mut()) = Self::list(vec![item], None)?.into();
            }
        }

        // Append or extend in-place when possible.
        if let (Ok(mut inner), Ok(to_append)) = (list.try_borrow_mut(), append.try_borrow()) {
            let inner = inner.object_mut::<List>().unwrap();

            // When append is a list, append all items to list
            if let Some(append) = to_append.object::<List>() {
                inner.reserve(append.len());

                for item in append.iter() {
                    inner.push(item.clone());
                }
            // Otherwise, just push append to the list.
            } else {
                inner.push(append.clone());
            }

            return Ok(list.clone());
        }

        // Otherwise, perform ordinary add first, then re-assign to list
        let new = Self::list_add(vec![list.clone(), append.clone()], None)?;
        *list.borrow_mut() = new.into();

        Ok(list)
    });

    tokay_method!("list_add : @list, append", {
        // Don't append void
        if append.is_void() {
            return Ok(list);
        }

        // In case list is not a list, make it a list.
        if !list.is("list") {
            list = RefValue::from(List::from(list));
        }

        let mut list = list.borrow().object::<List>().unwrap().clone();

        // When append is a list, append all items to list
        if let Some(append) = append.borrow().object::<List>() {
            list.reserve(append.len());

            for item in append.iter() {
                list.push(item.clone());
            }
        // Otherwise, just push append to the list.
        } else {
            list.push(append.clone());
        }

        Ok(RefValue::from(list))
    });

    /** Explicitly pushes `item` to `list`.

    When `index` is provided, the value is inserted at the given offset,
    otherwise it is appended (pushed) to the list's end. */
    tokay_method!("list_push : @list, item, index=void", {
        // Don't push void
        if item.is_void() {
            return Ok(value![void]);
        }

        let mut list = list.borrow_mut();

        // If first parameter is not a list, just do nothing!
        if let Some(list) = list.object_mut::<List>() {
            if index.is_void() {
                list.push(item);
            } else {
                let index = index.to_usize()?;
                let len = list.len();

                if index > len {
                    return Err(format!(
                        "{} provided index {} out of range in list sized {}",
                        __function, index, len
                    )
                    .into());
                }

                list.insert(index, item);
            }
        }

        Ok(value![void])
    });

    /** Explicitly extends `extend` to `list`.

    When `index` is provided, the list behind extend is inserted at the given offset,
    otherwise it is extended to the list's end. */
    tokay_method!("list_extend : @list, extend, index=void", {
        // Don't extend void
        if extend.is_void() {
            return Ok(value![void]);
        }

        // In case extend is not a list, make it a list.
        if !extend.is("list") {
            extend = RefValue::from(List::from(extend));
        }

        let mut list = list.borrow_mut();

        // If first parameter is not a list, just do nothing!
        if let Some(list) = list.object_mut::<List>() {
            let extend = extend.borrow();
            let extend = extend.object::<List>().unwrap();

            list.reserve(extend.len());

            if index.is_void() {
                for item in extend.iter() {
                    if !item.is_void() {
                        list.push(item.clone());
                    }
                }
            } else {
                let mut index = index.to_usize()?;
                let len = list.len();

                if index > len {
                    return Err(format!(
                        "{} provided index {} out of range in list sized {}",
                        __function, index, len
                    )
                    .into());
                }

                for item in extend.iter() {
                    if !item.is_void() {
                        list.insert(index, item.clone());
                        index += 1;
                    }
                }
            }
        }

        Ok(value![void])
    });

    /** Pops item off a list. */
    tokay_method!("list_pop : @list, index=void", {
        let index = if index.is_void() {
            None
        } else {
            Some(index.to_usize()?)
        };

        if !list.is("list") {
            if index.is_none() {
                return Ok(list); // "pops" the list, which is not a list
            }

            return Ok(value![void]);
        }

        let mut list = list.borrow_mut();
        let list = list.object_mut::<List>().unwrap();

        // Either pop or remove, regarding index setting.
        Ok(match index {
            None => match list.pop() {
                Some(item) => item,
                None => value![void],
            },
            Some(index) => {
                let len = list.len();

                if index < len {
                    list.remove(index)
                } else {
                    value![void]
                }
            }
        })
    });

    tokay_method!("list_sort : @list", {
        if !list.is("list") {
            return Ok(Self::list(vec![list], None)?);
        }

        {
            let mut list = list.borrow_mut();
            let list = list.object_mut::<List>().unwrap();
            list.sort();
        }

        Ok(list)
    });

    /** Find `item` in `list` and return its offset.

    In case `item` is not in the list, -1 is returned.
    */
    tokay_method!("list_index : @list, item", {
        let list = list.borrow();

        if let Some(list) = list.object::<List>() {
            if let Some(index) = list
                .list
                .iter()
                .position(|val| *val.borrow() == *item.borrow())
            {
                return Ok(value![index]);
            }
        }

        Ok(value![-1])
    });
}

impl std::ops::Deref for List {
    type Target = InnerList;

    fn deref(&self) -> &Self::Target {
        &self.list
    }
}

impl std::ops::DerefMut for List {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.list
    }
}

impl std::iter::IntoIterator for List {
    type Item = RefValue;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.list.into_iter()
    }
}

impl From<RefValue> for List {
    fn from(refvalue: RefValue) -> Self {
        match refvalue.name() {
            "iter" => {
                let mut iter = refvalue.borrow_mut();
                let iter = iter.object_mut::<Iter>().unwrap();
                let mut list = InnerList::new();

                for item in iter {
                    list.push(item);
                }

                Self { list }
            }
            "list" => {
                let list = refvalue.borrow();
                let list = list.object::<List>().unwrap();
                (*list).clone()
            }
            "void" => Self { list: Vec::new() },
            _ => Self {
                list: vec![refvalue.clone()],
            },
        }
    }
}

impl From<&RefValue> for List {
    fn from(refvalue: &RefValue) -> Self {
        List::from(refvalue.clone())
    }
}

impl From<List> for RefValue {
    fn from(value: List) -> Self {
        RefValue::from(Box::new(value) as BoxedObject)
    }
}

impl From<InnerList> for RefValue {
    fn from(list: InnerList) -> Self {
        RefValue::from(List { list })
    }
}
