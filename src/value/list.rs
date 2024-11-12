//! List object
use super::{BoxedObject, Iter, Object, RefValue};
use tokay_macros::tokay_method;
extern crate self as tokay;

/// Alias for the inner list definition
type InnerList = Vec<RefValue>;

/// List object type
#[derive(Debug, Clone, PartialEq, PartialOrd)]
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

    tokay_method!("list : @*args", {
        let list = if args.len() == 1 {
            // In case of an iter, use the collect-method
            if args[0].is("iter") {
                return Ok(args[0]
                    .call_method("collect", context, Vec::new())?
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

    tokay_method!("list_len : @list", {
        let list = list.borrow();

        Ok(RefValue::from(if let Some(list) = list.object::<List>() {
            list.len()
        } else {
            1
        }))
    });

    tokay_method!("list_get_item : @list, item, default=void", {
        // In case list is not a list, make it a list.
        if !list.is("list") {
            list = Self::list(vec![list], None)?;
        }

        let list = list.borrow();

        if let Ok(item) = item.to_usize() {
            if let Some(value) = list.object::<List>().unwrap().get(item) {
                return Ok(value.clone());
            }
        }

        Ok(default)
    });

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

        // Extend in-place when possible.
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
            list = Self::list(vec![list], None)?;
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

    tokay_method!("list_push : @list, item, index=void", {
        // Don't push void
        if item.is_void() {
            return Ok(list);
        }

        // In case list is not a list, make it a list.
        if !list.is("list") {
            list = Self::list(vec![list], None)?;
        }

        // list_push returns the list itself, therefore this block.
        {
            let mut list = list.borrow_mut();
            let list = list.object_mut::<List>().unwrap();

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

        Ok(list)
    });

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

            return Err(format!(
                "{} provided index {} out of range",
                __function,
                index.unwrap()
            )
            .into());
        }

        let mut list = list.borrow_mut();
        let list = list.object_mut::<List>().unwrap();

        // Either pop or remove, regarding index setting.
        match index {
            None => match list.pop() {
                Some(item) => Ok(item),
                None => {
                    return Err(format!("{} can't pop off empty list", __function).into());
                }
            },
            Some(index) => {
                let len = list.len();

                if index >= len {
                    return Err(format!(
                        "{} provided index {} out of range of list sized {}",
                        __function, index, len
                    )
                    .into());
                }

                Ok(list.remove(index))
            }
        }
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
