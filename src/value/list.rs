//! List object
use super::{BoxedObject, Object, RefValue};
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

        if self.len() == 1 {
            ret.push(',');
        }

        ret.push(')');
        ret
    }

    fn is_true(&self) -> bool {
        self.len() > 0
    }
}

impl List {
    pub fn new() -> Self {
        Self {
            list: InnerList::new(),
        }
    }

    tokay_method!("list(*args)", {
        let list = if args.len() == 1 {
            List::from(args[0].clone())
        } else {
            List { list: args }
        };

        Ok(RefValue::from(list))
    });

    tokay_method!("list_len(list)", {
        let list = list.borrow();

        Ok(RefValue::from(if let Some(list) = list.object::<List>() {
            list.len()
        } else {
            1
        }))
    });

    tokay_method!("list_iadd(list, append)", {
        // In case list is not a list, make it a list.
        if !list.is("list") {
            let item = RefValue::from(list.borrow().clone());
            list = Self::list(vec![item], None)?;
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

    tokay_method!("list_add(list, append)", {
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

    tokay_method!("list_push(list, item, index=void)", {
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
                let index = index.to_usize();
                let len = list.len();
                if index > len {
                    return Err(format!(
                        "{} index {} out of range in list sized {}",
                        __function, index, len
                    )
                    .into());
                }

                list.insert(index, item);
            }
        }

        Ok(list)
    });

    tokay_method!("list_pop(list, index=void)", {
        let index = match index.to_usize() {
            0 => None,
            i => Some(i),
        };

        if !list.is("list") {
            if index.is_none() {
                return Ok(list); // "pops" the list, which is not a list
            }

            return Err(format!("{} index {} out of range", __function, index.unwrap()).into());
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
                    return Err(format!("{} index {} out of range", __function, index).into());
                }

                Ok(list.remove(len - index - 1))
            }
        }
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
        if let Some(list) = refvalue.borrow().object::<List>() {
            (*list).clone()
        } else {
            Self {
                list: vec![refvalue.clone()],
            }
        }
    }
}

impl From<&RefValue> for List {
    fn from(refvalue: &RefValue) -> Self {
        List::from(refvalue.clone())
    }
}

/*
// fixme: This could be a replacement for value.list() but its usage is ugly.
impl<'list> From<&'list Value> for Option<&'list List> {
    fn from(value: &'list Value) -> Self {
        if let Value::List(list) = value {
            Some(&list)
        } else {
            None
        }
    }
}
*/

impl From<List> for RefValue {
    fn from(value: List) -> Self {
        RefValue::from(Box::new(value) as BoxedObject)
    }
}

#[test]
fn test_list_new() {
    assert_eq!(
        crate::run("list(true) list((1,2,3)) list(\"Tokay\")", ""),
        Ok(Some(crate::value!([[true], [1, 2, 3], ["Tokay"]])))
    )
}

#[test]
fn test_list_len() {
    assert_eq!(
        crate::run(
            "list().len() list(1).len() list(1, 2, 3).len() list_len(5)",
            ""
        ),
        Ok(Some(crate::value!([
            (0 as usize),
            (1 as usize),
            (3 as usize),
            (1 as usize)
        ])))
    )
}

#[test]
fn test_list_iadd() {
    assert_eq!(
        crate::run("l = (1,); l += 2; l += (3, 4); l", ""),
        Ok(Some(crate::value!([1, 2, 3, 4])))
    )
}

#[test]
fn test_list_add() {
    assert_eq!(
        crate::run("l = (1,); l + (2, 3) l", ""),
        Ok(Some(crate::value!([[1, 2, 3], [1]])))
    )
}

#[test]
fn test_list_push() {
    assert_eq!(
        crate::run("l = (1,); l.push(2); l.push((3, 4)); l", ""),
        Ok(Some(crate::value!([1, 2, [3, 4]])))
    );

    assert_eq!(
        crate::run("list_push((1,2,3), 99, 1)", ""),
        Ok(Some(crate::value!([1, 99, 2, 3])))
    );

    assert_eq!(
        crate::run("list_push((1,2,3), 99, 4)", ""),
        Err("Line 1, column 1: list_push() index 4 out of range in list sized 3".into())
    );
}

#[test]
fn test_list_pop() {
    assert_eq!(
        crate::run("l = (1,2,3,4); l.pop() l.pop(1)", ""),
        Ok(Some(crate::value!([4, 2])))
    );

    assert_eq!(
        crate::run("l = (1,2,3,4); l.pop(4)", ""),
        Err("Line 1, column 17: list_pop() index 4 out of range".into())
    );

    assert_eq!(crate::run("list_pop(1)", ""), Ok(Some(crate::value!(1))));

    assert_eq!(
        crate::run("list_pop(1, 1)", ""),
        Err("Line 1, column 1: list_pop() index 1 out of range".into())
    );
}

#[test]
fn test_list_repr() {
    /*
    Currently under consideration, see https://github.com/tokay-lang/tokay/issues/45
    assert_eq!(
        crate::run("repr((1, ))", ""),
        Ok(Some(crate::value!("(1, )")))
    );
    */

    assert_eq!(
        crate::run("l = (); l += 1; repr(l)", ""),
        Ok(Some(crate::value!("(1,)")))
    );

    assert_eq!(
        crate::run("repr((1, 2, 3, 4))", ""),
        Ok(Some(crate::value!("(1, 2, 3, 4)")))
    )
}
