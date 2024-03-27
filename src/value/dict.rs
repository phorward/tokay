//! Dictionary object
use super::{BoxedObject, MethodIter, Object, RefValue, Str, Value};
use crate::value;
use crate::Error;
use indexmap::IndexMap;
use tokay_macros::tokay_method;
extern crate self as tokay;
use num::ToPrimitive;
use std::cmp::Ordering;

// Alias for the inner dict
type InnerDict = IndexMap<RefValue, RefValue>;

// Dict object type
#[derive(Debug, Clone)]
pub struct Dict {
    dict: InnerDict,
}

impl Object for Dict {
    fn severity(&self) -> u8 {
        20
    }

    fn name(&self) -> &'static str {
        "dict"
    }

    fn repr(&self) -> String {
        if self.is_empty() {
            return "()".to_string();
        }

        let mut ret = "(".to_string();

        for (key, value) in self.iter() {
            let key = key.borrow();

            if ret.len() > 1 {
                ret.push_str(" ");
            }

            if let Some(key) = key.object::<Str>() {
                // check if identifier is allowed, otherwise put in "quotation marks"
                if !key.chars().all(|ch| ch.is_alphabetic() || ch == '_')
                    || crate::compiler::RESERVED_KEYWORDS.contains(&key.as_str())
                    || crate::compiler::RESERVED_TOKENS.contains(&key.as_str())
                {
                    ret.push('"');
                    for ch in key.chars() {
                        match ch {
                            '\\' => ret.push_str("\\\\"),
                            '\n' => ret.push_str("\\n"),
                            '\r' => ret.push_str("\\r"),
                            '\t' => ret.push_str("\\t"),
                            '"' => ret.push_str("\\\""),
                            _ => ret.push(ch),
                        }
                    }
                    ret.push('"');
                } else {
                    ret.push_str(key);
                }
            } else {
                ret.push_str(&key.repr());
            }

            ret.push_str(" => ");
            ret.push_str(&value.borrow().repr());
        }

        ret.push(')');
        ret
    }

    fn is_true(&self) -> bool {
        self.len() > 0
    }

    fn is_mutable(&self) -> bool {
        true
    }
}

impl Dict {
    pub fn new() -> Self {
        Self {
            dict: InnerDict::new(),
        }
    }

    pub fn insert_str(&mut self, key: &str, value: RefValue) -> Option<RefValue> {
        self.insert(RefValue::from(key), value)
    }

    pub fn get_str(&self, key: &str) -> Option<&RefValue> {
        self.get(&RefValue::from(key)) // fixme: improve lookup!
    }

    pub fn remove_str(&mut self, key: &str) -> Option<RefValue> {
        self.shift_remove(&RefValue::from(key)) // fixme: improve lookup!
    }

    tokay_method!("dict : @", Ok(RefValue::from(Dict::new())));

    tokay_method!("dict_len : @dict", {
        let dict = dict.borrow();

        if let Some(dict) = dict.object::<Dict>() {
            Ok(RefValue::from(dict.len()))
        } else {
            Err(Error::from(format!(
                "{} only accepts '{}' as parameter, not '{}'",
                __function,
                "dict",
                dict.name()
            )))
        }
    });

    tokay_method!("dict_clone : @dict", {
        let dict = dict.borrow();

        if let Some(dict) = dict.object::<Dict>() {
            Ok(RefValue::from(dict.clone()))
        } else {
            Err(Error::from(format!(
                "{} only accepts '{}' as parameter, not '{}'",
                __function,
                "dict",
                dict.name()
            )))
        }
    });

    // Method to retrieve or iterate the keys of a dict.
    tokay_method!("dict_keys : @dict, index=void", {
        // If index is void, create an iterator on keys.
        if index.is_void() {
            return Ok(RefValue::from(MethodIter::new_method_iter(
                dict.clone(),
                "keys",
                None,
                "iinc",
            )));
        }

        // Otherwise, borrow
        let dict = dict.borrow();
        if let Some(dict) = dict.object::<Dict>() {
            if let Some((key, _)) = dict.get_index(index.to_usize()?) {
                Ok(key.clone())
            } else {
                Ok(value!(void))
            }
        } else {
            Err(Error::from(format!(
                "{} only accepts '{}' as parameter, not '{}'",
                __function,
                "dict",
                dict.name()
            )))
        }
    });

    // Method to retrieve or iterate a list of [key, value] from a dict by index
    tokay_method!("dict_items : @dict, index=void", {
        // If index is void, create an iterator on items.
        if index.is_void() {
            return Ok(RefValue::from(MethodIter::new_method_iter(
                dict.clone(),
                "items",
                None,
                "iinc",
            )));
        }

        // Otherwise, borrow
        let dict = dict.borrow();
        if let Some(dict) = dict.object::<Dict>() {
            if let Some((key, value)) = dict.get_index(index.to_usize()?) {
                Ok(value!([(key.clone()), (value.clone())]))
            } else {
                Ok(value!(void))
            }
        } else {
            Err(Error::from(format!(
                "{} only accepts '{}' as parameter, not '{}'",
                __function,
                "dict",
                dict.name()
            )))
        }
    });

    tokay_method!("dict_get_item : @dict, item, default=void", {
        if !item.is_hashable() {
            return Err(Error::from(format!(
                "{} unhashable type '{}'",
                __function,
                item.name()
            )));
        }

        // todo: alias dict_get
        let dict = dict.borrow();

        if let Some(dict) = dict.object::<Dict>() {
            if let Some(item) = dict.get(&item) {
                Ok(item.clone())
            } else {
                // In case index is an int that can be turned into an usize,
                // try to obtain the dict item by its index
                if let Value::Int(index) = &*item.borrow() {
                    if let Some(index) = index.to_usize() {
                        if let Some((_, item)) = dict.get_index(index) {
                            return Ok(item.clone());
                        }
                    }
                }

                Ok(default)
            }
        } else {
            Err(Error::from(format!(
                "{} only accepts '{}' as parameter, not '{}'",
                __function,
                "dict",
                dict.name()
            )))
        }
    });

    tokay_method!("dict_set_item : @dict, item, value=void", {
        if !item.is_hashable() {
            return Err(Error::from(format!(
                "{} unhashable type '{}'",
                __function,
                item.name()
            )));
        }

        let mut dict = dict.borrow_mut();

        if let Some(dict) = dict.object_mut::<Dict>() {
            if value.is_void() {
                dict.shift_remove(&item);
                Ok(value![void])
            } else {
                dict.insert(item, value.clone());
                Ok(value)
            }
        } else {
            Err(Error::from(format!(
                "{} only accepts '{}' as parameter, not '{}'",
                __function,
                "dict",
                dict.name()
            )))
        }
    });

    tokay_method!("dict_merge : @dict, other", {
        {
            let dict = &mut *dict.borrow_mut();

            if let Ok(other) = other.try_borrow() {
                if let Some(dict) = dict.object_mut::<Dict>() {
                    if let Some(other) = other.object::<Dict>() {
                        for (k, v) in other.iter() {
                            dict.insert(k.clone(), v.clone());
                        }
                    } else {
                        return Err(Error::from(format!(
                            "{} only accepts '{}' as second parameter, not '{}'",
                            __function,
                            dict.name(),
                            other.name()
                        )));
                    }
                } else {
                    return Err(Error::from(format!(
                        "{} only accepts '{}' as first parameter, not '{}'",
                        __function,
                        "dict",
                        dict.name()
                    )));
                }
            }
        }

        Ok(dict)
    });

    tokay_method!("dict_push : @dict, key, value", {
        let dict = &mut *dict.borrow_mut();

        if let Some(dict) = dict.object_mut::<Dict>() {
            Ok(if let Some(old) = dict.insert(key, value) {
                old
            } else {
                value!(void)
            })
        } else {
            Err(Error::from(format!(
                "{} only accepts '{}' as parameter, not '{}'",
                __function,
                "dict",
                dict.name()
            )))
        }
    });

    tokay_method!("dict_pop : @dict, key=void, default=void", {
        let dict = &mut *dict.borrow_mut();

        if let Some(dict) = dict.object_mut::<Dict>() {
            if key.is_void() {
                return Ok(if let Some(last) = dict.pop() {
                    last.1
                } else {
                    default
                });
            }

            Ok(if let Some(value) = dict.shift_remove(&key) {
                value
            } else {
                default
            })
        } else {
            Err(Error::from(format!(
                "{} only accepts '{}' as parameter, not '{}'",
                __function,
                "dict",
                dict.name()
            )))
        }
    });
}

// Implement PartialOrd and PartialEq on our own,
// until https://github.com/bluss/indexmap/issues/153
// may become resolved.
impl PartialOrd for Dict {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.len() < other.len() {
            Some(Ordering::Less)
        } else if self.len() > other.len() {
            Some(Ordering::Greater)
        } else {
            for (a, b) in self.iter().zip(other.iter()) {
                if a.0 < b.0 || a.1 < b.1 {
                    return Some(Ordering::Less);
                } else if a.0 > b.0 || a.1 > b.1 {
                    return Some(Ordering::Greater);
                }
            }

            Some(Ordering::Equal)
        }
    }
}

impl PartialEq for Dict {
    fn eq(&self, other: &Self) -> bool {
        if self.id() == other.id() {
            return true;
        }

        if let Some(Ordering::Equal) = self.partial_cmp(other) {
            true
        } else {
            false
        }
    }
}

impl std::ops::Deref for Dict {
    type Target = InnerDict;

    fn deref(&self) -> &Self::Target {
        &self.dict
    }
}

impl std::ops::DerefMut for Dict {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.dict
    }
}

impl std::ops::Index<&str> for Dict {
    type Output = RefValue;

    fn index(&self, key: &str) -> &RefValue {
        self.get_str(key).expect("Key not found")
    }
}

impl From<Dict> for RefValue {
    fn from(value: Dict) -> Self {
        RefValue::from(Box::new(value) as BoxedObject)
    }
}
