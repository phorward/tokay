//! Dictionary object
use super::{BoxedObject, MethodIter, Object, RefValue, Str};
use crate::Error;
use crate::value;
use indexmap::IndexMap;
use tokay_macros::tokay_method;
extern crate self as tokay;
use std::cmp::Ordering;

// Alias for the inner dict
type InnerDict = IndexMap<RefValue, RefValue>;

// Dict object type
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
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

#[allow(unused_doc_comments)]
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

    /** Creates a new `dict`.

    Any provided `nargs` become key-value-pairs in the newly created dict.

    This can also be shortcut by the `()` syntax.
    */
    tokay_method!(
        "dict : @**nargs",
        Ok(RefValue::from(if let Some(nargs) = nargs {
            nargs.clone()
        } else {
            Dict::new()
        }))
    );

    /** Creates an iterator over a `dict`.

    The iterator is a method-iterator calling `iter_values()`.
    */
    tokay_method!("dict_iter : @dict", {
        // If index is void, create an iterator on keys.
        if dict.is("dict") {
            Ok(RefValue::from(MethodIter::new_method_iter(
                dict.clone(),
                "values",
                None,
                "iinc",
            )))
        } else {
            Err(Error::from(format!(
                "{} only accepts '{}' as parameter, not '{}'",
                __function,
                "dict",
                dict.name()
            )))
        }
    });

    /// Returns the number of items in the `dict`.
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

    /// Clone `dict` into a standalone copy.
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

    /** Retrieve or iterate the keys of a `dict`.

    When no `index` is given, the method returns an iterator over the keys.
    Otherwise, the key at the provided `index` is returned, or `default` in
    case the `index` is out of bounds.
    */
    tokay_method!("dict_keys : @dict, index=void, default=void", {
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

    /** Retrieve or iterate the values of a `dict`.

    When no `index` is given, the method returns an iterator over the values.
    Otherwise, the value at the provided `index` is returned, or `default` in
    case the `index` is out of bounds.
    */
    tokay_method!("dict_values : @dict, index=void, default=void", {
        // If index is void, create an iterator on keys.
        if index.is_void() {
            return Ok(RefValue::from(MethodIter::new_method_iter(
                dict.clone(),
                "values",
                None,
                "iinc",
            )));
        }

        // Otherwise, borrow
        let dict = dict.borrow();
        if let Some(dict) = dict.object::<Dict>() {
            if let Some((_, value)) = dict.get_index(index.to_usize()?) {
                Ok(value.clone())
            } else {
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

    /** Retrieve or iterate both keys and values of a `dict`.

    The function returns a list of key-value for each result.

    When no `index` is given, the method returns an iterator over the key-value-pairs.
    Otherwise, the key-value-pair at the provided `index` is returned, or `default` in
    case the `index` is out of bounds.
    */
    tokay_method!("dict_items : @dict, index=void, default=void", {
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

    /** Retrieve item with `key` from `dict`.

    When `upsert=true`, it creates and returns a new item with the `default` value, if no value with `key` is present.
    A `default`-value of `void` will become `null` in upsert-mode.

    Otherwise, `default` is just returned when the specified `key` is not present.

    This method is also invoked when using the `dict` item syntax.
    */
    tokay_method!("dict_get_item : @dict, key, default=void, upsert=false", {
        if !dict.is("dict") {
            return Err(Error::from(format!(
                "{} only accepts '{}' as parameter, not '{}'",
                __function,
                "dict",
                dict.name()
            )));
        }

        if !key.is_hashable() {
            return Err(Error::from(format!(
                "{} unhashable type '{}'",
                __function,
                key.name()
            )));
        }

        if upsert.is_true() {
            let mut dict = dict.borrow_mut();
            let dict = dict.object_mut::<Dict>().unwrap();

            if let Some(value) = dict.get(&key) {
                Ok(value.clone())
            } else {
                // follow the void paradigm; void cannot be upserted, so default to null.
                if default.is_void() {
                    default = value![null];
                }

                dict.insert(key, default.clone());
                Ok(default)
            }
        } else {
            let dict = dict.borrow();
            let dict = dict.object::<Dict>().unwrap();

            if let Some(value) = dict.get(&key) {
                Ok(value.clone())
            } else {
                Ok(default)
            }
        }
    });

    /** Insert or replace `value` under the given `key` in `dict`.

    When `value` is provided as void, the key is removed.

    Returns the previous item's value if the key already existed in `dict`,
    otherwise void.

    This method is also invoked when assigning to a `dict` item.
    */
    tokay_method!("dict_set_item : @dict, key, value=void", {
        if !key.is_hashable() {
            return Err(Error::from(format!(
                "{} unhashable type '{}'",
                __function,
                key.name()
            )));
        }

        let mut dict = dict.borrow_mut();

        if let Some(dict) = dict.object_mut::<Dict>() {
            if value.is_void() {
                dict.shift_remove(&key);
                Ok(value![void])
            } else {
                dict.insert(key, value.clone());
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

    /** Merges dict `other` into `dict`. */
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

    /** Returns and removes `key` from `dict`.

    When the given `key` does not exist, `default` will be returned, */
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
