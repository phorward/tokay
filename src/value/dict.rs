//! Dictionary object
use super::{BoxedObject, Object, RefValue};
use crate::value;
use crate::Error;
use indexmap::IndexMap;
use tokay_macros::tokay_method;
extern crate self as tokay;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;

// Alias for the inner dict
type InnerDict = IndexMap<String, RefValue>;

// Dict object type
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(transparent)]
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
        let mut ret = "(".to_string();

        for (key, value) in self.iter() {
            if ret.len() > 1 {
                ret.push_str(", ");
            }

            // todo: Put this into a utility function...
            if !key.chars().all(|ch| ch.is_alphabetic() || ch == '_') {
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

    tokay_method!("dict()", Ok(RefValue::from(Dict::new())));

    tokay_method!("dict_len(dict)", {
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

    tokay_method!("dict_merge(dict, other)", {
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

    tokay_method!("dict_push(dict, key, value)", {
        let dict = &mut *dict.borrow_mut();

        if let Some(dict) = dict.object_mut::<Dict>() {
            let key = key.to_string();

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

    tokay_method!("dict_pop(dict, key, default=void)", {
        let dict = &mut *dict.borrow_mut();

        if let Some(dict) = dict.object_mut::<Dict>() {
            let key = key.to_string();

            Ok(if let Some(value) = dict.remove(&key) {
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

    /*
    fn get_index(&self, index: &Value) -> Result<RefValue, String> {
        let index = index.to_string();
        if let Some(value) = self.get(&index) {
            Ok(value.clone())
        } else {
            Err(format!("Key '{}' not found", index))
        }
    }

    fn set_index(&mut self, index: &Value, value: RefValue) -> Result<(), String> {
        let index = index.to_string();
        self.insert(index, value);
        Ok(())
    }
    */
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

impl From<Dict> for RefValue {
    fn from(value: Dict) -> Self {
        RefValue::from(Box::new(value) as BoxedObject)
    }
}

#[test]
fn test_dict() {
    assert_eq!(
        crate::run("(b => 3, c => 1, a => 2)", ""),
        Ok(Some(crate::value!(["b" => 3, "c" => 1, "a" => 2])))
    );
}

#[test]
fn test_dict_compare() {
    assert_eq!(
        crate::run(
            r#"
                a = (a => 1, b => 2)
                b = (b => 2, a => 1)
                c = (a => 1, b => 2, c => 3)
                d = (c => 3, b => 2, a => 1)
                a < c   a < b   c < d   a == a   a != b   c >= a   b > a   c > a   c > b
            "#,
            ""
        ),
        Ok(Some(crate::value!([
            true, true, true, true, true, true, true, true, true
        ])))
    );
}

#[test]
fn test_dict_len() {
    assert_eq!(
        crate::run("dict().len() (a => 1, b => 2).len()", ""),
        Ok(Some(crate::value!([(0 as usize), (2 as usize)])))
    );

    assert_eq!(
        crate::run("dict_len(\"Donkey\")", ""),
        Err("Line 1, column 1: dict_len() only accepts 'dict' as parameter, not 'str'".to_string())
    );
}

#[test]
fn test_dict_merge() {
    assert_eq!(
        crate::run("d = (a => 1, b => 2); d.merge((c => 3)); d", ""),
        Ok(Some(crate::value!(["a" => 1, "b" => 2, "c" => 3])))
    );

    // issue #74
    assert_eq!(
        crate::run("d = (a => 1, b => 2); d.merge(d)", ""),
        Ok(Some(crate::value!(["a" => 1, "b" => 2])))
    );

    // issue #74
    assert_eq!(
        crate::run("(a => 1, b => 2) $1.merge($1)", ""),
        Ok(Some(
            crate::value!([["a" => 1, "b" => 2], ["a" => 1, "b" => 2]])
        ))
    );
}

#[test]
fn test_dict_push() {
    assert_eq!(
        crate::run("d = dict(); d.push(1, 2); d.push(2, 3); d", ""),
        Ok(Some(crate::value!(["1" => 2, "2" => 3])))
    );

    assert_eq!(
        crate::run("d = dict(); d.push(1, 2); d.push(2, 3); d.push(1, 4)", ""),
        Ok(Some(crate::value!(2)))
    );
}

#[test]
fn test_dict_pop() {
    assert_eq!(
        crate::run(
            r#"d = ("esel" => 1, "bert" => 2) d.pop("franz") d.pop("franz", "eugen") d.pop("bert") d"#,
            ""
        ),
        Ok(Some(crate::value!(["eugen", 2, ["esel" => 1]])))
    );
}
