//! String object
use super::{BoxedObject, List, Object, RefValue};
use crate::value;
use num::Zero;
use num_bigint::BigInt;
use num_parse::*;
use serde::{Deserialize, Serialize};
use tokay_macros::tokay_method;
extern crate self as tokay;

#[derive(Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Str {
    string: String,
}

impl Object for Str {
    fn severity(&self) -> u8 {
        10
    }

    fn name(&self) -> &'static str {
        "str"
    }

    fn repr(&self) -> String {
        let mut ret = String::with_capacity(self.string.len() + 2);
        ret.push('"');

        for ch in self.string.chars() {
            match ch {
                '\\' => ret.push_str("\\\\"),
                '\"' => ret.push_str("\\\""),
                '\n' => ret.push_str("\\n"),
                '\r' => ret.push_str("\\r"),
                '\t' => ret.push_str("\\t"),
                ch => ret.push(ch),
            }
        }

        ret.push('"');
        ret
    }

    fn is_true(&self) -> bool {
        self.len() > 0
    }

    fn to_i64(&self) -> Result<i64, String> {
        Ok(parse_int::<i64>(&self.string).unwrap_or(0))
    }

    fn to_f64(&self) -> Result<f64, String> {
        // todo: JavaScript-style parseFloat-like behavior?
        match self.string.parse::<f64>() {
            Ok(f) => Ok(f),
            Err(_) => Ok(0.0),
        }
    }

    fn to_usize(&self) -> Result<usize, String> {
        Ok(parse_uint::<usize>(&self.string).unwrap_or(0))
    }

    fn to_string(&self) -> String {
        self.string.clone()
    }

    fn to_bigint(&self) -> Result<BigInt, String> {
        Ok(parse_int::<BigInt>(&self.string).unwrap_or(BigInt::zero()))
    }
}

impl Str {
    /// Returns the &str slice of the Str object.
    pub fn as_str(&self) -> &str {
        &self.string
    }

    tokay_method!("str(value)", Ok(RefValue::from(value.to_string())));

    tokay_method!("str_len(s)", {
        let string = s.borrow();

        if let Some(string) = string.object::<Str>() {
            Ok(RefValue::from(string.chars().count()))
        } else {
            Ok(RefValue::from(string.to_string().chars().count()))
        }
    });

    tokay_method!("str_byteslen(s)", {
        let string = s.borrow();

        if let Some(string) = string.object::<Str>() {
            Ok(RefValue::from(string.len()))
        } else {
            Ok(RefValue::from(string.to_string().len()))
        }
    });

    tokay_method!("str_add(s, append)", {
        let mut string = s.to_string();

        if let Some(append) = append.borrow().object::<Str>() {
            string.push_str(append.as_str());
        } else {
            string.push_str(&append.to_string()); // todo: this might me done more memory saving
        }

        Ok(RefValue::from(string))
    });

    tokay_method!("str_endswith(s, postfix)", {
        let string = s.borrow();
        let postfix = postfix.borrow();

        Ok(if let Some(string) = string.object::<Str>() {
            let string = string.as_str();

            if let Some(postfix) = postfix.object::<Str>() {
                value!(string.ends_with(postfix.as_str()))
            } else {
                value!(string.ends_with(&postfix.to_string()))
            }
        } else {
            value!(string.to_string().ends_with(&postfix.to_string()))
        })
    });

    tokay_method!("str_mul(s, count)", {
        if let Some(string) = s.borrow().object::<Str>() {
            // string * count
            return Ok(RefValue::from(string.repeat(count.to_usize()?)));
        }

        // count * string is also possible
        Ok(RefValue::from(count.to_string().repeat(s.to_usize()?)))
    });

    tokay_method!("str_join(s, list)", {
        let delimiter = s.to_string();
        let list = List::from(list);

        let mut ret = String::new();

        for item in list.iter() {
            if ret.len() > 0 {
                ret.push_str(&delimiter);
            }

            ret.push_str(&item.to_string());
        }

        Ok(RefValue::from(ret))
    });

    tokay_method!("str_lower(s)", {
        Ok(RefValue::from(s.to_string().to_lowercase()))
    });

    tokay_method!("str_replace(s, from, to=void, n=void)", {
        let string = s.to_string();
        let from = from.to_string();
        let to = to.to_string();

        Ok(RefValue::from(if n.is_void() {
            string.replace(&from, &to)
        } else {
            string.replacen(&from, &to, n.to_usize()?)
        }))
    });

    tokay_method!("str_startswith(s, prefix)", {
        let string = s.borrow();
        let prefix = prefix.borrow();

        Ok(if let Some(string) = string.object::<Str>() {
            let string = string.as_str();

            if let Some(prefix) = prefix.object::<Str>() {
                value!(string.starts_with(prefix.as_str()))
            } else {
                value!(string.starts_with(&prefix.to_string()))
            }
        } else {
            value!(string.to_string().starts_with(&prefix.to_string()))
        })
    });

    tokay_method!("str_substr(s, start=0, length=void)", {
        if !s.is("str") {
            s = RefValue::from(s.to_string());
        }

        let string = s.borrow();
        let string = string.object::<Str>().unwrap().as_str();

        Ok(RefValue::from(if length.is_void() {
            string.chars().skip(start.to_usize()?).collect::<String>()
        } else {
            string
                .chars()
                .skip(start.to_usize()?)
                .take(length.to_usize()?)
                .collect::<String>()
        }))
    });

    tokay_method!("str_upper(s)", {
        Ok(RefValue::from(s.to_string().to_uppercase()))
    });
}

impl std::fmt::Debug for Str {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.string)
    }
}

impl std::fmt::Display for Str {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
}

impl std::ops::Deref for Str {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.string
    }
}

impl std::ops::DerefMut for Str {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.string
    }
}

impl From<String> for Str {
    fn from(string: String) -> Self {
        Str { string }
    }
}

impl From<&str> for Str {
    fn from(string: &str) -> Self {
        Str {
            string: string.to_string(),
        }
    }
}

impl From<&str> for RefValue {
    fn from(string: &str) -> Self {
        RefValue::from(string.to_string())
    }
}

impl From<String> for RefValue {
    fn from(string: String) -> Self {
        RefValue::from(Str { string })
    }
}

impl From<Str> for RefValue {
    fn from(string: Str) -> Self {
        RefValue::from(Box::new(string) as BoxedObject)
    }
}

/*
fn get_index(&self, index: &Value) -> Result<RefValue, String> {
    let index = index.to_usize();
    if let Some(ch) = self.chars().nth(index) {
        Ok(Value::Str(format!("{}", ch)).into())
    } else {
        Err(format!("Index {} beyond end of string", index))
    }
}

fn set_index(&mut self, index: &Value, value: RefValue) -> Result<(), String> {
    let index = index.to_usize();
    if index < self.len() {
        todo!();
        Ok(())
    } else {
        Err(format!("Index {} beyond end of string", index))
    }
}
*/

#[test]
fn test_str_len() {
    assert_eq!(
        crate::run("\"Hällo Wörld\".len() str_len(123.456)", ""),
        Ok(Some(crate::value!([(11 as usize), (7 as usize)])))
    )
}

#[test]
fn test_str_byteslen() {
    assert_eq!(
        crate::run("\"Hällo Wörld\".byteslen()", ""),
        Ok(Some(crate::value!(13 as usize)))
    )
}

#[test]
fn test_str_substr() {
    assert_eq!(
        crate::run(
            "s = \"Glasflügel\" s.substr(4) s.substr(4, 3) s.substr(length=4) s.substr(4, 10)",
            ""
        ),
        Ok(Some(crate::value!(["flügel", "flü", "Glas", "flügel"])))
    )
}

#[test]
// Tests for builtin string functions
fn test() {
    assert_eq!(
        crate::run(
            "
            \"abcäöü\".upper() \
            \"ABCÄÖÜ\".lower() \
            \"hello world\".replace(\"l\") \
            \"hello world\".replace(\"l\", n=2) \
            \"hello world\".replace(\"l\", \"x\") \
            \"hello world\".replace(\"l\", \"x\", 2) \
            \"hello world\".replace(\"l\").upper() \
            #\"Tokay\".upper()[1]  # index is not implemented for now \
            ",
            ""
        ),
        Ok(Some(value![[
            "ABCÄÖÜ",
            "abcäöü",
            "heo word",
            "heo world",
            "hexxo worxd",
            "hexxo world",
            "HEO WORD" //"O"
        ]]))
    );
}
