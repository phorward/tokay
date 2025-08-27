//! Token callables represented by Value::Token
use super::{BoxedObject, Dict, Object, RefValue};
use crate::vm::*;
use charclass::CharClass;
use num_bigint::BigInt;
use num_parse::*;
use tokay_macros::tokay_token;
extern crate self as tokay;

// todo: The entire Token enum could be split into separate objects.

#[derive(Debug, Clone, Hash, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Token {
    Empty,            // Matches the empty word
    EOF,              // Matches End of File
    Char(CharClass),  // Matches one character from a character class
    Chars(CharClass), // Matches multiple characters from a character class
    Match(String),    // Match a string
    Touch(String),    // Match a string with zero severity
}

impl Token {
    /// Retrieve builtin token
    pub fn builtin(ident: &str) -> Option<Token> {
        fn builtin_ccl(ident: &str) -> Option<Token> {
            let ccl = match ident {
                "Alphabetic" => Some(CharClass::new_with_predicate(|ch| ch.is_alphabetic())),
                "Alphanumeric" => Some(CharClass::new_with_predicate(|ch| ch.is_alphanumeric())),
                "Ascii" => Some(CharClass::new_with_predicate(char::is_ascii)),
                "AsciiAlphabetic" => Some(CharClass::new_with_predicate(char::is_ascii_alphabetic)),
                "AsciiAlphanumeric" => {
                    Some(CharClass::new_with_predicate(char::is_ascii_alphanumeric))
                }
                "AsciiControl" => Some(CharClass::new_with_predicate(char::is_ascii_control)),
                "AsciiDigit" => Some(CharClass::new_with_predicate(char::is_ascii_digit)),
                "AsciiGraphic" => Some(CharClass::new_with_predicate(char::is_ascii_graphic)),
                "AsciiHexdigit" => Some(CharClass::new_with_predicate(char::is_ascii_hexdigit)),
                "AsciiLowercase" => Some(CharClass::new_with_predicate(char::is_ascii_lowercase)),
                "AsciiPunctuation" => {
                    Some(CharClass::new_with_predicate(char::is_ascii_punctuation))
                }
                "AsciiUppercase" => Some(CharClass::new_with_predicate(char::is_ascii_uppercase)),
                "AsciiWhitespace" => Some(CharClass::new_with_predicate(char::is_ascii_whitespace)),
                "Control" => Some(CharClass::new_with_predicate(|c| c.is_control())),
                "Digit" => Some(CharClass::new_with_predicate(|c| c.is_digit(10))),
                "Lowercase" => Some(CharClass::new_with_predicate(|c| c.is_lowercase())),
                "Numeric" => Some(CharClass::new_with_predicate(|c| c.is_numeric())),
                "Uppercase" => Some(CharClass::new_with_predicate(|c| c.is_uppercase())),
                "Whitespace" => Some(CharClass::new_with_predicate(|c| c.is_whitespace())),
                _ => None,
            };

            if let Some(ccl) = ccl {
                Some(Token::Char(ccl))
            }
            // Any identifier attached with an "s" will be checked for Token+
            else if ident.len() > 1 && ident.ends_with("s") {
                match builtin_ccl(&ident[..ident.len() - 1]) {
                    Some(Token::Char(c)) => Some(Token::Chars(c)),
                    _ => None,
                }
            } else {
                None
            }
        }

        match ident {
            "Empty" => Some(Token::Empty),
            "EOF" => Some(Token::EOF),
            ident => builtin_ccl(ident),
        }
    }
}

impl Object for Token {
    fn name(&self) -> &'static str {
        "token"
    }

    fn repr(&self) -> String {
        match self {
            Token::Empty => "Empty".to_string(),
            Token::EOF => "EOF".to_string(),
            Token::Char(ccl) => format!("{:?}", ccl),
            Token::Chars(ccl) => format!("{:?}+", ccl),
            Token::Touch(s) => format!("'{}'", s),
            Token::Match(s) => format!("''{}''", s),
        }
    }

    fn is_callable(&self, without_arguments: bool) -> bool {
        without_arguments // Tokens don't support arguments
    }

    fn is_consuming(&self) -> bool {
        true // Tokens always consume!
    }

    fn is_nullable(&self) -> bool {
        match self {
            Token::Empty => true,
            Token::EOF => false,
            Token::Char(ccl) | Token::Chars(ccl) => ccl.len() == 0, //True shouldn't be possible here by definition!
            Token::Match(s) | Token::Touch(s) => s.len() == 0, //True shouldn't be possible here by definition!
        }
    }

    fn call(
        &self,
        context: Option<&mut Context>,
        args: Vec<RefValue>,
        nargs: Option<Dict>,
    ) -> Result<Accept, Reject> {
        assert!(context.is_some() && args.len() == 0 && nargs.is_none());

        let context = context.unwrap();
        let reader = &mut context.thread.reader;

        match self {
            Token::Empty => Ok(Accept::Next),
            Token::EOF => {
                if let Some(_) = reader.peek() {
                    Err(Reject::Next)
                } else {
                    Ok(Accept::Next)
                }
            }
            Token::Char(ccl) => {
                if let Some(ch) = reader.once(|ch| ccl.test(&(ch..=ch))) {
                    return Ok(Accept::Push(Capture::Range(
                        reader.capture_last(ch.len_utf8()),
                        None,
                        5,
                    )));
                }

                Err(Reject::Next)
            }
            Token::Chars(ccl) => {
                let start = reader.tell();

                while let Some(ch) = reader.peek() {
                    if !ccl.test(&(*ch..=*ch)) {
                        break;
                    }

                    reader.next();
                }

                let range = reader.capture_from(&start);

                if range.len() > 0 {
                    Ok(Accept::Push(Capture::Range(range, None, 5)))
                } else {
                    reader.reset(start);
                    Err(Reject::Next)
                }
            }
            Token::Match(string) | Token::Touch(string) => {
                let start = reader.tell();

                for ch in string.chars() {
                    if let Some(c) = reader.peek() {
                        if *c != ch {
                            break;
                        }
                    } else {
                        break;
                    }

                    reader.next();
                }

                let range = reader.capture_from(&start);

                if range.len() == string.len() {
                    Ok(Accept::Push(Capture::Range(
                        range,
                        None,
                        if matches!(self, Token::Touch(_)) {
                            0
                        } else {
                            5
                        },
                    )))
                } else {
                    reader.reset(start);
                    Err(Reject::Next)
                }
            }
        }
    }
}

impl From<Token> for RefValue {
    fn from(token: Token) -> Self {
        RefValue::from(Box::new(token) as BoxedObject)
    }
}

// Hard-coded Tokens are builtins, but they are consumable.

// Matching C-style identifiers
tokay_token!("Ident", {
    let reader = &mut context.thread.reader;
    let start = reader.tell();

    if reader.once(|ch| ch.is_alphabetic() || ch == '_').is_none() {
        return Err(Reject::Next);
    }

    reader.span(|ch| ch.is_alphanumeric() || ch == '_');

    Ok(Accept::Push(Capture::Range(
        reader.capture_from(&start),
        None,
        5,
    )))
});

// Matching 64-bit integers directly
tokay_token!("Int : @base=void, with_signs=true", {
    // Digits
    let base = if base.is_void() {
        None
    } else {
        let base = base.to_i64()?;
        if base < 1 || base > 36 {
            // maximum radix = 10 digits + 26 letters
            return Err(format!(
                "{} base value is {}, allowed is only between 1 and 36",
                __function, base
            )
            .into());
        }

        Some(base as u32)
    };

    if let Some(int) = if with_signs.is_true() {
        parse_int_from_iter_with_radix::<BigInt>(context.thread.reader, base, false)
    } else {
        parse_uint_from_iter_with_radix::<BigInt>(context.thread.reader, base, false)
    } {
        Ok(Accept::Push(Capture::Value(crate::value!(int), None, 5)))
    } else {
        Err(Reject::Next)
    }
});

// Matching 64-bit floats directly
tokay_token!("Float : @with_signs=true", {
    let reader = &mut context.thread.reader;
    let start = reader.tell();

    // Sign
    if with_signs.is_true() {
        reader.once(|ch: char| ch == '-' || ch == '+');
    }

    // Integer part
    let has_int = reader.span(|ch: char| ch.is_numeric()).is_some();

    // Decimal point
    if reader.once(|ch: char| ch == '.').is_none() {
        return Err(Reject::Next);
    }

    // Fractional part
    if reader.span(|ch: char| ch.is_numeric()).is_none() && !has_int {
        // Either integer or fractional part must be available!
        return Err(Reject::Next);
    }

    let mut range = reader.capture_from(&start);

    // Exponential notation
    if reader.once(|ch: char| ch == 'e' || ch == 'E').is_some() {
        reader.once(|ch: char| ch == '-' || ch == '+');

        if reader.span(|ch: char| ch.is_numeric()).is_some() {
            // Extend range when exponentional value could be read!
            range = reader.capture_from(&start)
        }
    }

    Ok(Accept::Push(Capture::Value(
        crate::value!(reader.get(&range).parse::<f64>().unwrap()),
        None,
        5,
    )))
});

// Words, optionally with limited length
tokay_token!("Word : @min=1 max=void", {
    let reader = &mut context.thread.reader;
    let start = reader.tell();

    if let Some(input) = reader.span(|ch| ch.is_alphabetic()) {
        if input.chars().count() < min.to_usize()? {
            return Err(Reject::Skip); // Accept input but skip the result
        }

        if !max.is_void() && input.chars().count() > max.to_usize()? {
            return Ok(Accept::Next);
        }

        Ok(Accept::Push(Capture::Range(
            reader.capture_from(&start),
            None,
            5,
        )))
    } else {
        Err(Reject::Next)
    }
});
