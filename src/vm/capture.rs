use crate::reader::{Range, Reader};
use crate::value::{Object, RefValue, Str};

/** Captures are stack items where the VM operates on.

A capture can either be just empty, a range from the input or a full qualified value (RefValue).
In case the capture is a range, it can be turned into a string value on demand and in-place.
*/
#[derive(Clone)]
pub enum Capture {
    Empty,                                 // Empty capture
    Range(Range, Option<RefValue>, u8),    // Captured range
    Value(RefValue, Option<RefValue>, u8), // Captured value
}

impl Capture {
    // Checks if the capture matches a given alias string
    pub fn alias(&self, name: &str) -> bool {
        match self {
            Self::Range(_, alias, _) | Self::Value(_, alias, _) => {
                if let Some(alias) = alias.as_ref() {
                    let alias = alias.borrow();
                    if let Some(alias) = alias.object::<Str>() {
                        return alias.as_str() == name;
                    }
                }
            }
            _ => {}
        }

        false
    }

    /** Extracts a value from a capture.

    In case the capture is a range, the range is extracted as a string from the reader. */
    pub fn extract(&mut self, reader: &Reader) -> RefValue {
        match self {
            Capture::Empty => crate::value!(void),
            Capture::Range(range, alias, severity) => {
                let value = RefValue::from(reader.get(range));
                *self = Capture::Value(value.clone(), alias.clone(), *severity);
                value
            }
            Capture::Value(value, ..) => value.clone(),
        }
    }

    pub fn get_value(&self) -> RefValue {
        match self {
            Capture::Empty => crate::value!(void),
            Capture::Range(..) => {
                panic!("Cannot retrieve value of Capture::Range, use self.extract() first!")
            }
            Capture::Value(value, ..) => value.clone(),
        }
    }

    pub fn get_severity(&self) -> u8 {
        match self {
            Capture::Range(_, _, severity) | Capture::Value(_, _, severity) => *severity,
            _ => 0,
        }
    }

    pub fn set_severity(&mut self, new_severity: u8) {
        match self {
            Capture::Range(_, _, severity) | Capture::Value(_, _, severity) => {
                *severity = new_severity
            }
            _ => {}
        }
    }
}

impl From<RefValue> for Capture {
    fn from(value: RefValue) -> Self {
        Capture::Value(value, None, 10)
    }
}

impl std::fmt::Debug for Capture {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Empty => write!(f, "(empty)"),
            Self::Range(range, alias, severity) => {
                if let Some(alias) = alias {
                    write!(f, "{} => ", alias.repr())?;
                }

                range.fmt(f)?;
                write!(f, "({})", severity)
            }
            Self::Value(value, alias, severity) => {
                if let Some(alias) = alias {
                    write!(f, "{} => ", alias.repr())?;
                }

                if let Ok(value) = value.try_borrow() {
                    write!(f, "[{:x?}] {} ({})", value.id(), value.repr(), severity)
                } else {
                    write!(f, "<currently borrowed> ({})", severity)
                }
            }
        }
    }
}
