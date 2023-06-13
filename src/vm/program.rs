use std::fs::File;
use std::io;

use super::*;
use crate::error::Error;
use crate::reader::Reader;
use crate::value::{Object, ParseletRef, RefValue};

/** Programs are containers holding statics and a pointer to the main parselet.

A program is the result of a successful compiler run. */
#[derive(Debug)]
pub struct Program {
    pub(crate) statics: Vec<RefValue>, // Static values referenced by this program
}

impl Program {
    pub fn new(statics: Vec<RefValue>) -> Self {
        //println!("Program with {} statics in total", statics.len());
        Self { statics }
    }

    /// Returns a reference to the program's main parselet.
    pub fn main(&self) -> ParseletRef {
        // Find main parselet by selecting the last parselet defined.
        // todo: allow to specify main parselet.
        for i in 0..self.statics.len() {
            if let Some(parselet) = self.statics[i].borrow().object::<ParseletRef>() {
                return parselet.clone();
            }
        }

        panic!("No main parselet found")
    }

    pub fn dump(&self) {
        for i in 0..self.statics.len() {
            println!("{} => {:#?}", i, self.statics[i]);
        }
    }

    pub fn run(&self, runtime: &mut Runtime) -> Result<Option<RefValue>, Error> {
        match self
            .main()
            .0
            .borrow()
            .run(self, runtime, runtime.stack.len(), None, true, 0)
        {
            Ok(Accept::Push(Capture::Value(value, ..))) => {
                if value.is_void() {
                    Ok(None)
                } else {
                    Ok(Some(value.clone()))
                }
            }
            Ok(_) | Err(Reject::Next) => Ok(None),
            Err(Reject::Error(error)) => Err(*error),
            Err(other) => Err(Error::new(None, format!("Runtime error {:?}", other))),
        }
    }

    pub fn run_from_reader(&self, reader: Reader) -> Result<Option<RefValue>, Error> {
        let mut runtime = Runtime::new(reader);
        self.run(&mut runtime)
    }

    pub fn run_from_str(&self, src: &'static str) -> Result<Option<RefValue>, Error> {
        self.run_from_reader(Reader::new(Box::new(std::io::Cursor::new(src))))
    }

    pub fn run_from_string(&self, src: String) -> Result<Option<RefValue>, Error> {
        self.run_from_reader(Reader::new(Box::new(std::io::Cursor::new(src))))
    }

    pub fn run_from_file(&self, filename: &str) -> Result<Option<RefValue>, Error> {
        if filename == "-" {
            self.run_from_reader(Reader::new(Box::new(io::stdin())))
        } else if let Ok(file) = File::open(filename) {
            self.run_from_reader(Reader::new(Box::new(file)))
        } else {
            Err(Error::new(
                None,
                format!("Unable to read from filename '{}'", filename),
            ))
        }
    }
}
