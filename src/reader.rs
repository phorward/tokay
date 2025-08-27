//! Universal low-level interface to let Tokay read input from different sources.
use num_parse::PeekableIterator;
use std::io::BufReader;
use std::io::prelude::*;

/// Position inside a reader, with row and column counting.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Offset {
    // todo: Hold source filename information as well in the future?
    pub offset: usize,
    pub row: u32,
    pub col: u32,
}

pub type Range = std::ops::Range<usize>;

// Abstraction of a buffered Reader with internal buffering, offset counting and clean-up.
pub struct Reader {
    pub filename: Option<String>, // Source filename
    reader: Box<dyn BufRead>,     // Reader object to read from
    buffer: String,               // Internal buffer
    peeked: char,                 // Currently peeked char
    offset: Offset,               // Current offset
    start: Offset,                // Offset of last commit
    pub eof: bool,                // EOF marker
}

impl Reader {
    /// Creates a new reader on buffer read.
    pub fn new(filename: Option<String>, read: Box<dyn Read>) -> Self {
        Self {
            filename,
            reader: Box::new(BufReader::new(read)),
            buffer: String::with_capacity(1024), //fixme: Modifyable capacity?
            peeked: ' ',
            offset: Offset {
                offset: 0,
                row: 1,
                col: 1,
            },
            start: Offset {
                offset: 0,
                row: 1,
                col: 1,
            },
            eof: false,
        }
    }

    /// Internal function for reading a line.
    fn read_line(&mut self) -> Option<usize> {
        if let Ok(n) = self.reader.read_line(&mut self.buffer) {
            if n == 0 {
                self.eof = true;
                return None;
            }

            Some(n)
        } else {
            self.eof = true;
            None
        }
    }

    pub fn tell(&self) -> Offset {
        self.offset
    }

    pub fn start(&self) -> Offset {
        self.start
    }

    pub fn eof(&mut self) -> bool {
        if self.buffer[self.offset.offset..].chars().next().is_some() {
            false
        } else {
            if !self.eof {
                self.peek();
            }

            self.eof
        }
    }

    pub fn reset(&mut self, offset: Offset) {
        self.offset = offset;
    }

    /// Capture last length characters.
    pub fn capture_last(&self, mut length: usize) -> Range {
        if length > self.offset.offset {
            length = self.offset.offset;
        }

        self.offset.offset - length..self.offset.offset
    }

    // Capture all characters from start to current offset.
    pub fn capture_from(&self, start: &Offset) -> Range {
        let mut start = start.offset;

        if start > self.offset.offset {
            start = self.offset.offset;
        }

        start..self.offset.offset
    }

    pub fn print(&self, start: usize) {
        println!("{:?}", &self.buffer[start..self.offset.offset])
    }

    /// Get slice from range
    pub fn get(&self, range: &Range) -> &str {
        &self.buffer[range.start..range.end]
    }

    /// Commits current input buffer and removes cached content
    pub fn commit(&mut self) {
        self.buffer.drain(0..self.offset.offset);
        self.start = self.offset;
        self.offset.offset = 0; // reset offset to 0
    }

    /// Take one character accepted by callback
    pub fn once<F>(&mut self, accept: F) -> Option<char>
    where
        F: Fn(char) -> bool,
    {
        if let Some(ch) = self.peek() {
            if accept(*ch) {
                return Some(self.next().unwrap());
            }
        }

        None
    }

    /// Read while conditional callback accepts characters
    pub fn span<F>(&mut self, accept: F) -> Option<&str>
    where
        F: Fn(char) -> bool + Copy,
    {
        let start = self.offset.offset;

        while self.once(accept).is_some() {}

        if start < self.offset.offset {
            Some(&self.buffer[start..self.offset.offset])
        } else {
            None
        }
    }
}

impl Iterator for Reader {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(ch) = self.buffer[self.offset.offset..].chars().next() {
                self.offset.offset += ch.len_utf8();

                if ch == '\n' {
                    self.offset.row += 1;
                    self.offset.col = 1;
                } else {
                    self.offset.col += 1;
                }

                return Some(ch);
            }

            if self.eof {
                return None;
            }

            self.read_line();
        }
    }
}

impl PeekableIterator for Reader {
    fn peek(&mut self) -> Option<&Self::Item> {
        loop {
            if let Some(ch) = self.buffer[self.offset.offset..].chars().next() {
                self.peeked = ch;
                return Some(&self.peeked);
            }

            if self.eof {
                return None;
            }

            self.read_line();
        }
    }
}
