use super::*;
use crate::error::Error;
use crate::reader::Reader;
use crate::value::{Dict, RefValue};
use log;
/** The Tokay parser is implemented in Tokay itself.

Because the parser must be some kind of bootstrapped to run in the Tokay VM,
it can be either loaded as a pre-compiled binary VM program from `_tokay.cbor`,
or it can be previously compiled once from a pre-generated AST representation
expressed in Rust (`_tokay.rs`).

Both sources are generated, either by the serde_cbor serialization, or by the
scripting toolchain etareneg.awk and the ast2rust builtin.
*/
use std::cell::RefCell;
use std::rc::Rc;
use std::thread_local;

thread_local! {
    // Keep a single Tokay parser for every thread.
    static PARSER: Rc<RefCell<Program>> = {
        {
            let parser_program = {
                // `_tokay.cbor` is a pre-compiled Tokay VM program in a binary format that implements the Tokay parser implemented in `tokay.tok`.
                #[cfg(feature = "use_cbor_parser")]
                {
                    log::info!("Using pre-compiled parser: _tokay.cbor");
                    serde_cbor::from_slice(include_bytes!("_tokay.cbor")).unwrap()
                }

                // `_tokay.rs` is a generated representation of the abstract syntax tree of `tokay.tok` itself, which is the internally compiled by the Tokay compiler.
                #[cfg(not(feature = "use_cbor_parser"))]
                {
                    let mut compiler = Compiler::new();
                    compiler.debug = 0; // unset debug always

                    log::info!("Using ast-based parser: _tokay.rs");
                    compiler
                        .compile_from_ast(&include!("_tokay.rs"), Some("parser".to_string()))
                        .expect("Tokay grammar cannot be compiled!")
                        .expect("Tokay grammar contains no main?")
                }
            };

            Rc::new(RefCell::new(parser_program))
        }
    }
}

pub struct Parser(Rc<RefCell<Program>>);

impl Parser {
    pub fn new() -> Self {
        Self(PARSER.with(|parser| parser.clone()))
    }

    pub fn parse(&self, mut reader: Reader) -> Result<RefValue, Error> {
        let program = self.0.borrow();
        let mut thread = Thread::new(&*program, vec![&mut reader]);

        if let Ok(level) = std::env::var("TOKAY_PARSER_DEBUG") {
            thread.debug = level.parse::<u8>().unwrap_or_default();
        } else {
            thread.debug = 0;
        }

        match thread.run() {
            Ok(Some(ast)) => {
                if ast.borrow().object::<Dict>().is_some() {
                    Ok(ast)
                } else {
                    Err(Error::new(None, "Parse error".to_string()))
                }
            }
            Ok(None) => Ok(crate::value!(void)),
            Err(error) => Err(error),
        }
    }
}

/*
    Below are some tests that provide indirect left-recursion.

    They currently don't work properly due to the following reason:
    For indirect left-recursion in packrat parsing, one rule in the
    grammar's graph must be declared as "leading", so that subsequent,
    even left-recursive parselets are considered as not left-recursive.


    An implementation of an solution for this issue can be found in
    the pegen parser generator from Python:

    https://github.com/python/cpython/blob/main/Tools/peg_generator/pegen/parser_generator.py

    Tokay won't take care of this right now as it is an edge-case
    and also more complex, as Tokay does not directly implements a
    grammar.
*/

//fixme: Remove this into tests...
/*
#[test]
fn parser_indirectleftrec() {
    /*
        X: Y 'c'
        Y: Z 'b'
        Z: X | Y | 'a'
        Z
    */

    let program = tokay!({
        (X = {
            [Y, (MATCH "c")]
        }),
        (Y = {
            [Z, (MATCH "b")]
            //Void
        }),
        (Z = {
            X,
            Y,
            (MATCH "a")
        }),
        Z
    });

    println!("{:#?}", program.run_from_str("aaabc"));
}

#[test]
fn parser_leftrec() {
    /*
    let program = tokay!({
        (X = {
            [X, (MATCH "b")],
            (MATCH "a")
        }),

        X
    });
    */

    let program = tokay!({
        (Y = {
            X,
            (MATCH "a")
        }),
        (X = {
            [Y, (MATCH "b")]
        }),
        X
    });

    /*
    let program = tokay!({
        (Factor = {
            ["(", (pos [Expression]), ")"],
            (token (Token::Chars(charclass!['0'..='9'])))
        }),
        (Expression = {
            [Expression, "+", Expression],
            Factor
        }),
        Expression
    });
    */

    println!("{:#?}", program.run_from_str("abb"));
}
*/

#[test]
// EOL
fn parser_eol() {
    for eol in ["\n", "\r", "\r\n", ";"] {
        let tok = format!("a = 1{}a + 2", eol);
        println!("EOL test {:?}", tok);
        assert_eq!(crate::eval(&tok, "", None), Ok(crate::value!(3)));
    }
}
