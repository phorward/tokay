//! Tokay compiler interface
use std::collections::HashMap;
use std::io::BufReader;

use super::*;
use crate::builtin::Builtin;
use crate::error::Error;
use crate::reader::Reader;
use crate::value::{RefValue, Token};
use crate::vm::*;

/** Compiler symbolic scope.

In Tokay code, this relates to any block.
Parselets introduce new variable scopes.
Loops introduce a new loop scope.
*/
#[derive(Debug)]
pub(super) enum Scope {
    Parselet {
        // parselet-level scope (variables and constants can be defined here)
        usage_start: usize, // Begin of usages to resolve until when scope is closed
        constants: HashMap<String, ImlValue>, // Constants symbol table
        variables: HashMap<String, usize>, // Variable symbol table
        begin: Vec<ImlOp>,  // Begin operations
        end: Vec<ImlOp>,    // End operations
        consuming: bool, // Determines whether the scope is consuming input for early consumable detection
    },
    Block {
        // block level (constants can be defined here)
        usage_start: usize, // Begin of usages to resolve until when scope is closed
        constants: HashMap<String, ImlValue>, // Constants symbol table
    },
    Loop, // loop level (allows use of break & continue)
}

/** Tokay compiler instance

A tokay compiler initializes a Tokay parser for later re-use when called multiple times.

The compiler can be set into an interactive mode so that statics, variables and constants once built
won't be removed and can be accessed later on. This is useful in REPL mode.
*/
pub struct Compiler {
    parser: Option<parser::Parser>,   // Internal Tokay parser
    pub debug: u8,                    // Compiler debug mode
    pub(super) values: Vec<ImlValue>, // Constant values and parselets created during compile
    pub(super) scopes: Vec<Scope>,    // Current compilation scopes
    pub(super) usages: Vec<Result<Vec<ImlOp>, Usage>>, // Usages of symbols in parselets
    pub(super) errors: Vec<Error>,    // Collected errors during compilation
}

impl Compiler {
    /** Initialize new compiler.

    By default, the prelude should be loaded, otherwise several standard parselets are not available. */
    pub fn new(with_prelude: bool) -> Self {
        let mut compiler = Self {
            parser: None,
            debug: 0,
            values: Vec::new(),
            scopes: Vec::new(),
            usages: Vec::new(),
            errors: Vec::new(),
        };

        // Compile with the default prelude
        if with_prelude {
            compiler
                .compile_from_str(include_str!("../prelude.tok"))
                .unwrap(); // this should panic in case of an error!
        }

        // Set compiler debug level afterwards
        compiler.debug = if let Ok(level) = std::env::var("TOKAY_DEBUG") {
            level.parse::<u8>().unwrap_or_default()
        } else {
            0
        };

        compiler
    }

    /** Compile a Tokay program from a Reader source into the compiler. */
    pub fn compile(&mut self, reader: Reader) -> Result<(), Vec<Error>> {
        // Create the Tokay parser when not already done
        if self.parser.is_none() {
            self.parser = Some(Parser::new());
        }

        let parser = self.parser.as_ref().unwrap();
        let ast = match parser.parse(reader) {
            Ok(ast) => ast,
            Err(error) => {
                eprintln!("{}", error);
                return Err(vec![error]);
            }
        };

        if self.debug > 0 {
            ast::print(&ast);
        }

        ast::traverse(self, &ast);

        if self.errors.len() > 0 {
            for error in &self.errors {
                eprintln!("{}", error);
            }

            return Err(self.errors.drain(..).collect());
        }

        Ok(())
    }

    /// Shortcut to compile a Tokay program from a &str into the compiler.
    pub fn compile_from_str(&mut self, src: &str) -> Result<(), Vec<Error>> {
        self.compile(Reader::new(Box::new(BufReader::new(std::io::Cursor::new(
            src.to_owned(),
        )))))
    }

    /** Converts the current compiler state into a Program. */
    pub fn finalize(&mut self) -> Result<Program, Vec<Error>> {
        // Check for correct scope level
        assert!(self.scopes.len() == 1);

        let mut errors: Vec<Error> = Vec::new();

        // Check and report any unresolved usages
        let mut usages = self
            .usages
            .drain(..)
            .map(|usage| {
                match usage {
                    Ok(usage) => usage,
                    Err(usage) => {
                        let error = match usage {
                            Usage::Load { name, offset } | Usage::CallOrCopy { name, offset } => {
                                Error::new(offset, format!("Use of unresolved symbol '{}'", name))
                            }

                            Usage::Call {
                                name,
                                args: _,
                                nargs: _,
                                offset,
                            } => {
                                Error::new(offset, format!("Call to unresolved symbol '{}'", name))
                            }

                            Usage::Error(error) => error,
                        };

                        errors.push(error);
                        vec![ImlOp::Nop] // Dummy instruction
                    }
                }
            })
            .collect();

        // Obtain intermediate values collected during compilation
        let values = self.values.clone();
        self.values.pop(); // pop-off last value, which is always the last main parselet

        /*
            Finalize the program by resolving any unresolved usages according to a grammar's
            point of view; This closure algorithm runs until no more changes on any parselet
            configuration occurs.

            The algorithm detects correct flagging fore nullable and left-recursive for any
            consuming parselet.

            It requires all parselets consuming input to be known before the finalization phase.
            Normally, this is already known due to Tokays identifier classification.

            Maybe there will be a better method for this detection in future.
        */
        let mut changes = true;
        let mut loops = 0;

        while changes {
            changes = false;

            for i in 0..values.len() {
                if let ImlValue::Parselet(parselet) = &values[i] {
                    let mut parselet = parselet.borrow_mut();

                    // Resolve usages
                    if loops == 0 {
                        parselet.resolve(&mut usages);
                    }

                    // Don't finalize any non-consuming parselet
                    if parselet.consuming.is_none() {
                        continue;
                    }

                    // Finalize according to grammar view, find left-recursions
                    let consuming = parselet.consuming.clone().unwrap();
                    let mut stack = vec![(i, consuming.nullable)];
                    if let Some(consuming) = parselet.finalize(&values, &mut stack) {
                        if *parselet.consuming.as_ref().unwrap() < consuming {
                            parselet.consuming = Some(consuming);
                            changes = true;
                        }
                    }
                }
            }

            loops += 1;
        }

        /*
        for i in 0..values.len() {
            if let ImlValue::Parselet(parselet) = &values[i] {
                let parselet = parselet.borrow();

                println!(
                    "{} consuming={:?}",
                    parselet.name.as_deref().unwrap_or("(unnamed)"),
                    parselet.consuming
                );
            }
        }

        println!("Finalization finished after {} loops", loops);
        */

        // Stop when any unresolved usages occured;
        // We do this here so that eventual undefined symbols are replaced by ImlOp::Nop,
        // and later don't throw other errors.
        if errors.len() > 0 {
            for error in &errors {
                eprintln!("{}", error);
            }

            return Err(errors);
        }

        // Compile values into a program
        let program = Program::new(
            values
                .into_iter()
                .map(|value| match value {
                    ImlValue::Parselet(parselet) => {
                        RefValue::from(parselet.borrow().into_parselet())
                    }
                    ImlValue::Value(value) => value,
                })
                .collect(),
        );

        if self.debug > 0 {
            program.dump();
        }

        Ok(program)
    }

    /// Resolves usages from current scope
    pub(super) fn resolve(&mut self) {
        if let Scope::Parselet { usage_start, .. } | Scope::Block { usage_start, .. } =
            &self.scopes[0]
        {
            // Cut out usages created inside this scope for processing
            let usages: Vec<Result<Vec<ImlOp>, Usage>> = self.usages.drain(usage_start..).collect();

            // Afterwards, resolve and insert them again
            for usage in usages.into_iter() {
                match usage {
                    Err(mut usage) => {
                        if let Some(res) = usage.try_resolve(self) {
                            self.usages.push(Ok(res))
                        } else {
                            self.usages.push(Err(usage))
                        }
                    }
                    Ok(res) => self.usages.push(Ok(res)),
                }
            }
        }
    }

    /// Push a parselet scope
    pub(super) fn push_parselet(&mut self) {
        self.scopes.insert(
            0,
            Scope::Parselet {
                usage_start: self.usages.len(),
                variables: HashMap::new(),
                constants: HashMap::new(),
                begin: Vec::new(),
                end: Vec::new(),
                consuming: false,
            },
        )
    }

    /// Push a block scope
    pub(super) fn push_block(&mut self) {
        self.scopes.insert(
            0,
            Scope::Block {
                usage_start: self.usages.len(),
                constants: HashMap::new(),
            },
        )
    }

    /// Push a loop scope
    pub(super) fn push_loop(&mut self) {
        self.scopes.insert(0, Scope::Loop);
    }

    /// Resolves and drops a parselet scope and creates a new parselet from it.
    pub(super) fn pop_parselet(
        &mut self,
        name: Option<String>,
        sig: Vec<(String, Option<usize>)>,
        body: ImlOp,
    ) -> ImlParselet {
        assert!(self.scopes.len() > 0 && matches!(self.scopes[0], Scope::Parselet { .. }));

        self.resolve();
        let mut scope = self.scopes.remove(0);

        if let Scope::Parselet {
            variables,
            begin,
            end,
            consuming,
            ..
        } = &mut scope
        {
            fn ensure_block(ops: Vec<ImlOp>) -> ImlOp {
                match ops.len() {
                    0 => ImlOp::Nop,
                    1 => ops.into_iter().next().unwrap(),
                    _ => ImlAlternation::new(ops).into_op(),
                }
            }

            let mut parselet = ImlParselet::new(
                name,
                sig,
                variables.len(),
                // Ensure that begin and end are blocks.
                ensure_block(begin.drain(..).collect()),
                ensure_block(end.drain(..).collect()),
                body,
            );

            parselet.consuming = if *consuming {
                Some(Consumable {
                    leftrec: false,
                    nullable: false,
                })
            } else {
                None
            };

            if self.scopes.len() == 0 {
                *consuming = false;
                self.scopes.push(scope);
            }

            parselet
        } else {
            unreachable!();
        }
    }

    /// Drops a block scope.
    pub(super) fn pop_block(&mut self) {
        assert!(self.scopes.len() > 0 && matches!(self.scopes[0], Scope::Block { .. }));
        self.resolve();
        self.scopes.remove(0);
    }

    /// Drops a loop scope.
    pub(super) fn pop_loop(&mut self) {
        assert!(self.scopes.len() > 0 && matches!(self.scopes[0], Scope::Loop));
        self.scopes.remove(0);
    }

    /// Marks the nearest parselet scope as consuming
    pub(super) fn mark_consuming(&mut self) {
        for scope in &mut self.scopes {
            if let Scope::Parselet { consuming, .. } = scope {
                *consuming = true;
                return;
            }
        }

        unreachable!("There _must_ be at least one parselet scope!");
    }

    /// Check if there's a loop
    pub(super) fn check_loop(&mut self) -> bool {
        for i in 0..self.scopes.len() {
            match &self.scopes[i] {
                Scope::Parselet { .. } => return false,
                Scope::Loop => return true,
                _ => {}
            }
        }

        unreachable!("There _must_ be at least one parselet scope!");
    }

    /** Retrieves the address of a local variable under a given name.

    Returns None when the variable does not exist. */
    pub(super) fn get_local(&self, name: &str) -> Option<usize> {
        // Retrieve local variables from next parselet scope owning variables, except global scope!
        for scope in &self.scopes[..self.scopes.len() - 1] {
            // Check for scope with variables
            if let Scope::Parselet { variables, .. } = &scope {
                if let Some(addr) = variables.get(name) {
                    return Some(*addr);
                }

                break;
            }
        }

        None
    }

    /** Insert new local variable under given name in current scope. */
    pub(super) fn new_local(&mut self, name: &str) -> usize {
        for scope in &mut self.scopes {
            // Check for scope with variables
            if let Scope::Parselet { variables, .. } = scope {
                if let Some(addr) = variables.get(name) {
                    return *addr;
                }

                let addr = variables.len();
                variables.insert(name.to_string(), addr);
                return addr;
            }
        }

        unreachable!("There _must_ be at least one parselet scope!");
    }

    /** Retrieve address of a global variable. */
    pub(super) fn get_global(&self, name: &str) -> Option<usize> {
        if let Scope::Parselet { variables, .. } = self.scopes.last().unwrap() {
            if let Some(addr) = variables.get(name) {
                return Some(*addr);
            }

            return None;
        }

        unreachable!("Top-level scope is not a parselet scope");
    }

    /** Set constant to name in current scope. */
    pub(super) fn set_constant(&mut self, name: &str, mut value: ImlValue) {
        /*
            Special meaning for whitespace constants names "_" and "__".

            When set, the corresponding consumable Value becomes the following:

            - `__ : Value+`
            - `_ : __?`

            This is always the case whenever "_" or "__" is set.
            Fallback defaults to `Value : Whitespace`, handled in get_constant().
        */
        let mut secondary = None;

        if name == "_" || name == "__" {
            // First of all, "__" is defined as `__ : Value+`...
            let mut parselet = ImlParselet::new(
                Some("__".to_string()),
                Vec::new(),
                0,
                ImlOp::Nop,
                ImlOp::Nop,
                // becomes `Value+`
                ImlRepeat::new(Op::CallStatic(self.define_value(value)).into(), 1, 0).into_op(),
            );

            parselet.consuming = Some(Consumable {
                leftrec: false,
                nullable: false,
            });
            parselet.severity = 0;

            value = parselet.into();

            // Insert "__" as new constant
            secondary = Some(("__", value.clone()));

            // ...and then in-place "_" is defined as `_ : __?`
            let mut parselet = ImlParselet::new(
                Some(name.to_string()),
                Vec::new(),
                0,
                ImlOp::Nop,
                ImlOp::Nop,
                // becomes `Value?`
                ImlRepeat::new(Op::CallStatic(self.define_value(value)).into(), 0, 1).into_op(),
            );

            parselet.consuming = Some(Consumable {
                leftrec: false,
                nullable: false,
            });
            parselet.severity = 0;

            value = parselet.into();

            // Insert "_" afterwards
        }

        // Insert constant into next constant-holding scope
        for scope in &mut self.scopes {
            if let Scope::Parselet { constants, .. } | Scope::Block { constants, .. } = scope {
                if let Some((name, value)) = secondary {
                    constants.insert(name.to_string(), value);
                }

                constants.insert(name.to_string(), value);
                return;
            }
        }

        unreachable!("There _must_ be at least one parselet or block scope!");
    }

    /** Get constant value, either from current or preceding scope,
    a builtin or special. */
    pub(super) fn get_constant(&mut self, name: &str) -> Option<ImlValue> {
        // Check for constant in available scopes
        for scope in &self.scopes {
            if let Scope::Parselet { constants, .. } | Scope::Block { constants, .. } = scope {
                if let Some(value) = constants.get(name) {
                    return Some(value.clone());
                }
            }
        }

        // When not found, check for a builtin function
        if let Some(builtin) = Builtin::get(name) {
            return Some(RefValue::from(builtin).into()); // fixme: Makes a Value into a RefValue into a Value...
        }

        // Builtin constants are defined on demand as fallback
        if name == "_" || name == "__" {
            // Fallback for "_" defines parselet `_ : Whitespace?`
            self.set_constant(
                "_",
                RefValue::from(Token::builtin("Whitespaces").unwrap()).into(),
            );

            return Some(self.get_constant(name).unwrap());
        }

        // Check for built-in token
        if let Some(value) = Token::builtin(name) {
            return Some(RefValue::from(value).into());
        }

        None
    }

    /** Defines a new constant value for compilation.
    Constants are only being inserted once when they already exist. */
    pub(super) fn define_value(&mut self, value: ImlValue) -> usize {
        // Check if there exists already a n equivalent constant
        // fixme: A HashTab might be faster here...
        {
            for (i, known) in self.values.iter().enumerate() {
                if *known == value {
                    return i; // Reuse existing value address
                }
            }
        }

        // Save value as new
        self.values.push(value);
        self.values.len() - 1
    }
}

#[test]
fn test_whitespace() {
    // Builtin whitespace handling
    let abc = "abc   \tdef  abcabc= ghi abcdef";

    assert_eq!(
        crate::run("Word _; ", abc),
        Ok(Some(crate::value![[
            "abc", "def", "abcabc", "ghi", "abcdef"
        ]]))
    );

    assert_eq!(
        crate::run("Word __; ", abc),
        Ok(Some(crate::value![["abc", "def", "ghi"]]))
    );
}

#[test]
// Testing several special parsing constructs and error reporting
fn test_error_reporting() {
    // Test for programs which consist just of one comment
    assert_eq!(crate::run("#tralala", ""), Ok(None));

    // Test for whitespace
    assert_eq!(
        crate::run("#normal comment\n#\n\t123", ""),
        Ok(Some(crate::value!(123)))
    );

    // Test for invalid input when EOF is expected
    assert_eq!(
        crate::run("{}}", ""),
        Err("Line 1, column 3: Parse error, expecting end-of-file".to_string())
    );

    // Test on unclosed sequences `(1 `
    assert_eq!(
        crate::run("(1", ""),
        Err("Line 1, column 3: Expecting \")\"".to_string())
    );

    assert_eq!(
        crate::run("(a => 1, b => 2", ""),
        Err("Line 1, column 16: Expecting \")\"".to_string())
    );

    // Test empty sequence
    assert_eq!(crate::run("()", ""), Ok(Some(crate::value::List::new().into())));

    // Tests on filled and empty blocks and empty blocks
    assert_eq!(
        crate::run(
            "
            a = {}
            b = {
            }
            c = {
                1
                2
                3
            }

            a b c
            ",
            ""
        ),
        Ok(Some(crate::value!(3)))
    );
}

#[test]
// Tests for correct identifier names for various value types
fn test_identifier_naming() {
    crate::test::testcase("tests/err_compiler_identifier_names.tok");
}

#[test]
// Tests for compiler string, match and ccl escaping
fn test_unescaping() {
    assert_eq!(
        crate::run(
            "\"test\\\\yes\n\\xCA\\xFE\t\\100\\x5F\\u20ac\\U0001F98E\"",
            ""
        ),
        Ok(Some(crate::value!("test\\yes\nÊþ\t@_€🦎")))
    );

    assert_eq!(
        crate::run(
            "'hello\\nworld'", // double \ quotation required
            "hello\nworld"
        ),
        Ok(Some(crate::value!("hello\nworld")))
    );

    assert_eq!(
        crate::run(
            "[0-9\\u20ac]+", // double \ quotation required
            "12345€ €12345"
        ),
        Ok(Some(crate::value!(["12345€", "€12345"])))
    );

    assert_eq!(
        crate::run(
            "'hello\\nworld'", // double \ quotation required
            "hello\nworld"
        ),
        Ok(Some(crate::value!("hello\nworld")))
    );

    assert_eq!(
        crate::run(
            "[0-9\\u20ac]+", // double \ quotation required
            "12345€ €12345"
        ),
        Ok(Some(crate::value!(["12345€", "€12345"])))
    );
}

#[test]
// Tests for compiler string, match and ccl escaping
fn test_prelude() {
    assert_eq!(
        crate::run("Number", "123 45.67 -8 -9.10"),
        Ok(Some(crate::value!([123, 45.67, (-8), (-9.1)])))
    );

    assert_eq!(
        crate::run(
            "Token",
            "The tokay gecko reaches a total length (including tail) of 25-30 cm on average."
        ),
        Ok(Some(crate::value!([
            "The",
            "tokay",
            "gecko",
            "reaches",
            "a",
            "total",
            "length",
            "(",
            "including",
            "tail",
            ")",
            "of",
            25,
            (-30),
            "cm",
            "on",
            "average",
            "."
        ])))
    );
}
