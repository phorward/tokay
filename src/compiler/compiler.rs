//! Tokay compiler

use super::*;
use crate::error::Error;
use crate::reader::*;
use crate::value;
use crate::value::RefValue;
use crate::vm::*;
use indexmap::{indexset, IndexMap, IndexSet};
use std::cell::RefCell;

/** Tokay compiler instance

A tokay compiler initializes a Tokay parser for later re-use when called multiple times.

The compiler works in a mode so that statics, variables and constants once built
won't be removed and can be accessed on later calls.
*/
pub struct Compiler {
    parser: Option<parser::Parser>, // Internal Tokay parser
    pub debug: u8,                  // Compiler debug mode
    pub(super) restrict: bool,      // Restrict assignment of reserved identifiers
    pub(super) statics: RefCell<IndexSet<RefValue>>, // Static values collected during compilation

    // TODO: As workaround to emulate old behavior of the Compiler struct
    main: ImlParseletModel,                // keep global parselet
    constants: IndexMap<String, ImlValue>, // keep global constants
}

impl Compiler {
    /** Initialize a new compiler.

    The compiler serves functions to compile Tokay source code into programs executable by
    the Tokay VM. It uses an intermediate language representation to implement derives of
    generics, statics, etc.

    The compiler struct serves as some kind of helper that should be used during traversal of a
    Tokay program's AST. It therefore offers functions to open particular blocks and handle symbols
    in different levels. Parselets are created by using the parselet_pop() function with provided
    parameters.
    */
    pub fn new() -> Self {
        let statics = indexset![
            value!(void),
            value!(null),
            value!(true),
            value!(false),
            value!(0),
            value!(1),
        ];

        let mut compiler = Self {
            parser: None,
            debug: 0,
            restrict: true,
            statics: RefCell::new(statics),
            // TODO: workaround...
            main: ImlParseletModel::new(None),
            constants: IndexMap::new(),
        };

        // Compile with the default prelude
        compiler.load_prelude();

        // Set compiler debug level afterwards
        compiler.debug = if let Ok(level) = std::env::var("TOKAY_DEBUG") {
            level.parse::<u8>().unwrap_or_default()
        } else {
            0
        };

        compiler
    }

    /** Compile a Tokay program from an existing AST into the compiler. */
    pub(super) fn compile_from_ast(
        &mut self,
        ast: &RefValue,
    ) -> Result<Option<Program>, Vec<Error>> {
        // Create main parselet from current main model
        let main_parselet = ImlParselet::new(ImlParseletInstance::new(
            // TODO: Keep backward compatible: copy Compiler's main model into the main_parselet
            Some(self.main.clone()),
            None,
            None,
            Some("__main__".to_string()),
            5,
            false,
        ));

        // println!("=> self.constants {:?}", self.constants.keys());

        self.constants = {
            // Create new global scope
            let global_scope = Scope::new(self, ScopeLevel::Parselet(main_parselet.clone()), None);

            // Extend compiler's constants into global_scope
            global_scope
                .constants
                .borrow_mut()
                .extend(self.constants.clone());

            // Traverse the parsed AST
            ast::traverse(&global_scope, &ast);

            // try to resolve any open usages
            global_scope.resolve_usages();

            // println!("constants {:#?}, {} usages", global_scope.constants, global_scope.usages.borrow().len());

            // Report unresolved names
            // println!("usages = {:?}", global_scope.usages);

            for usage in global_scope.usages.borrow_mut().drain(..) {
                global_scope.error(usage.offset(), format!("Use of undefined name '{}'", usage));
            }

            // Break on error
            if !global_scope.errors.borrow().is_empty() {
                return Err(global_scope.errors.borrow_mut().drain(..).collect());
            }

            // Otherwise, write new contants back into compiler
            global_scope.constants.take()
        };

        // println!("<= self.constants {:?}", self.constants.keys());

        // TODO: Keep backward compatible: copy main parselet and constants into compiler
        self.main = main_parselet.borrow().model.borrow().clone();
        self.main.body = ImlOp::Nop;
        self.main.begin = ImlOp::Nop;
        self.main.end = ImlOp::Nop;

        /*
        if self.debug > 1 {
            println!("--- Global scope ---\n{:#?}", scope)
        }
        */

        if self.debug > 1 {
            println!("--- Intermediate main ---\n{:#?}", main_parselet);
        }

        let mut program = ImlProgram::new(ImlValue::from(main_parselet));
        program.debug = self.debug > 1;

        let ret = program.compile();

        // println!("\n--- program compiled: {:?} ---\n", ret);

        match ret {
            Ok(program) => {
                if self.debug > 1 {
                    println!("--- Finalized program ---");
                    program.dump();
                }

                Ok(Some(program))
            }
            Err(errors) => Err(errors),
        }
    }

    /** Compile a Tokay program from a Reader source into the compiler. */
    pub fn compile(&mut self, reader: Reader) -> Result<Option<Program>, Vec<Error>> {
        // Create the Tokay parser when not already done
        if self.parser.is_none() {
            self.parser = Some(Parser::new());
        }

        let parser = self.parser.as_ref().unwrap();
        let ast = match parser.parse(reader) {
            Ok(ast) => ast,
            Err(error) => {
                return Err(vec![error]);
            }
        };

        if self.debug > 0 {
            println!("--- Abstract Syntax Tree ---");
            ast::print(&ast);
            //println!("###\n{:#?}\n###", ast);
        }

        self.compile_from_ast(&ast)
    }

    /// Shortcut to compile a Tokay program from a &str into the compiler.
    pub fn compile_from_str(&mut self, src: &str) -> Result<Option<Program>, Vec<Error>> {
        self.compile(Reader::new(
            None,
            Box::new(std::io::Cursor::new(src.to_owned())),
        ))
    }

    /** Register a static value within a compiler instance.

    This avoids that the compiler produces multiple results pointing to effectively the same values
    (althought they are different objects, but  the same value)
    */
    pub(super) fn register_static(&self, value: RefValue) -> ImlValue {
        let mut statics = self.statics.borrow_mut();

        if let Some(value) = statics.get(&value) {
            ImlValue::Value(value.clone())
        } else {
            statics.insert(value.clone());
            ImlValue::Value(value)
        }
    }
}
