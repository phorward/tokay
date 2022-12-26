//! Parselet object represents a callable, user-defined function.

use std::cell::RefCell;
use std::rc::Rc;

use super::{BoxedObject, Dict, Object, RefValue};

use crate::error::Error;
use crate::vm::*;

/** Parselet is the conceptual building block of a Tokay program.

A parselet is like a function in ordinary programming languages, with the
exception that it can either be a snippet of parsing instructions combined with
semantic code, or just an ordinary function consisting of code and returning
values. The destinction if a parselet represents just a function or a parselet is
done by the consuming-flag, which is determined by use of static tokens, parselets
and consuming builtins.

Parselets support static program constructs being left-recursive, and extend
the generated parse tree automatically until no more input can be consumed.
*/

#[derive(Debug)]
pub struct Parselet {
    pub name: String,                   // Parselet's name from source (for debugging)
    pub(crate) consuming: Option<bool>, // Indicator for consuming & left-recursion
    pub(crate) severity: u8,            // Capture push severity
    signature: Vec<(String, Option<usize>)>, // Argument signature with default arguments
    pub(crate) locals: usize,           // Number of local variables present
    pub(crate) begin: Vec<Op>,          // Begin-operations
    pub(crate) end: Vec<Op>,            // End-operations
    pub(crate) body: Vec<Op>,           // Operations
}

impl Parselet {
    /// Creates a new parselet.
    pub(crate) fn new(
        name: Option<String>,
        consuming: Option<bool>,
        severity: u8,
        signature: Vec<(String, Option<usize>)>,
        locals: usize,
        begin: Vec<Op>,
        end: Vec<Op>,
        body: Vec<Op>,
    ) -> Self {
        assert!(
            signature.len() <= locals,
            "signature may not be longer than locals..."
        );

        let mut ret = Self {
            name: name.unwrap_or(String::new()),
            consuming,
            severity,
            signature,
            locals,
            begin,
            end,
            body,
        };

        if ret.name.is_empty() {
            ret.name = format!("parselet_{:x}", &ret as *const Parselet as usize);
        }

        ret
    }

    /** Run parselet on a given runtime.

    The main-parameter defines if the parselet behaves like a main loop or
    like subsequent parselet. */
    pub fn run(
        &self,
        program: &Program,
        runtime: &mut Runtime,
        args: usize,
        mut nargs: Option<Dict>,
        main: bool,
        depth: usize,
    ) -> Result<Accept, Reject> {
        // Check for a previously memoized result in memo table
        let id = self as *const Parselet as usize;

        // When parselet is consuming, try to read previous result from cache.
        if self.consuming.is_some() {
            // Get unique parselet id from memory address
            let reader_start = runtime.reader.tell();

            if let Some((reader_end, result)) = runtime.memo.get(&(reader_start.offset, id)) {
                runtime.reader.reset(*reader_end);
                return result.clone();
            }
        }

        // If not, start a new context.
        let mut context = Context::new(
            program,
            self,
            runtime,
            self.locals,
            args,
            if main { self.locals } else { 0 }, // Hold runtime globals when this is main!
            depth,
        );

        if !main {
            // Check for provided argument count bounds first
            // todo: Not executed when *args-catchall is implemented
            if args > self.signature.len() {
                return Err(match self.signature.len() {
                    0 => format!(
                        "{}() doesn't accept any arguments ({} given)",
                        self.name, args
                    ),
                    1 => format!(
                        "{}() takes exactly one argument ({} given)",
                        self.name, args
                    ),
                    _ => format!(
                        "{}() expected at most {} arguments ({} given)",
                        self.name,
                        self.signature.len(),
                        args
                    ),
                }
                .into())
                .into();
            }

            // Set remaining parameters to their defaults
            for (i, arg) in (&self.signature[args..]).iter().enumerate() {
                // args parameters are previously pushed onto the stack.
                let var = &mut context.runtime.stack[context.stack_start + args + i];

                //println!("{} {:?} {:?}", i, arg, var);
                if matches!(var, Capture::Empty) {
                    // In case the parameter is empty, try to get it from nargs...
                    if let Some(ref mut nargs) = nargs {
                        if let Some(value) = nargs.remove_str(&arg.0) {
                            *var = Capture::Value(value, None, 0);
                            continue;
                        }
                    }

                    // Otherwise, use default value if available.
                    if let Some(addr) = arg.1 {
                        // fixme: This might leak the immutable static value to something mutable...
                        *var = Capture::Value(context.program.statics[addr].clone(), None, 0);
                        //println!("{} receives default {:?}", arg.0, var);
                        continue;
                    }

                    return Error::new(
                        None,
                        format!("{}() expected argument '{}'", self.name, arg.0),
                    )
                    .into();
                }
            }

            // Check for remaining nargs
            // todo: Not executed when **nargs-catchall is implemented
            if let Some(mut nargs) = nargs {
                if let Some((name, _)) = nargs.pop() {
                    return Err(match nargs.len() {
                        0 => format!(
                            "{}() doesn't accept named argument '{}'",
                            self.name,
                            name.to_string()
                        ),
                        n => format!(
                            "{}() doesn't accept named arguments ({} given)",
                            self.name,
                            n + 1
                        ),
                    }
                    .into())
                    .into();
                }
            }
        } else
        /* main */
        {
            assert!(self.signature.len() == 0)
        }

        // Initialize locals
        for i in 0..self.locals {
            if let Capture::Empty = context.runtime.stack[context.stack_start + i] {
                context.runtime.stack[context.stack_start + i] =
                    Capture::Value(crate::value!(void), None, 0);
            }
        }

        //println!("remaining {:?}", nargs);

        // Perform left-recursive execution
        let result = if let Some(true) = self.consuming {
            /*
            println!(
                "--- {} @ {} ---",
                self.name.as_deref().unwrap_or("(unnamed)"),
                context.frame0().reader_start.offset
            );
            */

            // Left-recursive parselets are called in a loop until no more input is consumed.
            let mut reader_end = context.frame0().reader_start;
            let mut result = Err(Reject::Next);

            // Insert a fake memo entry to avoid endless recursion
            context.runtime.memo.insert(
                (context.frame0().reader_start.offset, id),
                (reader_end, result.clone()),
            );

            loop {
                let loop_result = context.run(main);

                match loop_result {
                    // Hard reject
                    Err(Reject::Main) | Err(Reject::Error(_)) => {
                        result = loop_result;
                        break;
                    }

                    // Soft reject
                    Err(_) => break,

                    _ => {}
                }

                let loop_end = context.runtime.reader.tell();

                // Stop when no more input was consumed
                if loop_end.offset <= reader_end.offset {
                    break;
                }

                result = loop_result;
                reader_end = loop_end;

                // Save intermediate result in memo table
                context.runtime.memo.insert(
                    (context.frame0().reader_start.offset, id),
                    (reader_end, result.clone()),
                );

                // Reset reader & stack
                context.runtime.reader.reset(context.frame0().reader_start);
                context.runtime.stack.truncate(context.stack_start); //fixme: context.frame0()?
                context
                    .runtime
                    .stack
                    .resize(context.frame0().capture_start, Capture::Empty);
            }

            context.runtime.reader.reset(reader_end);

            result
        } else {
            let result = context.run(main);

            if !main && self.consuming.is_some() {
                context.runtime.memo.insert(
                    (context.frame0().reader_start.offset, id),
                    (context.runtime.reader.tell(), result.clone()),
                );
            }

            result
        };

        /*
        // Dump AST when parselet returns an AST for debugging purposes.
        // fixme: Disabled for now, can be enabled on demand.
        if context.runtime.debug > 1 {
            loop {
                if let Ok(Accept::Push(Capture::Value(ref value, ..))) = result {
                    let value = value.borrow();
                    if let Some(d) = value.dict() {
                        if d.get("emit").is_some() {
                            context.debug("=> AST");
                            ast::print(&value);
                            break;
                        }
                    }
                }

                context.debug(&format!("=> {:?}", result));
                break;
            }
        }
        */

        result
    }
}

impl From<Parselet> for RefValue {
    fn from(parselet: Parselet) -> Self {
        RefValue::from(Box::new(ParseletRef(Rc::new(RefCell::new(parselet)))) as BoxedObject)
    }
}

#[derive(Clone, Debug)]
pub struct ParseletRef(pub Rc<RefCell<Parselet>>);

impl Object for ParseletRef {
    fn id(&self) -> usize {
        &*self.0.borrow() as *const Parselet as usize
    }

    fn name(&self) -> &'static str {
        "parselet"
    }

    fn repr(&self) -> String {
        format!("<{} {}>", self.name(), self.0.borrow().name)
    }

    fn is_callable(&self, without_arguments: bool) -> bool {
        let parselet = self.0.borrow();

        if without_arguments {
            parselet.signature.len() == 0 || parselet.signature.iter().all(|arg| arg.1.is_some())
        } else {
            true
        }
    }

    fn is_consuming(&self) -> bool {
        self.0.borrow().consuming.is_some()
    }

    fn call(
        &self,
        context: &mut Context,
        args: usize,
        nargs: Option<Dict>,
    ) -> Result<Accept, Reject> {
        self.0.borrow().run(
            context.program,
            context.runtime,
            args,
            nargs,
            false,
            context.depth + 1,
        )
    }
}

impl PartialEq for ParseletRef {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl PartialOrd for ParseletRef {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id().partial_cmp(&other.id())
    }
}
