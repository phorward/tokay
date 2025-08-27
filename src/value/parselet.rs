//! Parselet object represents a callable, user-defined function.

use super::{BoxedObject, Dict, Object, RefValue};
use crate::error::Error;
use crate::vm::*;
use std::cell::RefCell;
use std::rc::Rc;

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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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

    /** Run parselet on a given thread.

    The main-parameter defines if the parselet behaves like a main loop or
    like subsequent parselet. */
    pub fn run(
        &self,
        thread: &mut Thread,
        mut args: Vec<Capture>,
        mut nargs: Option<Dict>,
        main: bool,
        depth: usize,
    ) -> Result<Accept, Reject> {
        // Get unique parselet id from memory address
        let id = self as *const Parselet as usize;

        // When parselet is consuming, try to read previous result from cache.
        if self.consuming.is_some() {
            let reader_start = thread.reader.tell();

            // Check for a previously memoized result
            // fixme: This doesn't recognize calls to the same parselet with same parameters,
            //        which might lead in unwanted results. This must be checked! It might become
            //        a problem when the Repeat<P>(min=0, max=void) generic parselet becomes available.
            if let Some((reader_end, result)) = thread.memo.get(&(reader_start.offset, id)) {
                thread.reader.reset(*reader_end);
                return result.clone();
            }
        }

        let args_len = args.len();

        // Check for provided argument count bounds first
        // todo: Not executed when *args-catchall is implemented
        if args_len > self.signature.len() {
            return Err(match self.signature.len() {
                0 => format!(
                    "{}() doesn't accept any arguments ({} given)",
                    self.name, args_len
                ),
                1 => format!(
                    "{}() takes exactly one argument ({} given)",
                    self.name, args_len
                ),
                _ => format!(
                    "{}() expected at most {} arguments ({} given)",
                    self.name,
                    self.signature.len(),
                    args_len
                ),
            }
            .into())
            .into();
        }

        if main {
            for global in &self.signature {
                if let Some(addr) = global.1 {
                    thread.globals.push(thread.program.statics[addr].clone())
                } else {
                    thread.globals.push(crate::value!(void));
                }
            }

            // Initialize remaining global variables
            thread
                .globals
                .resize_with(self.locals, || crate::value!(void));
        } else {
            // Initialize local variables
            args.resize(self.locals, Capture::Empty);

            // Set remaining parameters to their defaults
            for (i, arg) in (&self.signature[args_len..]).iter().enumerate() {
                // args parameters are previously pushed onto the stack.
                let var = &mut args[args_len + i];

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
                        *var = Capture::Value(thread.program.statics[addr].clone(), None, 0);
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
        }

        // Create a new conrext
        let mut context = Context::new(thread, self, depth, args);

        //println!("remaining {:?}", nargs);
        let reader_start = context.frame0().reader_start;

        // Perform left-recursive execution
        let result = if let Some(true) = self.consuming {
            /*
            println!(
                "--- {} @ {} ---",
                self.name.as_deref().unwrap_or("(unnamed)"),
                reader_start.offset
            );
            */

            // Left-recursive parselets are called in a loop until no more input is consumed.
            let mut reader_end = reader_start;
            let mut result = Err(Reject::Next);

            // Insert a fake memo entry to avoid endless recursion
            context
                .thread
                .memo
                .insert((reader_start.offset, id), (reader_end, result.clone()));

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

                let loop_end = context.thread.reader.tell();

                // Stop when no more input was consumed
                if loop_end.offset <= reader_end.offset {
                    break;
                }

                result = loop_result;
                reader_end = loop_end;

                // Save intermediate result in memo table
                context
                    .thread
                    .memo
                    .insert((reader_start.offset, id), (reader_end, result.clone()));

                // Reset reader & stack
                context.thread.reader.reset(reader_start);
                context.stack.clear();
                context
                    .stack
                    .resize(context.frame0().capture_start, Capture::Empty);
            }

            context.thread.reader.reset(reader_end);

            result
        } else {
            let result = context.run(main);

            if self.consuming.is_some() {
                context.thread.memo.insert(
                    (reader_start.offset, id),
                    (context.thread.reader.tell(), result.clone()),
                );
            }

            result
        };

        /*
        // Dump AST when parselet returns an AST for debugging purposes.
        // fixme: Disabled for now, can be enabled on demand.
        if context.thread.debug > 1 {
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
        context: Option<&mut Context>,
        args: Vec<RefValue>,
        nargs: Option<Dict>,
    ) -> Result<Accept, Reject> {
        match context {
            Some(context) => self.0.borrow().run(
                context.thread,
                args.into_iter()
                    .map(|arg| Capture::Value(arg, None, 0))
                    .collect(),
                nargs,
                false,
                context.depth + 1,
            ),
            None => panic!("{} needs a context to operate", self.repr()),
        }
    }

    fn call_direct(
        &self,
        context: &mut Context,
        args: usize,
        nargs: Option<Dict>,
    ) -> Result<Accept, Reject> {
        self.0.borrow().run(
            context.thread,
            context.stack.split_off(context.stack.len() - args),
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

#[cfg(feature = "serde")]
impl serde::Serialize for ParseletRef {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.borrow().serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for ParseletRef {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = Parselet::deserialize(deserializer)?;
        Ok(ParseletRef(Rc::new(RefCell::new(value))))
    }
}
