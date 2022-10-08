//! Parselet object represents a callable, user-defined function.
use super::{BoxedObject, Dict, List, Object, RefValue};
use crate::error::Error;
use crate::vm::*;
use serde::{Deserialize, Serialize};
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

#[derive(Debug, Serialize, Deserialize)]
pub struct Parselet {
    pub(crate) name: String, // Parselet's name from source (for debugging)
    pub(crate) consuming: Option<bool>, // Indicator for consuming & left-recursion
    pub(crate) severity: u8, // Capture push severity
    signature: Vec<(String, Option<usize>)>, // Argument signature with default arguments
    pub(crate) locals: usize, // Number of local variables present
    begin: Vec<Op>,          // Begin-operations
    end: Vec<Op>,            // End-operations
    body: Vec<Op>,           // Operations
}

impl Parselet {
    /// Creates a new parselet.
    pub fn new(
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

    fn _run(&self, context: &mut Context, main: bool) -> Result<Accept, Reject> {
        // Initialize parselet execution loop
        let mut first = self.begin.len() > 0;
        let mut results = List::new();
        let mut state = if self.begin.len() == 0 {
            None
        } else {
            Some(true)
        };

        // Debugging
        let mut debug = context.runtime.debug;
        if debug < 3 {
            if let Ok(inspect) = std::env::var("TOKAY_INSPECT") {
                for name in inspect.split(" ") {
                    if name == self.name {
                        debug = 6;
                        break;
                    }
                }
            }
        }

        let result = loop {
            let ops = match state {
                // begin
                Some(true) => &self.begin,

                // end
                Some(false) => &self.end,

                // default
                None => &self.body,
            };

            let mut result = Op::execute(ops, context, debug);

            // Either take $0 if it was set to a value, or
            // use any last remaining value as result,
            // if available
            if let Ok(Accept::Next) = result {
                let dollar0 = &mut context.runtime.stack[context.capture_start - 1];

                if let Capture::Value(value, ..) = dollar0 {
                    result = Ok(Accept::Push(value.clone().into()));
                } else if context.runtime.stack.len() > context.capture_start {
                    result = Ok(Accept::Push(context.runtime.stack.pop().unwrap()));
                }
            }

            // Debug
            if context.runtime.debug > 2 {
                context.debug(&format!("returns {:?}", state));
            }

            /*
                In case this is the main parselet, try matching main as much
                as possible. This is only the case when input is consumed.
            */
            if main {
                //println!("main result(1) = {:#?}", result);
                result = match result {
                    Ok(Accept::Next) => Ok(Accept::Repeat(None)),

                    Ok(Accept::Return(value)) => Ok(Accept::Repeat(value)),

                    Ok(Accept::Push(capture)) => Ok(Accept::Repeat(match capture {
                        Capture::Range(range, ..) => {
                            Some(RefValue::from(context.runtime.reader.get(&range)))
                        }
                        Capture::Value(value, ..) => Some(value),
                        _ => None,
                    })),
                    result => result,
                };
                //println!("main result(2) = {:#?}", result);
            }

            // if main {
            //     println!("state = {:?} result = {:?}", state, result);
            // }

            // Evaluate result of parselet loop.
            match result {
                Ok(accept) => {
                    match accept {
                        Accept::Hold => break Some(Ok(Accept::Next)),

                        Accept::Return(value) => {
                            if let Some(value) = value {
                                break Some(Ok(Accept::Push(Capture::Value(
                                    value,
                                    None,
                                    self.severity,
                                ))));
                            } else {
                                break Some(Ok(Accept::Push(Capture::Empty)));
                            }
                        }

                        Accept::Repeat(value) => {
                            if let Some(value) = value {
                                results.push(value);
                            }
                        }

                        Accept::Push(mut capture) if capture.get_severity() > self.severity => {
                            capture.set_severity(self.severity);
                            break Some(Ok(Accept::Push(capture)));
                        }

                        accept => {
                            if results.len() > 0 {
                                break None;
                            }

                            break Some(Ok(accept));
                        }
                    }

                    if main {
                        // In case no input was consumed in main loop, skip character
                        if state.is_none()
                            && context
                                .runtime
                                .reader
                                .capture_from(&context.reader_start)
                                .len()
                                == 0
                        {
                            context.runtime.reader.next();
                        }

                        // Update absolute start offset
                        context.runtime.start = context.runtime.reader.tell().offset;

                        // Clear input buffer
                        context.runtime.reader.commit();

                        // Clear memo table
                        context.runtime.memo.clear();
                    }
                }

                Err(reject) => {
                    match reject {
                        Reject::Skip => {
                            if main && state.is_none() {
                                continue;
                            }

                            break Some(Ok(Accept::Next));
                        }
                        Reject::Error(mut err) => {
                            // Patch source position on error, when no position already set
                            if let Some(source_offset) = context.source_offset {
                                err.patch_offset(source_offset);
                            }

                            break Some(Err(Reject::Error(err)));
                        }
                        Reject::Main if !main => break Some(Err(Reject::Main)),
                        _ => {}
                    }

                    // Skip character and reset reader start
                    if main && state.is_none() {
                        context.runtime.reader.next();
                        context.reader_start = context.runtime.reader.tell();
                    } else if results.len() > 0 && state.is_none() {
                        state = Some(false);
                        continue;
                    } else if state.is_none() {
                        break Some(Err(reject));
                    }
                }
            }

            if let Some(false) = state {
                break None;
            } else if !first && context.runtime.reader.eof() {
                state = Some(false);
            } else {
                state = None;
            }

            // Reset capture stack for loop repeat
            context.runtime.stack.truncate(context.capture_start - 1); // Truncate stack including $0
            context.runtime.stack.push(Capture::Empty); // re-initialize $0
            context.reader_start = context.runtime.reader.tell(); // Reset reader

            first = false;
        };

        match result {
            Some(result) if !matches!(result, Ok(Accept::Next)) => result,
            _ => {
                if results.len() > 1 {
                    Ok(Accept::Push(Capture::Value(
                        RefValue::from(results),
                        None,
                        self.severity,
                    )))
                } else if results.len() == 1 {
                    Ok(Accept::Push(Capture::Value(
                        results.pop().unwrap(),
                        None,
                        self.severity,
                    )))
                } else {
                    Ok(Accept::Push(Capture::Empty))
                }
            }
        }
    }

    /** Run parselet on a given runtime.

    The main-parameter defines if the parselet behaves like a main loop or
    like subsequent parselet. */
    pub fn run(
        &self,
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
            runtime,
            &self,
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
                        if let Some(value) = nargs.remove(&arg.0) {
                            *var = Capture::Value(value, None, 0);
                            continue;
                        }
                    }

                    // Otherwise, use default value if available.
                    if let Some(addr) = arg.1 {
                        // fixme: This might leak the immutable static value to something mutable...
                        *var =
                            Capture::Value(context.runtime.program.statics[addr].clone(), None, 0);
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
                        0 => format!("{}() doesn't accept named argument '{}'", self.name, name),
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
                context.reader_start.offset
            );
            */

            // Left-recursive parselets are called in a loop until no more input
            // is consumed.
            let mut reader_end = context.reader_start;
            let mut result = Err(Reject::Next);

            // Insert a fake memo entry to avoid endless recursion
            context.runtime.memo.insert(
                (context.reader_start.offset, id),
                (reader_end, result.clone()),
            );

            loop {
                let loop_result = self._run(&mut context, main);

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
                    (context.reader_start.offset, id),
                    (reader_end, result.clone()),
                );

                // Reset reader & stack
                context.runtime.reader.reset(context.reader_start);
                context.runtime.stack.truncate(context.stack_start);
                context
                    .runtime
                    .stack
                    .resize(context.capture_start, Capture::Empty);
            }

            context.runtime.reader.reset(reader_end);

            result
        } else {
            let result = self._run(&mut context, main);

            if !main && self.consuming.is_some() {
                context.runtime.memo.insert(
                    (context.reader_start.offset, id),
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

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ParseletRef(pub Rc<RefCell<Parselet>>);

impl Object for ParseletRef {
    fn id(&self) -> usize {
        &*self.0.borrow() as *const Parselet as usize
    }

    fn name(&self) -> &'static str {
        "parselet"
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
        self.0
            .borrow()
            .run(context.runtime, args, nargs, false, context.depth + 1)
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

#[test]
fn test_function_as_static_with_args() {
    assert_eq!(
        crate::run(
            "
            factorial : @x {
                if !x return 1
                x * factorial(x - 1)
            }

            factorial(4)
            ",
            ""
        ),
        Ok(Some(crate::value!(24)))
    );
}

#[test]
fn test_function_as_variable_with_args() {
    assert_eq!(
        crate::run(
            "
            factorial = @x {
                if !x return 1
                x * factorial(x - 1)
            }

            factorial(4)
            ",
            ""
        ),
        Ok(Some(crate::value!(24)))
    );
}

#[test]
fn test_parselet_call_error_reporting() {
    // Tests for calling single argument parselet
    for (call, msg) in [
        ("f()", "Line 2, column 1: f() expected argument 'x'"),
        (
            "f(1, 2)",
            "Line 2, column 1: f() takes exactly one argument (2 given)",
        ),
        (
            "f(1, y=2)",
            "Line 2, column 1: f() doesn't accept named argument 'y'",
        ),
    ] {
        let call = format!("f : @x {{ x * x }}\n{}", call);

        assert_eq!(crate::run(&call, ""), Err(msg.to_owned()));
    }

    // Tests for calling mutli-argument parselet with wrong arguments counts
    for (call, msg) in [
        ("foo()", "Line 2, column 1: Call to unresolved symbol 'foo'"),
        ("f()", "Line 2, column 1: f() expected argument 'a'"),
        (
            "f(1, 2, 3, 4)",
            "Line 2, column 1: f() expected at most 3 arguments (4 given)",
        ),
        (
            "f(c=10, d=3, e=10)",
            "Line 2, column 1: f() expected argument 'a'",
        ),
        (
            "f(1, c=10, d=3)",
            "Line 2, column 1: f() doesn't accept named argument 'd'",
        ),
        (
            "f(1, c=10, d=3, e=7)",
            "Line 2, column 1: f() doesn't accept named arguments (2 given)",
        ),
    ] {
        let call = format!("f : @a, b=2, c {{ a b c }}\n{}", call);

        assert_eq!(crate::run(&call, ""), Err(msg.to_owned()));
    }
}

#[test]
fn test_parselet_begin_end() {
    assert_eq!(
        crate::run(
            "
            begin { x = 0 1337 }
            end 1338

            P: @{ 'lol' x = x + 1 x }
            P",
            "lolalolaalolol"
        ),
        Ok(Some(crate::value!([1337, 1, 2, 3, 1338])))
    );

    assert_eq!(
        crate::run(
            "
            begin x = 1

            'lol' $1 * x x x = x + 1",
            "lolAlolBlol"
        ),
        Ok(Some(crate::value!([
            ["lol", 1],
            ["lollol", 2],
            ["lollollol", 3]
        ])))
    );

    // begin and end without any input
    assert_eq!(
        crate::run(
            "
            begin 1
            2 3 4
            end 5
            ",
            ""
        ),
        Ok(Some(crate::value!([1, [2, 3, 4], 5])))
    )
}

#[test]
fn test_parselet_leftrec() {
    assert_eq!(
        crate::run("P: @{ P? ''a'' }\nP", "aaaa"),
        Ok(Some(crate::value!([[["a", "a"], "a"], "a"])))
    );

    // todo: More examples here please!
}

#[test]
fn test_parselet_repeat() {
    assert_eq!(
        crate::run("P: @{ 'a' repeat $1 }\nP", "aaaa"),
        Ok(Some(crate::value!(["a", "a", "a", "a"])))
    );

    // todo: More examples here please!
}

#[test]
fn test_anonymous_parselet() {
    assert_eq!(crate::run("@{ Int }", "123"), Ok(Some(crate::value!(123))));

    // todo: More examples here please!
}
