use super::*;
use crate::reader::Offset;
use crate::value;
use crate::value::{Dict, List, Object, RefValue, Str, Value};
use std::io;
use std::io::prelude::*;
use std::rc::Rc;

// --- UnaryOp -----------------------------------------------------------------

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub(crate) enum UnaryOp {
    Dec,
    Inc,
    Neg,
    Not,
}

impl UnaryOp {
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Dec => "idec",
            Self::Inc => "iinc",
            Self::Neg => "neg",
            Self::Not => "not",
        }
    }
}

// --- BinaryOp ----------------------------------------------------------------

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub(crate) enum BinaryOp {
    Add,
    Div,
    Eq,
    Gt,
    GtEq,
    InlineAdd,
    InlineDiv,
    InlineIntDiv,
    InlineMod,
    InlineMul,
    InlineSub,
    IntDiv,
    Lt,
    LtEq,
    Mod,
    Mul,
    Neq,
    Sub,
}

impl BinaryOp {
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Add => "add",
            Self::Div => "div",
            Self::Eq => "eq",
            Self::Gt => "gt",
            Self::GtEq => "gteq",
            Self::InlineAdd => "iadd",
            Self::InlineDiv => "idiv",
            Self::InlineIntDiv => "idivi",
            Self::InlineMod => "imod",
            Self::InlineMul => "imul",
            Self::InlineSub => "isub",
            Self::IntDiv => "divi",
            Self::Lt => "lt",
            Self::LtEq => "lteq",
            Self::Mod => "mod",
            Self::Mul => "mul",
            Self::Neq => "neq",
            Self::Sub => "sub",
        }
    }
}

// --- Op ----------------------------------------------------------------------

/**
Atomic operations.

Specifies all atomic level VM code operations to run the Tokay VM.
*/
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub(crate) enum Op {
    Nop,
    Offset(Box<Offset>), // Source offset position for debugging

    // Capture frames
    Frame(usize), // Start new frame with optional relative forward address fuse
    // Capture,      // Reset frame capture to current stack size, saving captures
    Extend,       // Extend frame's reader to current position
    Reset,        // Reset frame, stack+reader
    ResetReader,  // Reset reader
    ResetCapture, // Reset captures
    Close,        // Close frame
    Collect,      // Collect stack values from current frame
    // InCollect,    // Same as collect, but degrate the parselet level (5) (fixme: This is temporary!)
    Fuse(usize), // Set frame fuse to relative forward address

    // Loop frames
    Loop(usize), // Loop frame
    Break,       // Ok(Accept::Break)
    LoadBreak,   // Ok(Accept::Break) with value
    Continue,    // Ok(Accept::Continue)

    // Conditional jumps
    ForwardIfTrue(usize),     // Jump forward when TOS is true
    ForwardIfFalse(usize),    // Jump forward when TOS is false
    ForwardIfNotVoid(usize),  // Jump forward when TOS is not void
    ForwardIfConsumed(usize), // Jump forward when frame consumed input

    // Direct jumps
    Forward(usize), // Jump forward
    // Backward(usize), // Jump backward

    // Interrupts
    Push,       // Ok(Accept::Push)
    LoadPush,   // Ok(Accept::Push) with value
    Accept,     // Ok(Accept::Return)
    LoadAccept, // Ok(Accept::Return) with value
    Repeat,     // Ok(Accept::Repeat)
    Next,       // set state to Err(Reject::Next), continue
    Reject,     // hard return Err(Err::Reject)
    LoadExit,   // Exit with errorcode
    Exit,       // Exit with 0

    // Call
    CallOrCopy,          // Load and eventually call stack element without parameters
    Call,                // Call stack element without parameters
    CallArg(usize),      // Call stack element with sequential parameters
    CallArgNamed(usize), // Call stack element with sequential and named parameters
    CallStatic(usize),   // Call static element without parameters
    CallStaticArg(Box<(usize, usize)>), // Call static element with sequential parameters
    CallStaticArgNamed(Box<(usize, usize)>), // Call static element with sequential and named parameters

    // Constants
    LoadStatic(usize), // Push a constant from the statics
    Push0,             // Push Integer(0)
    Push1,             // Push Integer(1)
    PushVoid,          // Push Void
    PushNull,          // Push Null
    PushTrue,          // Push True
    PushFalse,         // Push False

    // Variables, Values and Captures
    LoadGlobal(usize),           // Load global variable
    LoadFast(usize),             // Load local variable by current context
    LoadFastCapture(usize),      // Load capture by known index
    LoadCapture,                 // Load capture by evaluated index
    LoadItem { upsert: bool },   // Load item
    LoadAttr,                    // Load attr
    StoreGlobal(usize),          // Store global variable
    StoreGlobalHold(usize),      // Store global variable and keep tos
    StoreFast(usize),            // Store local variable
    StoreFastHold(usize),        // Store local variable and keep tos
    StoreFastCapture(usize),     // Store capture with known index
    StoreFastCaptureHold(usize), // Store capture with known index and keep tos
    StoreCapture,                // Store capture with evaluated index
    StoreCaptureHold,            // Store capture with evaluated index and keep tos
    StoreItem,                   // Store item
    StoreItemHold,               // Store item and push item reference to tos

    MakeAlias,       // Make key-value-Capture from last two stack items
    MakeList(usize), // Make a List from specified amount of items on stack
    MakeDict(usize), // Make a Dict from specified amount of key-value-pairs on the stack

    // Operations
    Drop,        // drop TOS
    Inv,         // invalidate TOS to empty capture
    Sep,         // separate, ensure TOS value is not shared
    Dup,         // duplicate TOS (creates a new object)
    Copy(usize), // copy indexed element as TOS
    Swap(usize), // swap indexed element with TOS

    UnaryOp(UnaryOp),   // Operation with one operand
    BinaryOp(BinaryOp), // Operation with two operands
}

impl Op {
    /** Runs a sequence of Ops on a given Context.

    This function is the heart of the Tokay VM, and executes the individual instructions.

    There are several frames managed within the context, which represent sub-sequences and
    reader areas within the current thread.
    */
    pub(in crate::vm) fn run(ops: &[Op], context: &mut Context) -> Result<Accept, Reject> {
        if ops.len() == 0 {
            return Ok(Accept::Next);
        }

        assert!(context.frames.len() == 0);

        // ---------------------------------------------------------------------

        let mut ip = 0; // Instruction pointer
        let mut state = Ok(Accept::Next);

        while ip < ops.len() {
            let op = &ops[ip];

            // Debug
            if context.debug == 3 {
                context.log(&format!("{:03}:{:?}", ip, op));
            } else if context.debug > 3 {
                if context.debug > 5 {
                    // Skip any Nop-Operations
                    if matches!(op, Op::Nop | Op::Offset(_)) {
                        ip += 1;
                        continue;
                    }
                }

                // Dump entire code
                context.log("--- Code ---");

                fn dump(ops: &[Op], context: &Context, ip: usize) {
                    for (i, op) in ops.iter().enumerate() {
                        context.log(&format!(
                            "{}{:03} {:?}",
                            if i == ip { ">" } else { " " },
                            i,
                            op
                        ));
                    }
                }

                dump(ops, context, ip);

                // Dump stack and frames
                if context.debug > 4 {
                    context.log("--- Reader ---");
                    context.log(&format!(" offset={:?}", context.thread.reader.tell()));
                    context.log(&format!(" eof={:?}", context.thread.reader.eof));

                    context.log("--- Globals ---");
                    for i in 0..context.thread.globals.len() {
                        context.log(&format!(" {:03} {:?}", i, context.thread.globals[i]));
                    }

                    context.log("--- Stack ---");
                    for i in 0..context.stack.len() {
                        context.log(&format!(" {:03} {:?}", i, context.stack[i]));
                    }

                    context.log("--- Frames ---");
                    for i in 0..context.frames.len() {
                        context.log(&format!(" {:03} {}", i, context.frames[i]));
                    }

                    context.log(&format!(" {:03} {}", context.frames.len(), context.frame));
                }

                // Step-by-step
                if context.debug > 5 {
                    let _ = io::stdin().read(&mut [0u8]).unwrap();
                }
            }

            // Execute instruction
            state = match op {
                Op::Nop => Ok(Accept::Next),

                Op::Offset(offset) => {
                    context.source_offset = Some(**offset);
                    Ok(Accept::Next)
                }

                // Frames
                Op::Frame(fuse) => {
                    context.frames.push(context.frame);
                    context.frame = Frame {
                        fuse: if *fuse > 0 { Some(ip + *fuse) } else { None },
                        capture_start: context.stack.len(),
                        reader_start: context.thread.reader.tell(),
                    };

                    Ok(Accept::Next)
                }

                /*
                Op::Capture => {
                    context.frame.capture_start = context.stack.len();
                    Ok(Accept::Next)
                }
                */
                Op::Extend => {
                    context.frame.reader_start = context.thread.reader.tell();
                    Ok(Accept::Next)
                }

                Op::Reset => {
                    context.stack.truncate(context.frame.capture_start);
                    context.thread.reader.reset(context.frame.reader_start);
                    Ok(Accept::Next)
                }

                Op::ResetReader => {
                    context.thread.reader.reset(context.frame.reader_start);
                    Ok(Accept::Next)
                }

                Op::ResetCapture => {
                    context.stack.truncate(context.frame.capture_start);
                    Ok(Accept::Next)
                }

                Op::Close => {
                    context.frame = context.frames.pop().unwrap();
                    Ok(Accept::Next)
                }

                Op::Collect => Ok(Accept::Push(context.collect(
                    context.frame.capture_start,
                    false,
                    true,
                    context.thread.debug > 5,
                ))),

                /*
                Op::InCollect => {
                    let mut capture =
                        context.collect(context.frame.capture_start, false, context.debug > 5);

                    if capture.get_severity() > 5 {
                        capture.set_severity(5);
                    }

                    Ok(Accept::Push(capture))
                }
                */
                Op::Fuse(addr) => {
                    context.frame.fuse = Some(ip + *addr);
                    Ok(Accept::Next)
                }

                // Loops
                Op::Loop(size) => {
                    context.loops.push(Loop {
                        frames: context.frames.len(),
                        start: ip + 1,
                        end: ip + *size,
                    });
                    Ok(Accept::Next)
                }

                Op::Break | Op::LoadBreak => {
                    let current = context.loops.pop().unwrap();

                    // Save value?
                    let value = if matches!(op, Op::LoadBreak) {
                        Some(context.pop())
                    } else {
                        None
                    };

                    // Discard all open frames inside current loop.
                    while context.frames.len() > current.frames {
                        context.frame = context.frames.pop().unwrap();
                    }

                    context.stack.truncate(context.frame.capture_start);

                    // Jump behind loop
                    ip = current.end;

                    // Break will always leave a value, either defined or empty capture
                    Ok(if let Some(value) = value {
                        Accept::Push(Capture::Value(value, None, 10))
                    } else {
                        context.stack.push(Capture::Empty);
                        Accept::Hold
                    })
                }

                Op::Continue => {
                    let current = context
                        .loops
                        .last()
                        .expect("Op::Continue used outside of a loop frame");

                    // Discard all open frames inside current loop.
                    while context.frames.len() > current.frames {
                        context.frame = context.frames.pop().unwrap();
                    }

                    context.stack.truncate(context.frame.capture_start);

                    // Jump to loop start.
                    ip = current.start;

                    Ok(Accept::Hold)
                }

                // Conditional jumps
                Op::ForwardIfTrue(goto) => {
                    if context.pop().is_true() {
                        ip += goto;
                    } else {
                        ip += 1;
                    }

                    Ok(Accept::Hold)
                }

                Op::ForwardIfFalse(goto) => {
                    if !context.pop().is_true() {
                        ip += goto;
                    } else {
                        ip += 1;
                    }

                    Ok(Accept::Hold)
                }

                Op::ForwardIfNotVoid(goto) => {
                    if !context.pop().is_void() {
                        ip += goto;
                    } else {
                        ip += 1;
                    }

                    Ok(Accept::Hold)
                }

                Op::ForwardIfConsumed(goto) => {
                    if context.frame.reader_start != context.thread.reader.tell() {
                        ip += goto;
                        Ok(Accept::Hold)
                    } else {
                        Ok(Accept::Next)
                    }
                }

                Op::Forward(goto) => {
                    ip += goto;
                    Ok(Accept::Hold)
                }

                /*
                Op::Backward(goto) => {
                    ip -= goto;
                    Ok(Accept::Hold)
                }
                */
                // Interrupts
                Op::Push => Ok(Accept::Push(Capture::Empty)),
                Op::LoadPush => {
                    let value = context.pop();
                    Ok(Accept::Push(Capture::Value(value, None, 15))) // high severity for override required here
                }
                Op::Accept => Ok(Accept::Return(Capture::Empty)),
                Op::LoadAccept => Ok(Accept::Return(context.stack.pop().unwrap())),
                Op::Repeat => Ok(Accept::Repeat),
                Op::Next => Err(Reject::Next),
                Op::Reject => {
                    state = Err(Reject::Next);
                    break;
                }
                Op::LoadExit => {
                    std::process::exit(context.pop().to_i64()? as i32);
                }
                Op::Exit => std::process::exit(0),

                // Calls
                Op::CallOrCopy => {
                    let value = context.pop();

                    if false && context.debug > 3 {
                        println!(
                            "CallOrCopy is_callable={:?} is_mutable={:?}",
                            value.is_callable(true),
                            value.is_mutable()
                        )
                    }

                    if value.is_callable(true) {
                        // Call the value without parameters
                        value.call_direct(context, 0, None)
                    } else if value.is_mutable() {
                        // Push a reference to the value
                        context.push(value)
                    } else {
                        // Push a copy of the value
                        context.push(value.borrow().clone().into())
                    }
                }

                Op::Call => {
                    let target = context.pop();
                    target.call_direct(context, 0, None)
                }

                Op::CallArg(args) => {
                    let target = context.pop();
                    target.call_direct(context, *args, None)
                }

                Op::CallArgNamed(args) => {
                    let target = context.pop();
                    let nargs = Value::from(context.pop());

                    if let Some(nargs) = nargs.into_object::<Dict>() {
                        target.call_direct(context, *args, Some(nargs))
                    } else {
                        panic!("nargs operand required to be dict")
                    }
                }

                Op::CallStatic(addr) => {
                    context.thread.program.statics[*addr].call_direct(context, 0, None)
                }

                Op::CallStaticArg(addr_args) => {
                    context.thread.program.statics[addr_args.0].call_direct(
                        context,
                        addr_args.1,
                        None,
                    )
                    //println!("CallStaticArg returns {:?}", ret);
                }

                Op::CallStaticArgNamed(addr_args) => {
                    let nargs = Value::from(context.pop());

                    if let Some(nargs) = nargs.into_object::<Dict>() {
                        context.thread.program.statics[addr_args.0].call_direct(
                            context,
                            addr_args.1,
                            Some(nargs),
                        )
                    } else {
                        panic!("nargs operand required to be dict")
                    }
                }

                // Variables and values
                Op::LoadStatic(addr) => {
                    let value = &context.thread.program.statics[*addr];
                    context.push(value.borrow().clone().into())
                }
                Op::Push0 => context.push(value!(0i64)),
                Op::Push1 => context.push(value!(1i64)),
                Op::PushVoid => context.push(value!(void)),
                Op::PushNull => context.push(value!(null)),
                Op::PushTrue => context.push(value!(true)),
                Op::PushFalse => context.push(value!(false)),

                Op::LoadGlobal(addr) => context.push(context.thread.globals[*addr].clone()),
                Op::LoadFast(addr) => context.load(*addr),

                Op::LoadFastCapture(index) => {
                    let mut capture = context.get_capture(*index).unwrap_or(Capture::Empty);

                    capture.set_severity(10);
                    context.stack.push(capture);

                    Ok(Accept::Next)
                }

                Op::LoadCapture => {
                    let index = context.pop();
                    let index = index.borrow();

                    let mut capture = if let Some(alias) = index.object::<Str>() {
                        context
                            .get_capture_by_name(alias.as_str())
                            .unwrap_or(Capture::Empty)
                    } else {
                        context
                            .get_capture(index.to_usize()?)
                            .unwrap_or(Capture::Empty)
                    };

                    capture.set_severity(10);
                    context.stack.push(capture);

                    Ok(Accept::Next)
                }

                Op::LoadItem { upsert } => {
                    let item = context.pop();
                    let object = context.pop();
                    let upsert = if *upsert {
                        let mut dict = Dict::new();
                        dict.insert(RefValue::from("upsert"), RefValue::from(true));
                        Some(dict)
                    } else {
                        None
                    };

                    match object.call_method("get_item", Some(context), vec![item], upsert) {
                        Ok(Some(value)) => context.push(value),
                        Ok(None) => Ok(Accept::Next),
                        Err(msg) => Err(Reject::from(msg)),
                    }
                }

                Op::LoadAttr => {
                    let attr = context.pop();
                    let attr = attr.borrow();
                    let value = context.pop();

                    match value.create_method(attr.object::<Str>().unwrap().as_str()) {
                        Ok(value) => context.push(value),
                        Err(err) => err.into(),
                    }
                }

                Op::StoreGlobal(addr) => {
                    // todo: bounds checking?
                    let value = context.pop().ref_or_copy();
                    context.thread.globals[*addr] = value;
                    Ok(Accept::Push(Capture::Empty))
                }

                Op::StoreGlobalHold(addr) => {
                    // todo: bounds checking?
                    let value = context.peek().ref_or_copy();
                    context.thread.globals[*addr] = value;
                    Ok(Accept::Next)
                }

                Op::StoreFast(addr) => {
                    // todo: bounds checking?
                    let value = context.pop().ref_or_copy();
                    context.stack[*addr] = Capture::Value(value, None, 0);
                    Ok(Accept::Push(Capture::Empty))
                }

                Op::StoreFastHold(addr) => {
                    // todo: bounds checking?
                    let value = context.peek().ref_or_copy();
                    context.stack[*addr] = Capture::Value(value, None, 0);
                    Ok(Accept::Next)
                }

                Op::StoreFastCapture(index) => {
                    let value = context.pop().ref_or_copy();

                    context.set_capture(*index, value);
                    Ok(Accept::Push(Capture::Empty))
                }

                Op::StoreFastCaptureHold(index) => {
                    let value = context.peek().ref_or_copy();

                    context.set_capture(*index, value);
                    Ok(Accept::Next)
                }

                Op::StoreCapture | Op::StoreCaptureHold => {
                    let index = context.pop();
                    let index = index.borrow();

                    if let Some(alias) = index.object::<Str>() {
                        if matches!(op, Op::StoreCapture) {
                            let value = context.pop().ref_or_copy();
                            context.set_capture_by_name(alias, value);
                            Ok(Accept::Push(Capture::Empty))
                        } else {
                            let value = context.peek().ref_or_copy();
                            context.set_capture_by_name(alias, value);
                            Ok(Accept::Next)
                        }
                    } else {
                        if matches!(op, Op::StoreCapture) {
                            let value = context.pop().ref_or_copy();
                            context.set_capture(index.to_usize()?, value);
                            Ok(Accept::Push(Capture::Empty))
                        } else {
                            let value = context.peek().ref_or_copy();
                            context.set_capture(index.to_usize()?, value);
                            Ok(Accept::Next)
                        }
                    }
                }

                Op::StoreItem | Op::StoreItemHold => {
                    let item = context.pop();
                    let object = context.pop();
                    let value = context.pop();

                    match object.call_method("set_item", Some(context), vec![item, value], None) {
                        Ok(value) => {
                            let value = value.unwrap(); // setitem must always return a value!

                            if matches!(op, Op::StoreItemHold) {
                                context.push(value)
                            } else {
                                Ok(Accept::Next)
                            }
                        }
                        Err(msg) => Err(Reject::from(msg)),
                    }
                }

                Op::MakeAlias => {
                    let name = context.pop();

                    match context.stack.last_mut().unwrap() {
                        Capture::Range(_, alias, ..) | Capture::Value(_, alias, ..) => {
                            *alias = Some(name);
                        }

                        empty => {
                            *empty = Capture::Value(value!(null), Some(name), 10);
                        }
                    }

                    Ok(Accept::Next)
                }

                Op::MakeList(count) => {
                    let mut list = List::with_capacity(*count);

                    for mut value in context.stack.drain(context.stack.len() - *count..) {
                        let value = value.extract(&mut context.thread.reader);

                        if !value.is_void() {
                            list.push(value);
                        }
                    }

                    context.push(RefValue::from(list))
                }

                Op::MakeDict(count) => {
                    let mut dict = Dict::new();

                    {
                        let start = context.stack.len() - *count * 2;
                        let mut items = context.stack.drain(start..);

                        while let (Some(mut value), Some(mut key)) = (items.next(), items.next()) {
                            let key = key.extract(&mut context.thread.reader);
                            let value = value.extract(&mut context.thread.reader);

                            dict.insert(key, value);
                        }
                    }

                    context.push(RefValue::from(dict))
                }

                // Operations
                Op::Drop => {
                    context.pop();
                    Ok(Accept::Next)
                }

                Op::Inv => {
                    context.pop();
                    Ok(Accept::Push(Capture::Empty))
                }

                Op::Sep => {
                    let mut value = context.pop();

                    // fixme: Replace by https://doc.rust-lang.org/std/rc/struct.Rc.html#method.unwrap_or_clone ?
                    if Rc::strong_count(&value) > 1 {
                        value = RefValue::from({
                            let inner = value.borrow();
                            inner.clone()
                        });
                    }

                    context.push(value)
                }

                Op::Dup => {
                    let value = context.peek();
                    let value = value.borrow();
                    context.push(value.clone().into())
                }

                Op::Copy(index) => {
                    assert!(*index > 0);

                    let index = context.stack.len() - index;
                    context.stack.push(context.stack[index].clone());

                    Ok(Accept::Next)
                }

                Op::Swap(index) => {
                    assert!(*index > 1);

                    let index = context.stack.len() - index;
                    let tos = context.stack.pop().unwrap();

                    context.stack.push(context.stack[index].clone());
                    context.stack[index] = tos;

                    Ok(Accept::Next)
                }

                Op::UnaryOp(op) => {
                    let value = context.pop();
                    context.push(value.unary_op(op.to_str())?)
                }

                Op::BinaryOp(op) => {
                    let last = context.pop();
                    let first = context.pop();
                    context.push(first.binary_op(last, op.to_str())?)
                }
            };

            // Debug
            if context.debug > 3 {
                context.log(&format!("ip = {} state = {:?}", ip, state));
            }

            match state {
                Ok(Accept::Hold) => state = Ok(Accept::Next),
                Ok(Accept::Next) => ip += 1,
                Ok(Accept::Push(capture)) if ip + 1 < ops.len() => {
                    context.stack.push(capture);
                    state = Ok(Accept::Next);
                    ip += 1;
                }
                Err(Reject::Next) if context.frames.len() > 0 => loop {
                    context.stack.truncate(context.frame.capture_start);
                    context.thread.reader.reset(context.frame.reader_start);

                    if let Some(fuse) = context.frame.fuse {
                        if fuse > ip {
                            ip = fuse;
                            break;
                        }
                    }

                    if context.frames.len() == 0 {
                        return Err(Reject::Next);
                    }

                    context.frame = context.frames.pop().unwrap();
                },
                _ => break,
            }
        }

        // Clear all frames, except base frame
        if !context.frames.is_empty() {
            context.frames.truncate(1);
            context.frame = context.frames.pop().unwrap();
        }

        if context.debug > 3 {
            context.log(&format!("exit state = {:?}", state));
        }

        state
    }
}
