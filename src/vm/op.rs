use super::*;
use crate::value::{Dict, RefValue, Value};

// --- Op ----------------------------------------------------------------------

/**
Atomic operations.

Specifies atomic level operations like running a parsable structure or running
VM code.
*/
#[derive(Debug)]
pub enum Op {
    Nop,
    Usage(usize), // (yet) unresolved usage

    // Parsing constructs
    Empty, // The empty word

    Scanable(Box<dyn Scanable>), // Scanable item
    Runable(Box<dyn Runable>),   // Runable item

    // Call
    TryCall,
    Call,
    CallStatic(usize),

    // Debuging and error reporting
    Print,               // todo: make this a builtin
    Debug(&'static str), // todo: make this a builtin
    Error(&'static str), // todo: make this a builtin

    // AST construction
    Create(&'static str), // todo: make this a builtin
    Lexeme(&'static str), // todo: make this a builtin

    // Interrupts
    Skip,
    LoadAccept,
    Reject,

    // Constants
    LoadStatic(usize),
    PushTrue,
    PushFalse,
    PushVoid,

    // Variables & Values
    LoadGlobal(usize),
    LoadFast(usize),
    StoreGlobal(usize),
    StoreFast(usize),
    LoadFastCapture(usize),
    LoadCapture,
    StoreFastCapture(usize),
    StoreCapture,

    // Operations
    Add,
    Sub,
    Div,
    Mul,
}

impl Op {
    pub fn from_vec(ops: Vec<Op>) -> Self {
        match ops.len() {
            0 => Op::Nop,
            1 => ops.into_iter().next().unwrap(),
            _ => Sequence::new(ops.into_iter().map(|item| (item, None)).collect()),
        }
    }

    pub fn into_box(self) -> Box<Self> {
        Box::new(self)
    }

    pub fn into_kleene(self) -> Self {
        Repeat::kleene(self)
    }

    pub fn into_positive(self) -> Self {
        Repeat::positive(self)
    }

    pub fn into_optional(self) -> Self {
        Repeat::optional(self)
    }

    /*
        Utility function to replace an Op during tranformation
        either by another Op or by a sequence.
    */
    pub(super) fn replace_usage(&mut self, usages: &mut Vec<Vec<Op>>) {
        if let Op::Usage(usage) = self {
            *self = Self::from_vec(usages[*usage].drain(..).collect())
        }
    }
}

impl Runable for Op {
    fn run(&self, context: &mut Context) -> Result<Accept, Reject> {
        //println!("RUN {:?}", self);

        match self {
            Op::Nop => Ok(Accept::Next),

            Op::Usage(_) => panic!(
                "{:?} can't be run; Trying to run an unresolved program?",
                self
            ),

            Op::Empty => Ok(Accept::Push(Capture::Empty)),

            Op::Scanable(scanable) => scanable.scan(&mut context.runtime.reader),
            Op::Runable(runable) => runable.run(context),

            Op::TryCall => {
                let value = context.pop();
                if value.borrow().is_callable() {
                    value.borrow().call(context)
                } else {
                    Ok(Accept::Push(Capture::Value(value.clone(), 10)))
                }
            }

            Op::Call => {
                let value = context.pop();
                let value = value.borrow();
                value.call(context)
            }

            Op::CallStatic(addr) => context.runtime.program.statics[*addr]
                .borrow()
                .call(context),

            Op::Print => {
                let value = context.collect(context.capture_start, true, false, 0);

                if value.is_some() {
                    println!("{:?}", value.unwrap());
                }

                Ok(Accept::Next)
            }

            Op::Debug(s) => {
                println!("{}", s);
                Ok(Accept::Next)
            }

            Op::Error(s) => Err(Reject::Error(s.to_string())),

            Op::Create(emit) => {
                /*
                println!("Create {} from {:?}",
                    emit, &context.runtime.stack[context.capture_start..]
                );
                */

                let value = match context.collect(context.capture_start, false, false, 0) {
                    Some(capture) => {
                        let value = capture.as_value(context.runtime);
                        let mut ret = Dict::new();

                        ret.insert(
                            "emit".to_string(),
                            Value::String(emit.to_string()).into_ref(),
                        );

                        // List or Dict values are classified as child nodes
                        if value.borrow().get_list().is_some()
                            || value.borrow().get_dict().is_some()
                        {
                            ret.insert("children".to_string(), value);
                        } else {
                            ret.insert("value".to_string(), value);
                        }

                        Value::Dict(Box::new(ret)).into_ref()
                    }
                    None => Value::String(emit.to_string()).into_ref(),
                };

                //println!("Create {} value = {:?}", emit, value);

                Ok(Accept::Return(Some(value)))
            }

            Op::Lexeme(emit) => {
                let value = Value::String(
                    context
                        .runtime
                        .reader
                        .extract(&context.runtime.reader.capture_from(&context.reader_start)),
                );

                let mut ret = Dict::new();

                ret.insert(
                    "emit".to_string(),
                    Value::String(emit.to_string()).into_ref(),
                );

                ret.insert("value".to_string(), value.into_ref());

                Ok(Accept::Return(Some(Value::Dict(Box::new(ret)).into_ref())))
            }

            Op::Skip => Ok(Accept::Skip),

            Op::LoadAccept => {
                let value = context.pop();
                Ok(Accept::Return(Some(value.clone())))
            }

            /*
            Op::Accept(value) => {
                Ok(Accept::Return(value.clone()))
            },

            Op::Repeat(value) => {
                Ok(Accept::Repeat(value.clone()))
            },
            */
            Op::Reject => Err(Reject::Return),

            Op::LoadStatic(addr) => Ok(Accept::Push(Capture::Value(
                context.runtime.program.statics[*addr].clone(),
                10,
            ))),

            Op::PushTrue => Ok(Accept::Push(Capture::Value(Value::True.into_ref(), 10))),
            Op::PushFalse => Ok(Accept::Push(Capture::Value(Value::False.into_ref(), 10))),
            Op::PushVoid => Ok(Accept::Push(Capture::Value(Value::Void.into_ref(), 10))),

            Op::LoadGlobal(addr) => Ok(Accept::Push(Capture::Value(
                context.runtime.stack[*addr].as_value(&context.runtime),
                10,
            ))),

            Op::LoadFast(addr) => Ok(Accept::Push(Capture::Value(
                context.runtime.stack[context.stack_start + *addr].as_value(&context.runtime),
                10,
            ))),

            Op::StoreGlobal(addr) => {
                // todo: bounds checking?
                context.runtime.stack[*addr] = Capture::Value(context.pop(), 10);

                Ok(Accept::Next)
            }

            Op::StoreFast(addr) => {
                // todo: bounds checking?
                context.runtime.stack[context.stack_start + *addr] =
                    Capture::Value(context.pop(), 10);

                Ok(Accept::Next)
            }

            Op::LoadFastCapture(index) => {
                let value = context
                    .get_capture(*index)
                    .unwrap_or(Value::Void.into_ref());

                Ok(Accept::Push(Capture::Value(value, 10)))
            }

            Op::LoadCapture => {
                let index = context.pop();
                let index = index.borrow();

                match *index {
                    Value::Addr(_) | Value::Integer(_) | Value::Float(_) => {
                        Op::LoadFastCapture(index.to_addr()).run(context)
                    }

                    _ => {
                        unimplemented!("//todo")
                    }
                }
            }

            Op::StoreFastCapture(index) => {
                let value = context.pop();

                context.set_capture(*index, value);
                Ok(Accept::Next)
            }

            Op::StoreCapture => {
                let index = context.pop();
                let index = index.borrow();

                match *index {
                    Value::Addr(_) | Value::Integer(_) | Value::Float(_) => {
                        Op::StoreFastCapture(index.to_addr()).run(context)
                    }

                    _ => {
                        unimplemented!("//todo")
                    }
                }
            }

            Op::Add | Op::Sub | Op::Div | Op::Mul => {
                let b = context.pop();
                let a = context.pop();

                /*
                println!("{:?}", self);
                println!("a = {:?}", a);
                println!("b = {:?}", b);
                */

                let c = match self {
                    Op::Add => (&*a.borrow() + &*b.borrow()).into_ref(),
                    Op::Sub => (&*a.borrow() - &*b.borrow()).into_ref(),
                    Op::Div => (&*a.borrow() / &*b.borrow()).into_ref(),
                    Op::Mul => (&*a.borrow() * &*b.borrow()).into_ref(),
                    _ => unimplemented!("Unimplemented operator"),
                };

                Ok(Accept::Push(Capture::Value(c, 10)))
            }
        }
    }

    fn finalize(
        &mut self,
        statics: &Vec<RefValue>,
        usages: &mut Vec<Vec<Op>>,
        leftrec: &mut bool,
        nullable: &mut bool,
    ) {
        match self {
            Op::Scanable(_) => {
                *nullable = false;
            }

            Op::Runable(runable) => {
                runable.finalize(statics, usages, leftrec, nullable);
            }

            Op::Usage(_) => self.replace_usage(usages),

            Op::CallStatic(addr) => {
                if let Value::Parselet(parselet) = &*statics[*addr].borrow() {
                    if let Ok(mut parselet) = parselet.try_borrow_mut() {
                        let mut call_leftrec = parselet.leftrec;
                        let mut call_nullable = parselet.nullable;

                        parselet.body.finalize(
                            statics,
                            usages,
                            &mut call_leftrec,
                            &mut call_nullable,
                        );

                        parselet.leftrec = call_leftrec;
                        parselet.nullable = call_nullable;

                        *nullable = parselet.nullable;
                    } else {
                        *leftrec = true;
                    }
                }
            }

            _ => {}
        }
    }
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Scanable(s) => write!(f, "{}", s),
            Op::Runable(p) => write!(f, "{}", p),
            op => write!(f, "Op {:?}", op),
        }
    }
}

// --- Rust --------------------------------------------------------------------

/*
/** This is allows to run any Rust code in position as Parsable. */
pub struct Rust(pub fn(&mut Context) -> Result<Accept, Reject>);

impl Parsable for Rust {
    fn run(&self, context: &mut Context) -> Result<Accept, Reject> {
        self.0(context)
    }
}

impl std::fmt::Debug for Rust {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{rust-function}}")
    }
}

impl std::fmt::Display for Rust {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{rust-function}}")
    }
}
*/