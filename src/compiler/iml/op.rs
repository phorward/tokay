/*! Intermediate code representation. */
use super::*;

/*
    Todo / Ideas for this module

    - Usage is integrated into ImlOp
    - Compilable is integrated into ImlOp as full variation
      - Alternation, Sequence, If, Loop
      - Remove of expect, not, peek, repeat as these are replaced by (inline?) generics
*/

#[derive(Debug)]
pub enum ImlOp {
    Nop,
    Usage(usize),                      // (yet) unresolved usage
    Compileable(Box<dyn Compileable>), // Compileable item
    Ops(Vec<ImlOp>),                   // Sequence of ImlOps
    Op(Op),                            // VM Operation
}

impl ImlOp {
    pub fn from_vec(ops: Vec<ImlOp>) -> Self {
        match ops.len() {
            0 => ImlOp::Nop,
            1 => ops.into_iter().next().unwrap(),
            _ => ImlOp::Ops(ops),
        }
    }

    pub fn into_kleene(self) -> Self {
        ImlRepeat::kleene(self)
    }

    pub fn into_positive(self) -> Self {
        ImlRepeat::positive(self)
    }

    pub fn into_optional(self) -> Self {
        ImlRepeat::optional(self)
    }
}

impl Compileable for ImlOp {
    fn compile(&self, parselet: &ImlParselet) -> Vec<Op> {
        match self {
            ImlOp::Nop => Vec::new(),
            ImlOp::Usage(_) => panic!("Cannot compile ImlOp::Usage"),
            ImlOp::Compileable(compileable) => compileable.compile(parselet),
            ImlOp::Ops(ops) => {
                let mut ret = Vec::new();

                for op in ops.into_iter() {
                    ret.extend(op.compile(parselet));
                }

                ret
            }
            ImlOp::Op(op) => vec![op.clone()],
        }
    }

    fn resolve(&mut self, usages: &mut Vec<Vec<ImlOp>>) {
        match self {
            ImlOp::Usage(usage) => *self = Self::from_vec(usages[*usage].drain(..).collect()),
            ImlOp::Compileable(compileable) => compileable.resolve(usages),
            ImlOp::Ops(ops) => ops.iter_mut().map(|op| op.resolve(usages)).collect(),
            _ => {}
        }
    }

    fn finalize(
        &mut self,
        values: &Vec<ImlValue>,
        stack: &mut Vec<(usize, bool)>,
    ) -> Option<Consumable> {
        match self {
            ImlOp::Compileable(runable) => runable.finalize(values, stack),
            ImlOp::Ops(ops) => {
                let mut ret: Option<Consumable> = None;

                for op in ops.iter_mut() {
                    if let Some(part) = op.finalize(values, stack) {
                        ret = if let Some(ret) = ret {
                            Some(Consumable {
                                leftrec: ret.leftrec || part.leftrec,
                                nullable: ret.nullable || part.nullable,
                            })
                        } else {
                            Some(part)
                        }
                    }
                }

                ret
            }
            ImlOp::Op(Op::CallStatic(target)) => {
                match &values[*target] {
                    ImlValue::Parselet(parselet) => {
                        if stack.len() > 0 {
                            if let Ok(mut parselet) = parselet.try_borrow_mut() {
                                if parselet.consuming.is_none() {
                                    return None;
                                }

                                stack.push((
                                    *target,
                                    if let Some(consuming) = parselet.consuming.as_ref() {
                                        consuming.nullable
                                    } else {
                                        false
                                    },
                                ));
                                let ret = parselet.finalize(values, stack);
                                stack.pop();

                                // --- Incomplete solution for the problem described in test/testindirectleftrec ---
                                // ImlIf left-recursion detected and called parselet is already
                                // left-recursive, thread currently analyzed parselet as
                                // not left-recursive here!
                                /*
                                if ret.0 && parselet.leftrec {
                                    ret.0 = false;
                                }
                                */

                                ret
                            } else {
                                for i in 0..stack.len() {
                                    if *target == stack[i].0 {
                                        return Some(Consumable {
                                            leftrec: i == 0,
                                            nullable: stack[i].1,
                                        });
                                    }
                                }

                                panic!("Can't find entry for {}", *target)
                            }
                        } else {
                            None
                        }
                    }

                    object => {
                        if object.is_consuming() {
                            Some(Consumable {
                                leftrec: false,
                                nullable: object.is_nullable(),
                            })
                        } else {
                            None
                        }
                    }
                }
            }

            _ => None,
        }
    }
}

impl std::fmt::Display for ImlOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImlOp::Compileable(p) => write!(f, "{}", p),
            op => write!(f, "Op {:?}", op),
        }
    }
}

impl From<Op> for ImlOp {
    fn from(op: Op) -> Self {
        ImlOp::Op(op)
    }
}
