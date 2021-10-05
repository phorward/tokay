use super::*;
use crate::value::RefValue;

/** Alternation construct.

The alternation construct defines either an alternation of sequences or a grouped sequence
of instructions. An alternation is only performed when input is consumed, otherwise the
alternation works similar to a sequence of sequences.
*/

#[derive(Debug)]
pub struct Alternation {
    items: Vec<ImlOp>,
}

impl Alternation {
    pub fn new(items: Vec<ImlOp>) -> ImlOp {
        Self { items: items }.into_op()
    }
}

impl Runable for Alternation {
    fn run(&self, context: &mut Context) -> Result<Accept, Reject> {
        let mut result = Ok(Accept::Next);
        let reader_start = context.runtime.reader.tell();

        for alt in self.items.iter() {
            result = alt.run(context);

            // Generally break on anything which is not Next.
            if !matches!(&result, Ok(Accept::Next) | Err(Reject::Next)) {
                // Push only accepts when input was consumed, otherwise the
                // push value is just discarded, except for the last item
                // being executed.
                if let Ok(Accept::Push(_)) = result {
                    // No consuming, no breaking!
                    if reader_start == context.runtime.reader.tell() {
                        continue;
                    }
                }

                break;
            }
        }

        result
    }

    fn resolve(&mut self, usages: &mut Vec<Vec<ImlOp>>) {
        for alt in self.items.iter_mut() {
            alt.resolve(usages);
        }
    }

    fn finalize(
        &mut self,
        statics: &Vec<RefValue>,
        stack: &mut Vec<(usize, bool)>,
    ) -> Option<Consumable> {
        let mut leftrec = false;
        let mut nullable = false;
        let mut consumes = false;

        for alt in self.items.iter_mut() {
            if let Some(consumable) = alt.finalize(statics, stack) {
                leftrec |= consumable.leftrec;
                nullable |= consumable.nullable;
                consumes = true;
            }
        }

        if consumes {
            Some(Consumable { leftrec, nullable })
        } else {
            None
        }
    }

    fn compile(&self, parselet: &ImlParselet) -> Vec<Op> {
        let mut ret = Vec::new();
        let mut iter = self.items.iter();
        let mut jumps = Vec::new();

        while let Some(item) = iter.next() {
            let alt = item.compile(parselet);

            if iter.len() > 0 {
                ret.push(Op::Fuse(alt.len() + 4));
                ret.extend(alt);
                ret.extend(vec![Op::Consumed, Op::Nop, Op::Reset]);

                jumps.push(ret.len() - 2);

                let len = ret.len();
            } else {
                ret.extend(alt);
            }
        }

        while let Some(addr) = jumps.pop() {
            ret[addr] = Op::ForwardIfTrue(ret.len() - addr);
        }

        if self.items.len() > 1 {
            ret.insert(0, Op::Capture);
            ret.push(Op::Commit);
        }

        ret
    }
}

impl std::fmt::Display for Alternation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for item in &self.items {
            write!(f, "{} ", item)?;
        }
        write!(f, "}}")?;

        Ok(())
    }
}
