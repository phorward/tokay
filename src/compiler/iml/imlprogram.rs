//! ImlProgram glues ImlParselet, ImlOp and ImlValue together to produce a VM program.

use super::*;
use crate::Error;
use crate::reader::Offset;
use crate::value::ParseletRef;
use crate::vm::Program;
use crate::{Object, RefValue};
use indexmap::{IndexMap, IndexSet, indexmap, indexset};
use log;
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Debug)]
pub(in crate::compiler) struct ImlProgram {
    errors: Vec<Error>, // errors collected during compilation
    statics: IndexSet<Result<RefValue, usize>>,
    parselets: IndexMap<ImlRefParselet, usize>,
}

impl ImlProgram {
    pub fn new(main: ImlRefParselet) -> Self {
        ImlProgram {
            errors: Vec::new(),
            statics: indexset![Err(0)],
            parselets: indexmap![main => 0],
        }
    }

    /// Push an Error to the programs's error log, with given offset and msg.
    pub fn push_error(&mut self, offset: Option<Offset>, msg: String) {
        self.errors.push(Error::new(offset, msg))
    }

    /** Registers an ImlValue in the ImlProgram's statics map and returns its index.

    Only resolved values can be registered.

    In case *value* already exists inside of the current statics, the existing index will be returned,
    otherwiese the value is cloned and put into the statics table. */
    pub fn register(&mut self, value: &ImlValue) -> Result<usize, ()> {
        match value {
            ImlValue::Shared(value) => return self.register(&*value.borrow()),
            ImlValue::Parselet(parselet) => match self.parselets.get(parselet) {
                Some(idx) => Ok(*idx),
                None => {
                    let idx = self.statics.insert_full(Err(self.parselets.len() + 1)).0;
                    self.parselets.insert(parselet.clone(), idx);
                    Ok(idx)
                }
            },
            ImlValue::Value(value) => {
                let value = Ok(value.clone());

                match self.statics.get_index_of(&value) {
                    Some(idx) => Ok(idx),
                    None => Ok(self.statics.insert_full(value).0),
                }
            }
            ImlValue::Variable { offset, name, .. } => {
                self.errors.push(Error::new(
                    offset.clone(),
                    format!("Variable '{}' used in static context", name),
                ));
                Err(())
            }
            ImlValue::Generic { offset, .. } | ImlValue::Instance(ImlInstance { offset, .. }) => {
                self.errors
                    .push(Error::new(offset.clone(), format!("Unresolved {}", value)));
                Err(())
            }
            _ => unreachable!(),
        }
    }

    /** Turns the ImlProgram and its intermediate values into a final VM program ready for execution.

    The finalization is done according to a grammar's point of view, as this is one of Tokays core features.
    This closure algorithm runs until no more changes on any parselet configurations regarding left-recursive
    and nullable parselet detection occurs.
    */
    pub fn compile(mut self) -> Result<Program, Vec<Error>> {
        log::info!("compiling {}", self.parselets[0]);

        // Loop until end of parselets is reached
        let mut count = 0;

        // self.parselets grows inside of this while loop, therefore this condition.
        while count < self.parselets.len() {
            log::trace!(
                "count = {: >3}, parselets.len() = {: >3}",
                count,
                self.parselets.len()
            );

            let (parselet, idx) = self
                .parselets
                .get_index(count)
                .map(|(p, idx)| (p.clone(), *idx))
                .unwrap();
            log::trace!("idx = {: >3}, parselet = {:?}", idx, parselet.borrow().name);

            // Compile static VM parselet from intermediate parselet
            let compiled_parselet = parselet.compile(&mut self, idx);

            // Insert new parselet before placeholder...
            self.statics
                .insert_before(idx, Ok(RefValue::from(compiled_parselet)));
            // ...and remove the placeholder.
            self.statics.shift_remove_index(idx + 1);

            count += 1;
        }

        // Stop on any raised error
        if !self.errors.is_empty() {
            return Err(self.errors);
        }

        // Finalize parselets
        self.finalize();

        // Assemble all statics to be transferred into a Program
        let statics: Vec<RefValue> = self
            .statics
            .into_iter()
            .map(|value| value.unwrap())
            .collect();

        log::info!("{:?} has {} statics compiled", statics[0], statics.len());

        for (i, value) in statics.iter().enumerate() {
            log::trace!(" {: >3} : {:#?}", i, value);
        }

        Ok(Program::new(statics))
    }

    /** Internal function to finalize a program on a grammar's point of view.

    The finalization performs a closure algorithm on every parselet to detect

    - nullable parselets
    - left-recursive parselets

    until no more changes to these flag configurations occur.

    It can only be run on a previously compiled program without any unresolved usages.
    */
    fn finalize(&mut self) {
        #[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
        struct Consumable {
            leftrec: bool,
            nullable: bool,
        }

        // Finalize ImlValue
        fn finalize_value(
            value: &ImlValue,
            current: &ImlRefParselet,
            visited: &mut IndexSet<ImlRefParselet>,
            configs: &HashMap<ImlRefParselet, RefCell<Consumable>>,
        ) -> Option<Consumable> {
            match value {
                ImlValue::Shared(value) => {
                    finalize_value(&*value.borrow(), current, visited, configs)
                }
                ImlValue::SelfToken => {
                    configs[current].borrow_mut().leftrec = true;

                    Some(Consumable {
                        leftrec: true,
                        nullable: configs[current].borrow().nullable,
                    })
                }
                ImlValue::Parselet(parselet) => {
                    // Try to derive the parselet with current constants
                    let derived = parselet.derive(current).unwrap();

                    // The derived parselet's original must be in the configs!
                    let parselet = configs.get_key_value(&derived).unwrap().0.clone();

                    finalize_parselet(&parselet, visited, configs)
                }
                ImlValue::Value(callee) => {
                    if callee.is_consuming() {
                        //println!("{:?} called, which is nullable={:?}", callee, callee.is_nullable());
                        Some(Consumable {
                            leftrec: false,
                            nullable: callee.is_nullable(),
                        })
                    } else {
                        None
                    }
                }
                ImlValue::Generic { name, .. } => finalize_value(
                    current.borrow().generics[name].as_ref().unwrap(),
                    current,
                    visited,
                    configs,
                ),
                _ => None,
            }
        }

        // Finalize ImlOp
        fn finalize_op(
            op: &ImlOp,
            current: &ImlRefParselet,
            visited: &mut IndexSet<ImlRefParselet>,
            configs: &HashMap<ImlRefParselet, RefCell<Consumable>>,
        ) -> Option<Consumable> {
            match op {
                ImlOp::Call { target, .. } => finalize_value(target, current, visited, configs),
                ImlOp::Alt { alts } => {
                    let mut leftrec = false;
                    let mut nullable = false;
                    let mut consumes = false;

                    for alt in alts {
                        if let Some(consumable) = finalize_op(alt, current, visited, configs) {
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
                ImlOp::Seq { seq, .. } => {
                    let mut leftrec = false;
                    let mut nullable = true;
                    let mut consumes = false;

                    for item in seq {
                        if !nullable {
                            break;
                        }

                        if let Some(consumable) = finalize_op(item, current, visited, configs) {
                            leftrec |= consumable.leftrec;
                            nullable = consumable.nullable;
                            consumes = true;
                        }
                    }

                    if consumes {
                        Some(Consumable { leftrec, nullable })
                    } else {
                        None
                    }
                }
                ImlOp::If { then, else_, .. } => {
                    let then = finalize_op(then, current, visited, configs);

                    if let Some(else_) = finalize_op(else_, current, visited, configs) {
                        if let Some(then) = then {
                            Some(Consumable {
                                leftrec: then.leftrec || else_.leftrec,
                                nullable: then.nullable || else_.nullable,
                            })
                        } else {
                            Some(else_)
                        }
                    } else {
                        then
                    }
                }
                ImlOp::Loop {
                    initial,
                    condition,
                    body,
                    ..
                } => {
                    let mut ret: Option<Consumable> = None;

                    for part in [initial, condition, body] {
                        let part = finalize_op(part, current, visited, configs);

                        if let Some(part) = part {
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

                // default case
                _ => None,
            }
        }

        // Finalize ImlRefParselet
        fn finalize_parselet(
            current: &ImlRefParselet,
            visited: &mut IndexSet<ImlRefParselet>,
            configs: &HashMap<ImlRefParselet, RefCell<Consumable>>,
        ) -> Option<Consumable> {
            // ... only if it's generally flagged to be consuming.
            let parselet = current.borrow();
            let model = parselet.model.borrow();

            //println!("- {}{}", ".".repeat(visited.len()), current);

            if let Some(idx) = visited.get_index_of(current) {
                // When in visited, this is a recursion
                Some(Consumable {
                    // If the idx is 0, current is the seeked parselet, so it is left-recursive
                    leftrec: if idx == 0 && !current.borrow().is_generated {
                        configs[current].borrow_mut().leftrec = true;
                        true
                    } else {
                        false
                    },
                    nullable: configs[current].borrow().nullable,
                })
            } else {
                // If not already visited, add and recurse.
                visited.insert(current.clone());

                for part in [&model.begin, &model.body, &model.end] {
                    finalize_op(part, current, visited, configs);
                }

                visited.shift_remove(current);

                Some(Consumable {
                    leftrec: false,
                    nullable: configs[current].borrow().nullable,
                })
            }
        }

        // Now, start the closure algorithm with left-recursive and nullable configurations
        // for all consumable parselets.
        let mut changes = true;
        let configs: HashMap<ImlRefParselet, RefCell<Consumable>> = self
            .parselets
            .keys()
            .filter(|k| k.borrow().model.borrow().is_consuming)
            .map(|k| {
                (
                    k.clone(),
                    RefCell::new(Consumable {
                        leftrec: false,
                        nullable: false,
                    }),
                )
            })
            .collect();

        log::info!(
            "{:?} has {} parselets to finalize",
            self.statics[0],
            configs.len()
        );

        for (i, parselet) in configs.keys().enumerate() {
            log::trace!(" {: >3} => {:#?}", i, parselet);
        }

        while changes {
            changes = false;

            for parselet in configs.keys() {
                if let Some(result) = finalize_parselet(parselet, &mut IndexSet::new(), &configs) {
                    changes = result > *configs[parselet].borrow();
                }
            }
        }

        // set left recursion flags
        for (parselet, config) in configs {
            // get compiled parselet from statics
            let parselet = self.statics[self.parselets[&parselet]].as_ref().unwrap();

            if let Some(parselet) = parselet.borrow().object::<ParseletRef>() {
                parselet.0.borrow_mut().consuming = Some(config.borrow().leftrec);
            }

            log::trace!(" {:?} consuming={:?}", parselet, config);
        }

        log::debug!(
            "{:?} has {} parselets finalized",
            self.statics[0],
            self.parselets.len()
        );
    }
}
