//! ImlProgram glues ImlParselet, ImlOp and ImlValue together to produce a VM program.

use super::*;
use crate::value::Parselet;
use crate::vm::Program;
use crate::Error;
use crate::{Object, RefValue};
use indexmap::{indexmap, IndexMap, IndexSet};
use log;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub(in crate::compiler) struct ImlProgram {
    main: ImlValue,
    statics: IndexMap<ImlValue, Option<Parselet>>, // static values with optional final parselet replacement
    pub errors: Vec<Error>, // errors collected during finalization (at least these are unresolved symbols)
}

impl ImlProgram {
    pub fn new(main: ImlValue) -> Self {
        ImlProgram {
            main: main.clone(),
            statics: indexmap!(main => None),
            errors: Vec::new(),
        }
    }

    /** Registers an ImlValue in the ImlProgram's statics map and returns its index.

    Only resolved values can be registered.

    In case *value* already exists inside of the current statics, the existing index will be returned,
    otherwiese the value is cloned and put into the statics table. */
    pub fn register(&mut self, value: &ImlValue) -> usize {
        match value {
            ImlValue::Shared(value) => return self.register(&*value.borrow()),
            ImlValue::Parselet(_) | ImlValue::Value(_) => match self.statics.get_index_of(value) {
                None => return self.statics.insert_full(value.clone(), None).0,
                Some(idx) => return idx,
            },
            ImlValue::Variable { offset, name, .. } => self.errors.push(Error::new(
                offset.clone(),
                format!("Variable '{}' used in static context", name),
            )),
            ImlValue::Generic { offset, .. } | ImlValue::Instance { offset, .. } => {
                self.errors
                    .push(Error::new(offset.clone(), format!("Unresolved {}", value)));
            }
            _ => unreachable!(),
        }

        0
    }

    /** Turns the ImlProgram and its intermediate values into a final VM program ready for execution.

    The finalization is done according to a grammar's point of view, as this is one of Tokays core features.
    This closure algorithm runs until no more changes on any parselet configurations regarding left-recursive
    and nullable parselet detection occurs.
    */
    pub fn compile(mut self) -> Result<Program, Vec<Error>> {
        log::info!("compiling {}", self.main);

        let mut finalize = HashSet::new(); // list of consuming parselets required to be finalized

        // Loop until end of statics is reached
        let mut idx = 0;

        // self.statics grows inside of this while loop, therefore this condition.
        while idx < self.statics.len() {
            log::trace!(
                "idx = {: >3}, statics.len() = {: >3}",
                idx,
                self.statics.len()
            );

            // Pick only intermediate parselets, other static values are directly moved
            let parselet = match self.statics.get_index_mut(idx).unwrap() {
                (_, Some(_)) => unreachable!(), // may not exist!
                (ImlValue::Parselet(parselet), None) => parselet.clone(),
                _ => {
                    idx += 1;
                    continue;
                }
            };

            log::trace!("idx = {: >3}, parselet = {:?}", idx, parselet.borrow().name);

            // Memoize parselets required to be finalized (needs a general rework later...)
            if parselet.borrow().model.borrow().is_consuming {
                //fixme...
                finalize.insert(parselet.clone());
            }

            // Compile VM parselet from intermediate parselet
            // println!("...compiling {} {:?}", idx, parselet.name);
            *self.statics.get_index_mut(idx).unwrap().1 = Some(parselet.compile(&mut self, idx));

            idx += 1;
        }

        // Stop on any raised error
        if !self.errors.is_empty() {
            return Err(self.errors);
        }

        // Finalize parselets
        log::debug!("{} has {} parselets to finalize", self.main, finalize.len());

        for (i, parselet) in finalize.iter().enumerate() {
            log::trace!(" {: >3} => {:#?}", i, parselet);
        }

        let leftrec = self.finalize(finalize);

        // Assemble all statics to be transferred into a Program
        let statics: Vec<RefValue> = self
            .statics
            .into_iter()
            .map(|(iml, parselet)| {
                if let Some(mut parselet) = parselet {
                    if let ImlValue::Parselet(imlparselet) = iml {
                        parselet.consuming = leftrec
                            .get(&imlparselet)
                            .map_or(None, |leftrec| Some(*leftrec));

                        //println!("{:?} => {:?}", imlparselet.borrow().name, parselet.consuming);
                    }

                    RefValue::from(parselet)
                } else {
                    iml.unwrap()
                }
            })
            .collect();

        log::debug!("{} has {} statics compiled", self.main, statics.len());

        for (i, value) in statics.iter().enumerate() {
            log::trace!(" {: >3} : {:#?}", i, value);
        }

        Ok(Program::new(statics))
    }

    /** Internal function to finalize a program on a grammar's point of view.

    The finalization performs a closure algorithm on every parselet to detect

    - nullable parselets
    - left-recursive parselets

    until no more changes occur.

    It can only be run on a previously compiled program without any unresolved usages.
    */
    fn finalize(&mut self, parselets: HashSet<ImlParselet>) -> HashMap<ImlParselet, bool> {
        #[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
        struct Consumable {
            leftrec: bool,
            nullable: bool,
        }

        // Finalize ImlValue
        fn finalize_value(
            value: &ImlValue,
            current: &ImlParselet,
            visited: &mut IndexSet<ImlParselet>,
            configs: &mut HashMap<ImlParselet, Consumable>,
        ) -> Option<Consumable> {
            match value {
                ImlValue::Shared(value) => {
                    finalize_value(&*value.borrow(), current, visited, configs)
                }
                ImlValue::SelfToken => Some(Consumable {
                    leftrec: true,
                    nullable: false,
                }),
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
                ImlValue::Generic { name, .. } => {
                    // fixme: Is this still relevant???
                    finalize_value(
                        current.borrow().generics[name].as_ref().unwrap(),
                        current,
                        visited,
                        configs,
                    )
                }
                _ => None,
            }
        }

        // Finalize ImlOp
        fn finalize_op(
            op: &ImlOp,
            current: &ImlParselet,
            visited: &mut IndexSet<ImlParselet>,
            configs: &mut HashMap<ImlParselet, Consumable>,
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

        // Finalize ImlParselet
        fn finalize_parselet(
            current: &ImlParselet,
            visited: &mut IndexSet<ImlParselet>,
            configs: &mut HashMap<ImlParselet, Consumable>,
        ) -> Option<Consumable> {
            // ... only if it's generally flagged to be consuming.
            let parselet = current.borrow();
            let model = parselet.model.borrow();

            if !model.is_consuming {
                return None;
            }

            //println!("- {}{}", ".".repeat(visited.len()), current);

            if let Some(idx) = visited.get_index_of(current) {
                // When in visited, this is a recursion
                Some(Consumable {
                    // If the idx is 0, current is the seeked parselet, so it is left-recursive
                    leftrec: if idx == 0 && !current.borrow().is_generated {
                        configs.get_mut(current).unwrap().leftrec = true;
                        true
                    } else {
                        false
                    },
                    nullable: configs[current].nullable,
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
                    nullable: configs[current].nullable,
                })
            }
        }

        // Now, start the closure algorithm with left-recursive and nullable configurations for all parselets
        // put into the finalize list.
        let mut changes = true;
        let mut configs = parselets
            .iter()
            .map(|k| {
                (
                    k.clone(),
                    Consumable {
                        leftrec: false,
                        nullable: false,
                    },
                )
            })
            .collect();

        while changes {
            changes = false;

            for parselet in &parselets {
                let result = finalize_parselet(parselet, &mut IndexSet::new(), &mut configs);
                changes = result > configs.get(parselet).cloned();
            }
        }

        log::debug!(
            "{} has {} parselets finalized",
            self.statics.keys()[0],
            parselets.len()
        );

        for parselet in &parselets {
            log::trace!(
                " {} consuming={:?}",
                parselet.borrow().name.as_deref().unwrap_or("(unnamed)"),
                configs[&parselet]
            );
        }

        configs.into_iter().map(|(k, v)| (k, v.leftrec)).collect()
    }
}
