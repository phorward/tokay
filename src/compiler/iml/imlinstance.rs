//! Intermediate parselet instance
use super::*;
use crate::reader::Offset;
use crate::utils;
use indexmap::IndexMap;
use log;


#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::compiler) struct ImlInstance {
    pub offset: Option<Offset>,                              // Source offset
    pub target: Box<ImlValue>,                               // Instance target
    pub args: Vec<(Option<Offset>, ImlValue)>,               // Sequential generic args
    pub nargs: IndexMap<String, (Option<Offset>, ImlValue)>, // Named generic args
    pub severity: Option<u8>,                                // optional desired severity
    pub is_generated: bool, // flag for generated parselet (e.g. using modifier)
}

impl ImlInstance {
    pub fn derive(&self, program: &mut ImlProgram, current: (&ImlRefParselet, usize)) -> ImlValue {
        let mut instance = self.clone();  // FIXME: Temporary!

        if let ImlValue::Parselet(parselet) = &*instance.target {
            let parselet = parselet.borrow();
            let mut generics = IndexMap::new();

            // Map args and nargs to generics of this parselet
            for (name, default) in parselet.generics.iter() {
                // Take arguments by sequence first
                let arg = if !instance.args.is_empty() {
                    let arg = instance.args.remove(0);
                    (arg.0, Some(arg.1))
                }
                // Otherwise, take named arguments
                else if let Some(narg) = instance.nargs.shift_remove(name) {
                    (narg.0, Some(narg.1))
                }
                // Otherwise, use default
                else {
                    (instance.offset.clone(), default.clone())
                };

                // Check integrity of constant names
                if let (offset, Some(value)) = &arg {
                    if value.is_consuming() {
                        if !utils::identifier_is_consumable(name) {
                            program.push_error(
                                *offset,
                                format!(
                                    "Cannot assign consumable {} to non-consumable generic '{}'",
                                    value, name
                                )
                            );
                        }
                    } else if utils::identifier_is_consumable(name) {
                        program.push_error(
                            *offset,
                            format!(
                                "Cannot assign non-consumable {} to consumable generic {} of {}",
                                value, name, parselet
                            )
                        );
                    }
                } else {
                    program.push_error(
                        arg.0,
                        format!("Expecting argument for generic '{}'", name),
                    );
                }

                generics.insert(name.clone(), arg.1);
            }

            // Report any errors for remaining generic arguments.
            if !instance.args.is_empty() {
                program.push_error(
                    instance.args[0].0, // report first parameter
                    format!(
                        "{} got too many generic arguments ({} given, {} expected)",
                        instance.target,
                        generics.len() + instance.args.len(),
                        generics.len()
                    ),
                );
            }

            for (name, (offset, _)) in instance.nargs {
                if generics.get(&name).is_some() {
                    program.push_error(
                        offset,
                        format!("{} already got generic argument '{}'", instance.target, name),
                    );
                } else {
                    program.push_error(
                        offset,
                        format!(
                            "{} does not accept generic argument named '{}'",
                            instance.target, name
                        ),
                    );
                }
            }

            log::trace!("creating instance from {}", parselet);
            for (k, v) in &generics {
                log::trace!("  {} => {:?}", k, v);
            }

            // Make a parselet instance from the instance definition;
            // This can be the final parselet instance, but constants
            // might contain generic references as well, which are being
            // resolved during further compilation and derivation.
            let parselet = ImlRefParselet::new(ImlParselet {
                model: parselet.model.clone(),
                generics,
                offset: instance.offset,
                name: parselet.name.clone(),
                severity: instance.severity.unwrap_or(parselet.severity),
                is_generated: instance.is_generated,
            });

            log::info!("instance {} created", parselet);

            return ImlValue::from(parselet);
        }

        unreachable!()
    }
}
