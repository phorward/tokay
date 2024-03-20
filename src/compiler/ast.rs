//! Compiler's internal Abstract Syntax Tree traversal
use indexmap::IndexMap;
use tokay_macros::tokay_function;
extern crate self as tokay;
use super::*;
use crate::builtin::Builtin;
use crate::reader::Offset;
use crate::utils;
use crate::value;
use crate::value::{Dict, List, Object, RefValue, Str, Token};
use crate::vm::*;
use charclass::CharClass;

pub static RESERVED_TOKENS: &[&'static str] = &[
    "Char", "Chars", "Empty", "EOF", "Expect", "Not", "Kle", "Opt", "Peek", "Pos", "Repeat",
    "Self", "Void",
];

pub static RESERVED_KEYWORDS: &[&'static str] = &[
    "accept", "begin", "break", "continue", "else", "end", "exit", "false", "for", "if", "in",
    "loop", "next", "null", "push", "reject", "repeat", "reset", "return", "self", "true", "void",
];

/// AST traversal entry
pub(in crate::compiler) fn traverse(scope: &Scope, ast: &RefValue) -> ImlOp {
    if let Some(list) = ast.borrow().object::<List>() {
        let mut ops = Vec::new();

        for item in list.iter() {
            ops.push(traverse(scope, item));
        }

        ImlOp::from(ops)
    } else if let Some(dict) = ast.borrow().object::<Dict>() {
        traverse_node_rvalue(scope, dict, Rvalue::CallOrLoad)
    } else {
        ImlOp::load(scope, None, ImlValue::from(RefValue::from(ast.clone())))
    }
}

// Extract offset positions into an Offset structure
fn traverse_node_offset(node: &Dict) -> Option<Offset> {
    //return None; // Temporarily discard any Offset information (shortens debug output)

    let offset = node
        .get_str("offset")
        .and_then(|offset| Some(offset.to_usize().unwrap()));
    let row = node
        .get_str("row")
        .and_then(|row| Some(row.to_usize().unwrap() as u32));
    let col = node
        .get_str("col")
        .and_then(|col| Some(col.to_usize().unwrap() as u32));

    if let (Some(offset), Some(row), Some(col)) = (offset, row, col) {
        Some(Offset { offset, row, col })
    } else {
        None
    }
}

// Append offset to ops
fn traverse_offset(node: &Dict) -> ImlOp {
    if let Some(offset) = traverse_node_offset(node) {
        ImlOp::from(Op::Offset(Box::new(offset)))
    } else {
        ImlOp::Nop
    }
}

// Traverse a value node into an ImlValue instance
fn traverse_node_value(scope: &Scope, node: &Dict, name: Option<String>) -> ImlValue {
    let emit = node["emit"].borrow();
    let emit = emit.object::<Str>().unwrap().as_str();

    // Generate a value from the given code
    match emit {
        // Literals
        "value_void" => ImlValue::Value(scope.compiler.statics.borrow()[0].clone()),
        "value_null" => ImlValue::Value(scope.compiler.statics.borrow()[1].clone()),
        "value_true" => ImlValue::Value(scope.compiler.statics.borrow()[2].clone()),
        "value_false" => ImlValue::Value(scope.compiler.statics.borrow()[3].clone()),
        "value_self" => ImlValue::SelfValue,
        "value_integer" => match node["value"].to_i64() {
            Ok(0) => ImlValue::Value(scope.compiler.statics.borrow()[4].clone()),
            Ok(1) => ImlValue::Value(scope.compiler.statics.borrow()[5].clone()),
            _ => scope.compiler.register_static(node["value"].clone()),
        },
        "value_float" => scope.compiler.register_static(node["value"].clone()),
        "value_string" => scope.compiler.register_static(node["value"].clone()),

        // Tokens
        "value_token_self" => ImlValue::SelfToken,
        "value_token_void" => ImlValue::VoidToken,
        "value_token_match" | "value_token_touch" => {
            let mut value = node["value"].to_string();

            if value.len() == 0 {
                scope.error(
                    traverse_node_offset(node),
                    format!("Empty match not allowed"),
                );
                value = "#INVALID".to_string();
            }

            scope
                .compiler
                .register_static(if emit == "value_token_match" {
                    RefValue::from(Token::Match(value))
                } else {
                    RefValue::from(Token::Touch(value))
                })
        }
        "value_token_any" => scope
            .compiler
            .register_static(RefValue::from(Token::Char(CharClass::new().negate()))),
        "value_token_anys" => scope
            .compiler
            .register_static(RefValue::from(Token::Chars(CharClass::new().negate()))),
        "value_token_ccl" | "value_token_ccls" => {
            let many = emit.ends_with("s");

            let node = node["children"].borrow();
            let node = node.object::<Dict>().unwrap();

            let emit = node["emit"].borrow();
            let emit = emit.object::<Str>().unwrap().as_str();

            let children = List::from(&node["children"]);

            let mut ccl = CharClass::new();

            for range in children.iter() {
                let range = range.borrow();
                let range = range.object::<Dict>().unwrap();

                let emit = range["emit"].borrow();
                let emit = emit.object::<Str>().unwrap().as_str();

                let value = range["value"].borrow();
                let value = value.object::<Str>().unwrap().as_str();

                match &emit[..] {
                    "char" => {
                        let ch = value.chars().next().unwrap();
                        ccl.add(ch..=ch);
                    }
                    "range" => {
                        let from = value.chars().nth(0).unwrap();
                        let to = value.chars().nth(1).unwrap();

                        ccl.add(from..=to);
                    }
                    _ => {
                        unreachable!();
                    }
                }
            }

            if emit == "ccl_neg" {
                ccl = ccl.negate();
            } else {
                assert!(emit == "ccl");
            }

            scope.compiler.register_static(if many {
                RefValue::from(Token::Chars(ccl))
            } else {
                RefValue::from(Token::Char(ccl))
            })
        }

        // Parselets
        "value_parselet" => {
            let mut generics = IndexMap::new();
            let mut signature = IndexMap::new();

            // Traverse the AST
            let mut sigs = List::from(node["children"].clone());
            let body = sigs.pop().unwrap();

            for node in sigs.into_iter() {
                let node = node.borrow();
                let node = node.object::<Dict>().unwrap();

                let emit = node["emit"].borrow();
                let emit = emit.object::<Str>().unwrap().as_str();

                let children = List::from(node["children"].clone());
                let name = children[0].borrow().object::<Dict>().unwrap()["value"].to_string();

                match emit {
                    "gen" => {
                        let offset = traverse_node_offset(node);

                        assert!(children.len() <= 2);

                        // Evaluate default parameter
                        let default = if children.len() == 2 {
                            let default = traverse_node_static(
                                scope,
                                Some(name.clone()),
                                children[1].borrow().object::<Dict>().unwrap(),
                            );

                            if utils::identifier_is_consumable(&name) && !default.is_consuming() {
                                scope.error(
                                    offset,
                                    format!(
                                        "Generic '{}' defines consumable, but '{}' is not consuming",
                                        name, default
                                    ),
                                );
                            }

                            Some(default)
                        } else {
                            None
                        };

                        if generics.insert(name.clone(), default).is_some() {
                            scope.error(
                                offset,
                                format!("Generic '{}' already defined in signature before", name),
                            );
                        }
                    }
                    "arg" => {
                        let first = name.chars().nth(0).unwrap();

                        // Check for correct identifier semantics
                        if !first.is_lowercase() {
                            scope.error(
                                    traverse_node_offset(node),
                                    if first == '_' {
                                        format!(
                                            "Argument named '{}' invalid; May not start with '{}'",
                                            name, first
                                        )
                                    }
                                    else {
                                        format!(
                                            "Argument named '{}' invalid; Use a name starting in lower-case, e.g. '{}{}'",
                                            name, &name[0..1].to_lowercase(), &name[1..]
                                        )
                                    }
                            );
                        }

                        assert!(children.len() <= 2);

                        if signature
                            .insert(
                                name.clone(),
                                if children.len() == 2 {
                                    let default = children[1].borrow();
                                    Some(traverse_node_static(
                                        scope,
                                        Some(name.clone()),
                                        default.object::<Dict>().unwrap(),
                                    ))
                                } else {
                                    None
                                },
                            )
                            .is_some()
                        {
                            scope.error(
                                traverse_node_offset(node),
                                format!("Argument '{}' already given in signature before", name),
                            );
                        }
                        //println!("{} {} {:?}", emit.to_string(), ident, default);
                    }
                    _ => unreachable!(),
                }
            }

            // Create new parselet to construct
            let parselet = ImlParselet::new(ImlParseletInstance::new(
                Some(ImlParseletModel::new(Some(signature))),
                Some(generics),
                traverse_node_offset(node),
                name,
                5,
                false,
            ));

            let body = body.borrow();
            traverse_node_rvalue(
                // Push new parselet scope
                &scope.shadow(ScopeLevel::Parselet(parselet.clone())),
                body.object::<Dict>().unwrap(),
                Rvalue::CallOrLoad,
            );

            //println!("parselet = {:#?}", parselet);
            ImlValue::from(parselet)
        }

        "value_generic" => {
            let children = List::from(&node["children"]);

            // Traverse the target
            let target = &children[0].borrow();
            let target = target.object::<Dict>().unwrap();
            let target = traverse_node_static(scope, None, target);

            // Traverse generic arguments
            let mut args = Vec::new();
            let mut nargs = IndexMap::new();

            for genarg in children[1..].iter() {
                let genarg = genarg.borrow();
                let genarg = genarg.object::<Dict>().unwrap();

                let offset = traverse_node_offset(genarg);
                let emit = genarg["emit"].borrow();

                match emit.object::<Str>().unwrap().as_str() {
                    "genarg" => {
                        if !nargs.is_empty() {
                            scope.error(
                                traverse_node_offset(node),
                                format!(
                                    "Sequencial generics need to be specified before named generics."
                                ),
                            );

                            continue;
                        }

                        let param = &genarg["children"].borrow();
                        let param = param.object::<Dict>().unwrap();

                        args.push((offset, traverse_node_static(scope, None, param)));
                    }

                    "genarg_named" => {
                        let children = List::from(&genarg["children"]);

                        let ident = children[0].borrow();
                        let ident = ident.object::<Dict>().unwrap();
                        let ident = ident["value"].borrow();
                        let ident = ident.object::<Str>().unwrap().as_str();

                        if nargs.contains_key(ident) {
                            scope.error(
                                traverse_node_offset(genarg),
                                format!("Named generic '{}' provided more than once.", ident),
                            );

                            continue;
                        }

                        let param = &children[1].borrow();
                        let param = param.object::<Dict>().unwrap();

                        nargs.insert(
                            ident.to_string(),
                            (offset, traverse_node_static(scope, None, param)),
                        );
                    }

                    other => unimplemented!("Unhandled genarg type {:?}", other),
                }
            }

            /*
            if let Some(name) = &name {
                println!(
                    "name = {} target = {:?} args = {:?} nargs = {:?}",
                    name, target, args, nargs
                );
            }
            */

            ImlValue::Instance {
                target: Box::new(target),
                args,
                nargs,
                offset: traverse_node_offset(node),
                severity: None,
                is_generated: false,
            }

            /*
            if let Some(_) = &name {
                println!("ret = {:?}", ret);
            }
            */
        }

        _ => unimplemented!("unhandled value node {}", emit),
    }
}

/** Traverse a static value.

The value must either be a literal or something from a known constant.

The assign attribute is optional and is only provided when the static is being assigned to
some identifier. In this case, special handling of e.g. value_generic-nodes is being performed.
*/
fn traverse_node_static(scope: &Scope, assign: Option<String>, node: &Dict) -> ImlValue {
    let emit = node["emit"].borrow();
    let emit = emit.object::<Str>().unwrap().as_str();

    /*
    // Special case: Put a generic with an assignment name into its own parselet
    if emit == "value_generic" && assign.is_some() {
        // Handle anything else as an implicit parselet in its own scope
        let implicit_parselet = ImlParselet::new(ImlParseletInstance::new(
            None,
            None,
            traverse_node_offset(node),
            assign,
            5,
            false,
        ));

        implicit_parselet.borrow().model.borrow_mut().body = traverse_node_rvalue(
            &scope.shadow(ScopeLevel::Parselet(implicit_parselet.clone())),
            node,
            Rvalue::CallOrLoad,
        );

        ImlValue::from(implicit_parselet)
    } else */
    if emit.starts_with("value_") {
        traverse_node_value(scope, node, assign).try_resolve(scope)
    } else {
        // Handle anything else as an implicit parselet in its own scope
        let implicit_parselet = ImlParselet::new(ImlParseletInstance::new(
            None,
            None,
            traverse_node_offset(node),
            assign,
            5,
            false,
        ));

        implicit_parselet.borrow().model.borrow_mut().body = {
            match traverse_node_rvalue(
                &scope.shadow(ScopeLevel::Parselet(implicit_parselet.clone())),
                node,
                Rvalue::Load,
            ) {
                ImlOp::Nop => return value!(void).into(),
                // Defined value call without parameters, or load becomes just the value
                ImlOp::Load { target: value, .. } => return value,

                // Any other code becomes its own parselet without any signature.
                body => body,
            }
        };

        ImlValue::from(implicit_parselet)
    }
}

// Traverse lvalue
fn traverse_node_lvalue(scope: &Scope, node: &Dict, store: bool, hold: bool) -> ImlOp {
    let emit = node["emit"].borrow();
    let emit = emit.object::<Str>().unwrap().as_str();
    assert!(emit == "lvalue");

    let children = List::from(&node["children"]);

    let mut ops: Vec<ImlOp> = Vec::new();

    for (i, item) in children.iter().enumerate() {
        let item = item.borrow();
        let item = item.object::<Dict>().unwrap();

        let emit = item["emit"].borrow();
        let emit = emit.object::<Str>().unwrap().as_str();

        let last = i == children.len() - 1; // Identify last child
        let store = if !last { false } else { store }; // Identify store instruction, otherwise load

        match emit {
            capture if capture.starts_with("capture") => {
                let children = item["children"].borrow();

                match capture {
                    "capture_alias" | "capture_expr" => {
                        ops.push(traverse_node_rvalue(
                            scope,
                            children.object::<Dict>().unwrap(),
                            Rvalue::CallOrLoad,
                        ));

                        if store {
                            if hold {
                                ops.push(Op::StoreCaptureHold.into())
                            } else {
                                ops.push(Op::StoreCapture.into())
                            }
                        } else {
                            ops.push(Op::LoadCapture.into())
                        }
                    }

                    "capture_index" => {
                        let children = children.object::<Dict>().unwrap();
                        let index = traverse_node_value(scope, children, None).unwrap();

                        if store {
                            if hold {
                                ops.push(
                                    Op::StoreFastCaptureHold(index.to_usize().unwrap()).into(),
                                );
                            } else {
                                ops.push(Op::StoreFastCapture(index.to_usize().unwrap()).into());
                            }
                        } else {
                            ops.push(Op::LoadFastCapture(index.to_usize().unwrap()).into());
                        }
                    }

                    _ => {
                        unreachable!();
                    }
                }
            }

            "identifier" => {
                let name = item["value"].borrow();
                let name = name.object::<Str>().unwrap().as_str();

                // This loop is only iterated in case a variable isn't known!
                'load: loop {
                    match scope.resolve_name(traverse_node_offset(item), name) {
                        // Known local
                        Some(ImlValue::Variable {
                            addr, is_global, ..
                        }) => {
                            if store {
                                if hold {
                                    if is_global {
                                        ops.push(Op::StoreGlobalHold(addr).into())
                                    } else {
                                        ops.push(Op::StoreFastHold(addr).into())
                                    }
                                } else if is_global {
                                    ops.push(Op::StoreGlobal(addr).into())
                                } else {
                                    ops.push(Op::StoreFast(addr).into())
                                }
                            } else if is_global {
                                ops.push(Op::LoadGlobal(addr).into())
                            } else {
                                ops.push(Op::LoadFast(addr).into())
                            }

                            break;
                        }
                        // Check for not assigning to a constant (at any level)
                        Some(_) => {
                            scope.error(
                                traverse_node_offset(node),
                                format!("Cannot assign to constant '{}'", name),
                            );
                            break 'load;
                        }
                        // Undefined name
                        None => {
                            // Check if identifier is not a reserved word
                            if scope.compiler.restrict && RESERVED_KEYWORDS.contains(&name) {
                                scope.error(
                                    traverse_node_offset(node),
                                    format!("Expected identifier, found reserved word '{}'", name),
                                );

                                break 'load;
                            }

                            // Check if identifier is not defining a consumable
                            if utils::identifier_is_consumable(name) {
                                scope.error(
                                    traverse_node_offset(node),

                                    if &name[0..1] == "_" {
                                        format!(
                                            "The variable '{}' is invalid, only constants may start with '_'",
                                            name
                                        )
                                    }
                                    else {
                                        format!(
                                            "Cannot assign variable named '{}'; Try lower-case identifier, e.g. '{}'",
                                            name, name.to_lowercase()
                                        )
                                    }
                                );

                                break 'load;
                            }

                            // When chained lvalue, name must be declared!
                            if children.len() > 1 {
                                scope.error(
                                    traverse_node_offset(node),
                                    format!(
                                        "Undeclared variable '{}', please define it first",
                                        name
                                    ),
                                );
                                break;
                            }

                            scope.register_variable(name);
                        }
                    }
                }
            }

            // item -----------------------------------------------------------
            "item" => {
                ops.push(traverse(scope, &item["children"]));

                if store {
                    if hold {
                        ops.push(Op::StoreItemHold.into());
                    } else {
                        ops.push(Op::StoreItem.into());
                    }
                } else {
                    ops.push(Op::LoadItem.into())
                }
            }

            other => {
                unimplemented!("{:?} not implemented for lvalue", other);
            }
        }
    }

    ImlOp::from(ops)
}

/** Enum used to specify traverse_node_rvalue code generation.

Rvalue::Load generates code to just load the value,
Rvalue::CallOrLoad generates code to either call the value without parameters or load it
Rvalue::Call(args, nargs) generates code for a full-qualified value call
*/
#[derive(Debug)]
enum Rvalue {
    Load,              // Generate code to just load the value
    CallOrLoad,        // Generate code for a call without parameters, or load otherwise
    Call(usize, bool), // Generate code for a full qualified call
}

// Traverse an rvalue
fn traverse_node_rvalue(scope: &Scope, node: &Dict, mode: Rvalue) -> ImlOp {
    let emit = node["emit"].borrow();
    let emit = emit.object::<Str>().unwrap().as_str();

    //println!("emit = {:?}", emit);

    let op = match emit {
        // attribute ------------------------------------------------------
        "attribute" => ImlOp::from(vec![
            {
                let children = node["children"].borrow();
                traverse_node_rvalue(
                    scope,
                    children.object::<Dict>().unwrap(),
                    Rvalue::CallOrLoad,
                )
            },
            ImlOp::from(Op::LoadAttr),
        ]),

        // identifier -----------------------------------------------------
        "identifier" => {
            let name = node["value"].borrow();
            let name = name.object::<Str>().unwrap().as_str();

            let offset = traverse_node_offset(node);

            // Check if identifier is not a reserved word
            if scope.compiler.restrict && RESERVED_KEYWORDS.contains(&name) {
                scope.error(
                    offset,
                    format!("Expected identifier, found reserved word '{}'", name),
                );
            }

            //println!("identifier = {:?}, mode = {:?}", name, mode);

            return match mode {
                Rvalue::Load => ImlOp::load_by_name(scope, offset, name.to_string()),
                Rvalue::CallOrLoad => ImlOp::call_by_name(scope, offset, name.to_string(), None),
                Rvalue::Call(args, nargs) => {
                    ImlOp::call_by_name(scope, offset, name.to_string(), Some((args, nargs)))
                }
            };
        }

        // item -----------------------------------------------------------
        "item" => ImlOp::from(vec![
            traverse(scope, &node["children"]),
            traverse_offset(node),
            ImlOp::from(Op::LoadItem),
        ]),

        // rvalue ---------------------------------------------------------
        "rvalue" => {
            let children = List::from(&node["children"]);

            let mut ops = vec![traverse_offset(node)];

            for node in children.iter() {
                ops.push(traverse_node_rvalue(
                    scope,
                    node.borrow().object::<Dict>().unwrap(),
                    Rvalue::Load,
                ));
            }

            assert!(ops.len() > 0);
            ImlOp::from(ops)
        }

        // value ---------------------------------------------------------
        value if value.starts_with("value_") => {
            let offset = traverse_node_offset(node);
            let value = traverse_node_value(scope, node, None);

            return match mode {
                Rvalue::Load => ImlOp::load(scope, offset, value),
                Rvalue::CallOrLoad => ImlOp::call(scope, offset, value, None),
                Rvalue::Call(args, nargs) => ImlOp::call(scope, offset, value, Some((args, nargs))),
            };
        }

        _ => return traverse_node(scope, node),
    };

    // Rvalue call confguration of a value known at runtime.
    match mode {
        Rvalue::Load => op,
        Rvalue::CallOrLoad => ImlOp::from(vec![op, ImlOp::from(Op::CallOrCopy)]),
        Rvalue::Call(args, nargs) => ImlOp::from(vec![
            op,
            if args == 0 && !nargs {
                ImlOp::from(Op::Call)
            } else if args > 0 && !nargs {
                ImlOp::from(Op::CallArg(args))
            } else {
                ImlOp::from(Op::CallArgNamed(args))
            },
        ]),
    }
}

fn traverse_node(scope: &Scope, node: &Dict) -> ImlOp {
    let emit = node["emit"].borrow();
    let emit = emit.object::<Str>().unwrap().as_str();

    //println!("emit = {:?}", emit);
    match emit {
        "alias" => {
            let children = node["children"].borrow();
            let children = children.object::<List>().unwrap();

            let (alias, expr) = (&children[0].borrow(), &children[1].borrow());

            let alias = traverse_node_rvalue(scope, alias.object::<Dict>().unwrap(), Rvalue::Load);
            let expr =
                traverse_node_rvalue(scope, expr.object::<Dict>().unwrap(), Rvalue::CallOrLoad);

            // Push value first, then the alias
            ImlOp::from(vec![expr, alias, ImlOp::from(Op::MakeAlias)])
        }

        // area -----------------------------------------------------------
        "area" => {
            let body = traverse(scope, &node["children"]);
            ImlOp::seq(vec![body, ImlOp::from(Op::Extend)], false)
        }

        // assign ---------------------------------------------------------
        assign if assign.starts_with("assign") => {
            let children = node["children"].borrow();
            let children = children.object::<List>().unwrap();

            let (lvalue, value) = (children[0].borrow(), children[1].borrow());
            let lvalue = lvalue.object::<Dict>().unwrap();
            let value = value.object::<Dict>().unwrap();

            let parts: Vec<&str> = assign.split("_").collect();

            let mut ops = Vec::new();

            /* assignment with operation */
            if parts.len() > 1 && !["copy", "drop", "hold"].contains(&parts[1]) {
                ops.push(traverse_node_lvalue(scope, lvalue, false, false));
                ops.push(traverse_node_rvalue(scope, value, Rvalue::Load));

                ops.push(match parts[1] {
                    "add" => ImlOp::from(Op::BinaryOp("iadd")),
                    "sub" => ImlOp::from(Op::BinaryOp("isub")),
                    "mul" => ImlOp::from(Op::BinaryOp("imul")),
                    "div" => ImlOp::from(Op::BinaryOp("idiv")),
                    "divi" => ImlOp::from(Op::BinaryOp("idivi")),
                    "mod" => ImlOp::from(Op::BinaryOp("imod")),
                    _ => unreachable!(),
                });

                match *parts.last().unwrap() {
                    "hold" => {}
                    "copy" => ops.push(Op::Sep.into()),
                    _ => ops.push(Op::Inv.into()),
                }
            }
            /* normal assignment without operation */
            else {
                ops.push(traverse_node_rvalue(scope, value, Rvalue::Load));
                ops.push(traverse_offset(node));
                ops.push(traverse_node_lvalue(
                    scope,
                    lvalue,
                    true,
                    ["copy", "hold"].contains(parts.last().unwrap()),
                ));

                if *parts.last().unwrap() == "copy" {
                    ops.push(Op::Sep.into())
                }
            }

            ImlOp::from(ops)
        }

        // begin ----------------------------------------------------------
        "begin" | "end" => {
            let body = traverse(scope, &node["children"]);

            if let ScopeLevel::Parselet(parselet) = &scope.level {
                let parselet = parselet.borrow();
                let mut model = parselet.model.borrow_mut();

                if emit == "begin" {
                    match &mut model.begin {
                        ImlOp::Nop => model.begin = body,
                        ImlOp::Alt { alts } => alts.push(body),
                        _ => {
                            let alt = ImlOp::Alt {
                                alts: vec![std::mem::replace(&mut model.begin, ImlOp::Nop), body],
                            };
                            model.begin = alt;
                        }
                    }
                } else {
                    match &mut model.end {
                        ImlOp::Nop => model.end = body,
                        ImlOp::Alt { alts } => alts.push(body),
                        _ => {
                            let alt = ImlOp::Alt {
                                alts: vec![std::mem::replace(&mut model.end, ImlOp::Nop), body],
                            };
                            model.end = alt;
                        }
                    }
                }
            } else {
                scope.error(
                    traverse_node_offset(node),
                    format!("'{}' may only be used in parselet scope", emit),
                );
            }

            ImlOp::Nop
        }

        // block ----------------------------------------------------------
        "block" | "body" | "main" => {
            if let Some(ast) = node.get_str("children") {
                fn traverse_alts(scope: &Scope, ast: &RefValue) -> ImlOp {
                    if let Some(list) = ast.borrow().object::<List>() {
                        let mut alts = Vec::new();

                        for item in list.iter() {
                            match traverse(scope, item) {
                                ImlOp::Nop => {}
                                item => alts.push(item),
                            }
                        }

                        ImlOp::Alt { alts }
                    } else if let Some(dict) = ast.borrow().object::<Dict>() {
                        traverse_node_rvalue(scope, dict, Rvalue::CallOrLoad)
                    } else {
                        unreachable!();
                    }
                }

                match emit {
                    "body" | "main" => {
                        scope.parselet().borrow().model.borrow_mut().body =
                            traverse_alts(scope, ast);
                        ImlOp::Nop
                    }
                    // block and body runs in a block level scope
                    _ => traverse_alts(&scope.shadow(ScopeLevel::Block), ast),
                }
            } else {
                ImlOp::Nop
            }
        }

        // call -----------------------------------------------------------
        "call" => {
            let mut ops = Vec::new();
            let mut args = 0;
            let mut nargs = 0;

            // Traverse call parameters from AST
            let children = List::from(&node["children"]);

            for param in &children[1..] {
                let param = param.borrow();
                let param = param.object::<Dict>().unwrap();

                let emit = param["emit"].borrow();

                match emit.object::<Str>().unwrap().as_str() {
                    "callarg" => {
                        if nargs > 0 {
                            scope.error(
                                traverse_node_offset(node),
                                format!(
                                    "Sequencial arguments need to be specified before named arguments."
                                ),
                            );

                            continue;
                        }

                        let param = &param["children"].borrow();
                        let param = param.object::<Dict>().unwrap();

                        ops.push(traverse_node_rvalue(scope, param, Rvalue::CallOrLoad));
                        args += 1;
                    }

                    "callarg_named" => {
                        let children = List::from(&param["children"]);

                        let param = &children[1].borrow();
                        let param = param.object::<Dict>().unwrap();

                        ops.push(traverse_node_rvalue(scope, param, Rvalue::CallOrLoad));

                        let ident = children[0].borrow();
                        let ident = ident.object::<Dict>().unwrap();
                        let ident = ident["value"].borrow();
                        let ident = ident.object::<Str>().unwrap().as_str();

                        ops.push(ImlOp::load(
                            scope,
                            traverse_node_offset(&param),
                            ImlValue::from(RefValue::from(ident)),
                        ));

                        nargs += 1;
                    }

                    other => unimplemented!("Unhandled parameter type {:?}", other),
                }
            }

            // When calling with nargs, create a nargs dict first
            if nargs > 0 {
                ops.push(ImlOp::from(Op::MakeDict(nargs)));
            }

            let rvalue = children[0].borrow();
            ops.push(traverse_node_rvalue(
                scope,
                rvalue.object::<Dict>().unwrap(),
                Rvalue::Call(args, nargs > 0),
            ));

            ImlOp::from(ops)
        }

        // capture --------------------------------------------------------
        "capture_alias" | "capture_expr" => ImlOp::from(vec![
            {
                let children = node["children"].borrow();
                traverse_node_rvalue(
                    scope,
                    children.object::<Dict>().unwrap(),
                    Rvalue::CallOrLoad,
                )
            },
            ImlOp::from(Op::LoadCapture),
        ]),

        "capture_index" => {
            let children = node["children"].borrow();
            let index =
                traverse_node_value(scope, children.object::<Dict>().unwrap(), None).unwrap();
            ImlOp::from(Op::LoadFastCapture(index.to_usize().unwrap()))
        }

        // comparison -----------------------------------------------------
        "comparison" => {
            // comparison can be a chain of comparisons, allowing to compare e.g. `1 < 2 < 3`
            let children = node["children"].borrow();
            let mut children = children.object::<List>().unwrap().clone();

            let first = children.remove(0);
            let first = first.borrow();

            let mut ops = Vec::new();

            ops.push(traverse_node_rvalue(
                scope,
                &first.object::<Dict>().unwrap(),
                Rvalue::CallOrLoad,
            ));

            let mut backpatch = Vec::new();

            while !children.is_empty() {
                let child = children.remove(0);
                let child = child.borrow();
                let child = child.object::<Dict>().unwrap();

                let emit = child["emit"].borrow();
                let emit = emit.object::<Str>().unwrap().as_str();

                let next = child["children"].borrow();

                ops.push(traverse_node_rvalue(
                    scope,
                    &next.object::<Dict>().unwrap(),
                    Rvalue::CallOrLoad,
                ));

                // Chained comparison requires forperand duplication
                if !children.is_empty() {
                    ops.push(ImlOp::from(Op::Swap(2))); // Swap operands
                    ops.push(ImlOp::from(Op::Copy(2))); // Copy second operand
                }

                ops.push(ImlOp::from(match emit {
                    "cmp_eq" => Op::BinaryOp("eq"),
                    "cmp_neq" => Op::BinaryOp("neq"),
                    "cmp_lteq" => Op::BinaryOp("lteq"),
                    "cmp_gteq" => Op::BinaryOp("gteq"),
                    "cmp_lt" => Op::BinaryOp("lt"),
                    "cmp_gt" => Op::BinaryOp("gt"),
                    _ => unimplemented!("{}", emit),
                }));

                // Push and remember placeholder for later clean-up jump
                if !children.is_empty() {
                    backpatch.push(ops.len());
                    ops.push(ImlOp::Nop); // Placeholder for condition
                }
            }

            if backpatch.len() > 0 {
                // Jump over clean-up part with last result
                ops.push(ImlOp::from(Op::Forward(3)));

                // Otherwise, remember clean-up start
                let clean_up = ops.len();
                ops.push(ImlOp::from(Op::Drop));
                ops.push(ImlOp::from(Op::PushFalse));

                // Backpatch all placeholders to relative jump to the clean-up part
                for index in backpatch {
                    ops[index] = ImlOp::from(Op::ForwardIfFalse(clean_up - index + 1));
                }
            }

            ImlOp::from(ops)
        }

        // constant -------------------------------------------------------
        "constant" => {
            let children = node["children"].borrow();
            let children = children.object::<List>().unwrap();

            let (ident, value) = (children[0].borrow(), children[1].borrow());

            let ident = ident.object::<Dict>().unwrap();
            let ident = ident["value"].borrow();
            let ident = ident.object::<Str>().unwrap().as_str();

            // Disallow assignment to any reserved identifier
            if scope.compiler.restrict
                && (RESERVED_KEYWORDS.contains(&ident) || RESERVED_TOKENS.contains(&ident))
            {
                scope.error(
                    traverse_node_offset(node),
                    format!("Expected identifier, found reserved word '{}'", ident),
                );

                return ImlOp::Nop;
            }

            // Distinguish between pure values or an expression
            let value = value.object::<Dict>().unwrap();

            // fixme: Restricted to pure values currently.
            let value = traverse_node_static(scope, Some(ident.to_string()), value);

            if value.is_consuming() {
                if !utils::identifier_is_consumable(ident) {
                    scope.error(
                        traverse_node_offset(node),
                        format!(
                            "Cannot assign constant '{}' as consumable. Use an identifier starting in upper-case, e.g. '{}{}'",
                            ident, &ident[0..1].to_uppercase(), &ident[1..]
                        )
                    );

                    return ImlOp::Nop;
                }
            } else if utils::identifier_is_consumable(ident) {
                scope.error(
                    traverse_node_offset(node),
                    if ident.starts_with("_") {
                        format!(
                            "Cannot assign to constant '{}', because it must be consumable. Use an identifier not starting with '_'.",
                            ident
                        )
                    }
                    else {
                        format!(
                            "Cannot assign to constant '{}', because it must be consumable. Use an identifier starting in lower-case, e.g. '{}{}'",
                            ident, &ident[0..1].to_lowercase(), &ident[1..]
                        )
                    }
                );

                return ImlOp::Nop;
            }

            // println!("{} : {:#?}", ident, value);
            scope.define_constant(ident, value);

            // Try to resolve usage of newly introduced constant in current scope
            scope.resolve_usages();

            ImlOp::Nop
        }

        // inplace --------------------------------------------------------
        inplace if inplace.starts_with("inplace_") => {
            let children = node["children"].borrow();
            let lvalue = children.object::<Dict>().unwrap();

            let mut ops = vec![
                traverse_node_lvalue(scope, lvalue, false, false),
                traverse_offset(node),
            ];

            let parts: Vec<&str> = inplace.split("_").collect();

            match parts[1] {
                "pre" => {
                    ops.push(ImlOp::from(if parts[2] == "inc" {
                        Op::UnaryOp("iinc")
                    } else {
                        Op::UnaryOp("idec")
                    }));
                    ops.push(ImlOp::from(Op::Sep)); // Separate TOS
                }
                "post" => {
                    ops.extend(vec![
                        ImlOp::from(Op::Dup),
                        ImlOp::from(Op::Swap(2)),
                        ImlOp::from(if parts[2] == "inc" {
                            Op::UnaryOp("iinc")
                        } else {
                            Op::UnaryOp("idec")
                        }),
                        ImlOp::from(Op::Drop),
                    ]);
                }
                _ => unreachable!(),
            }

            ImlOp::from(ops)
        }

        // operator ------------------------------------------------------
        op if op.starts_with("op_") => {
            let parts: Vec<&str> = emit.split("_").collect();
            let mut ops = Vec::new();

            let op = match parts[1] {
                "accept" | "break" | "exit" | "push" => {
                    if parts[1] == "break" && !scope.is_loop() {
                        scope.error(
                            traverse_node_offset(node),
                            format!("'break' cannot be used outside of a loop."),
                        );
                    }

                    if let Some(value) = node.get_str("children") {
                        let value = value.borrow();
                        ops.push(traverse_node_rvalue(
                            scope,
                            &value.object::<Dict>().unwrap(),
                            Rvalue::CallOrLoad,
                        ));

                        match parts[1] {
                            "accept" => Op::LoadAccept.into(),
                            "break" => Op::LoadBreak.into(),
                            "exit" => Op::LoadExit.into(),
                            "push" => Op::LoadPush.into(), // usecase?
                            _ => unreachable!(),
                        }
                    } else {
                        match parts[1] {
                            "accept" => Op::Accept.into(),
                            "break" => Op::Break.into(),
                            "exit" => Op::Exit.into(),
                            "push" => Op::Push.into(), // usecase?
                            _ => unreachable!(),
                        }
                    }
                }

                "continue" => {
                    if !scope.is_loop() {
                        scope.error(
                            traverse_node_offset(node),
                            format!("'continue' cannot be used outside of a loop."),
                        );
                    }

                    Op::Continue.into()
                }

                "deref" => {
                    let children = node["children"].borrow();
                    let children = children.object::<Dict>().unwrap();

                    traverse_node_rvalue(scope, children, Rvalue::Load)
                }

                "next" => Op::Next.into(),

                "nop" => ImlOp::Nop,

                "reject" => Op::Reject.into(),

                "repeat" => Op::Repeat.into(),

                "reset" => Op::ResetReader.into(),

                "unary" => {
                    let children = node["children"].borrow();
                    let children = children.object::<Dict>().unwrap();

                    let res = traverse_node_rvalue(scope, children, Rvalue::CallOrLoad);

                    // Evaluate operation at compile-time if possible
                    if let Ok(value) = res.get_evaluable_value() {
                        if let Ok(value) = value.unary_op(parts[2]) {
                            return ImlOp::load(
                                scope,
                                traverse_node_offset(node),
                                ImlValue::from(value),
                            );
                        }
                    }

                    ops.push(res);

                    // Push operation position here
                    ops.push(traverse_offset(node));

                    ImlOp::from(match parts[2] {
                        "not" => Op::UnaryOp("not"),
                        "neg" => Op::UnaryOp("neg"),
                        _ => {
                            unimplemented!("{}", emit);
                        }
                    })
                }

                "binary" | "logical" => {
                    let children = node["children"].borrow();
                    let children = children.object::<List>().unwrap();
                    assert_eq!(children.len(), 2);

                    let (left, right) = (children[0].borrow(), children[1].borrow());
                    let left = traverse_node_rvalue(
                        scope,
                        &left.object::<Dict>().unwrap(),
                        Rvalue::CallOrLoad,
                    );
                    let right = traverse_node_rvalue(
                        scope,
                        &right.object::<Dict>().unwrap(),
                        Rvalue::CallOrLoad,
                    );

                    match parts[2] {
                        "and" => {
                            ops.push(left);
                            ops.push(traverse_offset(node));

                            ImlOp::If {
                                peek: true,
                                test: true,
                                then: Box::new(right),
                                else_: Box::new(ImlOp::from(Op::Push)),
                            }
                        }
                        "or" => {
                            ops.push(left);
                            ops.push(traverse_offset(node));

                            ImlOp::If {
                                peek: true,
                                test: false,
                                then: Box::new(right),
                                else_: Box::new(ImlOp::from(Op::Push)),
                            }
                        }
                        _ => {
                            // When both operands are direct values, evaluate operation at compile-time
                            if let (Ok(left), Ok(right)) =
                                (left.get_evaluable_value(), right.get_evaluable_value())
                            {
                                if let Ok(value) = left.binary_op(right, parts[2]) {
                                    return ImlOp::load(
                                        scope,
                                        traverse_node_offset(node),
                                        ImlValue::from(value),
                                    );
                                }
                            }

                            // Otherwise, generate code for operation
                            ops.push(left);
                            ops.push(right);

                            // Push operation position here
                            ops.push(traverse_offset(node));

                            ImlOp::from(match parts[2] {
                                "add" => Op::BinaryOp("add"),
                                "sub" => Op::BinaryOp("sub"),
                                "mul" => Op::BinaryOp("mul"),
                                "div" => Op::BinaryOp("div"),
                                "divi" => Op::BinaryOp("divi"),
                                "mod" => Op::BinaryOp("mod"),
                                _ => {
                                    unimplemented!("{}", emit);
                                }
                            })
                        }
                    }
                }

                "mod" => {
                    let children = node["children"].borrow();
                    let children = children.object::<Dict>().unwrap();

                    let res = traverse_node_static(scope, None, children);
                    let offset = traverse_node_offset(node);

                    /*
                    if !res.is_consuming() {
                        scope.error(
                            traverse_node_offset(node),
                            format!(
                                "Operator '{}' has no effect on non-consuming {}",
                                match parts[2] {
                                    "pos" => "+",
                                    "kle" => "*",
                                    "opt" => "?",
                                    other => other,
                                },
                                if matches!(res, ImlOp::Load { .. } | ImlOp::Call { .. }) {
                                    "value"
                                } else {
                                    "sequence"
                                }
                            ),
                        );
                    } else {
                        compiler.parselet_mark_consuming();
                    }
                    */

                    let mut assume_severity = None;

                    // Modifiers on usages of Token::Char can be optimized for better efficiency
                    if let ImlValue::Value(target) = &res {
                        let target = target.borrow();

                        // TODO: The Char-modifier-stuff needs the be refactored in a separate pull request.
                        match target.object::<Token>() {
                            Some(Token::Char(ccl)) => {
                                match parts[2] {
                                    // mod_pos on Token::Char becomes Token::Chars
                                    "pos" | "kle" => {
                                        let mut chars = ImlValue::from(RefValue::from(
                                            Token::Chars(ccl.clone()),
                                        ));

                                        // mod_kle on Token::Char becomes optional Token::Chars
                                        if parts[2] == "kle" {
                                            chars = chars.into_generic("Opt", None, offset.clone());
                                        }

                                        return ImlOp::call(scope, offset, chars, None);
                                    }

                                    // mod_not on Token::Char becomes a negated Token::Char
                                    "not" => {
                                        return ImlOp::call(
                                            scope,
                                            traverse_node_offset(node),
                                            ImlValue::from(RefValue::from(Token::Char(
                                                ccl.clone().negate(),
                                            ))),
                                            None,
                                        );
                                    }
                                    _ => {}
                                }
                            }
                            // fixme: This is an ugly hack to keep severity for modified versions
                            Some(Token::Touch(_)) => assume_severity = Some(0),
                            _ => {}
                        }
                    }

                    ImlOp::call(
                        scope,
                        offset.clone(),
                        match parts[2] {
                            "pos" => res.into_generic("Pos", assume_severity, offset),
                            "kle" => res.into_generic("Kle", assume_severity, offset),
                            "opt" => res.into_generic("Opt", assume_severity, offset),
                            _ => unreachable!(),
                        },
                        None,
                    )
                }

                "if" => {
                    let children = node["children"].borrow();
                    let children = children.object::<List>().unwrap();

                    let (condition, then_part) = (children[0].borrow(), children[1].borrow());

                    let condition = traverse_node_rvalue(
                        scope,
                        &condition.object::<Dict>().unwrap(),
                        Rvalue::CallOrLoad,
                    );
                    let then_part = traverse_node_rvalue(
                        scope,
                        &then_part.object::<Dict>().unwrap(),
                        Rvalue::CallOrLoad,
                    );
                    let else_part = if children.len() == 3 {
                        let else_part = children[2].borrow();
                        traverse_node_rvalue(
                            scope,
                            &else_part.object::<Dict>().unwrap(),
                            Rvalue::CallOrLoad,
                        )
                    } else {
                        ImlOp::from(Op::Push)
                    };

                    // Compile time evaluation;
                    // In case the condition of the if already fails here, it doesn't need to be
                    // compiled into the program.
                    if let Ok(value) = condition.get_evaluable_value() {
                        if value.is_true() {
                            return then_part;
                        }

                        return else_part;
                    }

                    ops.push(condition);

                    ImlOp::If {
                        peek: false,
                        test: true,
                        then: Box::new(then_part),
                        else_: Box::new(else_part),
                    }
                }

                "for" => {
                    let children = node["children"].borrow();
                    let children = children.object::<List>().unwrap();
                    assert!(children.len() == 3);

                    let (var, iter_expr, body) = (
                        &children[0].borrow(),
                        &children[1].borrow(),
                        &children[2].borrow(),
                    );

                    let var = var.object::<Dict>().unwrap();
                    let iter_expr = iter_expr.object::<Dict>().unwrap();
                    let body = body.object::<Dict>().unwrap();

                    let temp = scope.parselet().borrow().model.borrow_mut().claim_temp();

                    // Create an iter() on the iter expression
                    let initial = ImlOp::from(vec![
                        traverse_node_rvalue(scope, iter_expr, Rvalue::CallOrLoad),
                        {
                            let iter =
                                ImlValue::from(RefValue::from(Builtin::get("iter").unwrap()));
                            ImlOp::call(scope, None, iter, Some((1, false)))
                        },
                        ImlOp::from(if scope.is_global() {
                            Op::StoreGlobal(temp)
                        } else {
                            Op::StoreFast(temp)
                        }),
                    ]);

                    // Create the condition, which calls iter_next() until void is returned
                    let condition = ImlOp::from(vec![
                        ImlOp::from(if scope.is_global() {
                            Op::LoadGlobal(temp)
                        } else {
                            Op::LoadFast(temp)
                        }),
                        {
                            let iter_next =
                                ImlValue::from(RefValue::from(Builtin::get("iter_next").unwrap()));
                            ImlOp::call(scope, None, iter_next, Some((1, false)))
                        },
                        traverse_node_lvalue(
                            scope, var, true, true, //hold for preceding loop break check
                        ),
                    ]);

                    // Traverse loop body
                    let body =
                        traverse_node_rvalue(&scope.shadow(ScopeLevel::Loop), body, Rvalue::Load);

                    // Give temp variable back for possible reuse.
                    scope
                        .parselet()
                        .borrow()
                        .model
                        .borrow_mut()
                        .return_temp(temp);

                    ImlOp::Loop {
                        use_iterator: true,
                        initial: Box::new(initial),
                        condition: Box::new(condition),
                        body: Box::new(body),
                    }
                }

                "loop" => {
                    let children = List::from(&node["children"]);

                    let scope = scope.shadow(ScopeLevel::Loop);

                    let ret = match children.len() {
                        1 => {
                            let body = &children[0].borrow();

                            ImlOp::Loop {
                                use_iterator: false,
                                initial: Box::new(ImlOp::Nop),
                                condition: Box::new(ImlOp::Nop),
                                body: Box::new(traverse_node_rvalue(
                                    &scope,
                                    body.object::<Dict>().unwrap(),
                                    Rvalue::CallOrLoad,
                                )),
                            }
                        }
                        2 => {
                            let (condition, body) = (&children[0].borrow(), &children[1].borrow());

                            ImlOp::Loop {
                                use_iterator: false,
                                initial: Box::new(ImlOp::Nop),
                                condition: Box::new(traverse_node_rvalue(
                                    &scope,
                                    condition.object::<Dict>().unwrap(),
                                    Rvalue::CallOrLoad,
                                )),
                                body: Box::new(traverse_node_rvalue(
                                    &scope,
                                    body.object::<Dict>().unwrap(),
                                    Rvalue::CallOrLoad,
                                )),
                            }
                        }
                        _ => unreachable!(),
                    };

                    ret
                }

                _ => {
                    unimplemented!("{} missing", op);
                }
            };
            ops.push(op);

            ImlOp::from(ops)
        }

        // sequence, dict, list  -----------------------------------------
        "sequence" | "dict" | "list" => {
            let children = if let Some(children) = node.get_str("children") {
                List::from(children)
            } else {
                List::new()
            };

            //println!("{} => {:?}", emit, children);

            let mut ops = Vec::new();

            for node in children.iter() {
                ops.push(traverse_node_rvalue(
                    scope,
                    node.borrow().object::<Dict>().unwrap(),
                    Rvalue::CallOrLoad, // fixme: only statics or "real" rvalue nodes should be called, others just loaded
                ));
            }

            match emit {
                "list" if ops.is_empty() => ImlOp::from(Op::MakeList(0)),
                "list" => {
                    ops.push(ImlOp::from(Op::MakeList(ops.len())));
                    ImlOp::seq(ops, false)
                }
                "dict" if ops.is_empty() => ImlOp::from(Op::MakeDict(0)),
                _ => ImlOp::seq(ops, true),
            }
        }

        // ---------------------------------------------------------------
        _ => unreachable!("No handling for {:?}", emit),
    }
}

/// Debug function to print an AST to stdout.
pub fn print(ast: &RefValue) {
    fn print(value: &RefValue, indent: usize) {
        let value = value.borrow();

        if let Some(d) = value.object::<Dict>() {
            let emit = d["emit"].to_string();

            let row = d
                .get_str("row")
                .and_then(|row| Some(row.borrow().to_usize().unwrap()));
            let col = d
                .get_str("col")
                .and_then(|col| Some(col.borrow().to_usize().unwrap()));
            let stop_row = d
                .get_str("stop_row")
                .and_then(|row| Some(row.borrow().to_usize().unwrap()));
            let stop_col = d
                .get_str("stop_col")
                .and_then(|col| Some(col.borrow().to_usize().unwrap()));

            let value = d.get_str("value");
            let children = d.get_str("children");

            if let (Some(row), Some(col), Some(stop_row), Some(stop_col)) =
                (row, col, stop_row, stop_col)
            {
                print!(
                    "{:indent$}{} [start {}:{}, end {}:{}]",
                    "",
                    emit,
                    row,
                    col,
                    stop_row,
                    stop_col,
                    indent = indent
                );
            } else if let (Some(row), Some(col)) = (row, col) {
                print!("{:indent$}{} [{}:{}]", "", emit, row, col, indent = indent);
            } else {
                print!("{:indent$}{}", "", emit, indent = indent);
            }

            if let Some(value) = value {
                print!(" => {}", value.repr());
            }
            print!("\n");

            if let Some(children) = children {
                print(children, indent + 1);
            }
        } else if let Some(l) = value.object::<List>() {
            for item in l.iter() {
                print(item, indent);
            }
        }
    }

    print(ast, 0);
}

tokay_function!("ast : @emit, value=void, flatten=true, debug=false", {
    let context = context.unwrap();

    let mut ret = Dict::new();
    ret.insert_str("emit", emit.clone());

    // Need current frame position
    let capture_start = context.frame.capture_start;

    let value = if value.is_void() {
        Some(
            context
                .collect(capture_start, false, true, debug.is_true())
                .extract(&context.thread.reader),
        )
    } else {
        Some(value)
    };

    // Debug
    if debug.is_true() {
        println!(
            "ast(emit={:?}, value={:?}, flatten={:?})",
            emit, value, flatten
        );
    }

    if let Some(value) = value {
        // Lists can be flattened
        if value.borrow().object::<List>().is_some() {
            ret.insert_str(
                "children",
                if flatten.is_true() {
                    List::list_flatten(vec![value], None).unwrap()
                } else {
                    value.clone()
                },
            );
        }
        // Dicts can be used directly as children
        else if value.borrow().object::<Dict>().is_some() {
            ret.insert_str("children", value.clone());
        }
        // Otherwise this is a value
        else if !value.is_void() {
            ret.insert_str("value", value.clone());
        }
    }

    let start = context.frame.reader_start;
    let reader_start = context.thread.reader.start();

    // Store positions of reader start
    ret.insert_str("offset", value!(start.offset + reader_start.offset));
    ret.insert_str("row", value!(start.row as usize));
    ret.insert_str("col", value!(start.col as usize));

    // Store positions of reader stop
    let current = context.thread.reader.tell();

    ret.insert_str("stop_offset", value!(current.offset + reader_start.offset));
    ret.insert_str("stop_row", value!(current.row as usize));
    ret.insert_str("stop_col", value!(current.col as usize));

    RefValue::from(ret).into()
});

tokay_function!("ast_print : @ast", {
    print(&ast);
    value!(void).into()
});

tokay_function!("ast2rust : @ast, level=0", {
    fn print(value: &RefValue, indent: usize, first: bool) {
        let (br_left, br_right) = if first { ("", "") } else { ("(", ")") };
        let value = value.borrow();

        if let Some(d) = value.object::<Dict>() {
            let emit = d["emit"].to_string();
            let value = d.get_str("value");
            let children = d.get_str("children");

            print!(
                "{space:indent$}{br_left}value!([\n{space:indent$}    \"emit\" => {emit:?}",
                space = "",
                indent = indent * 4,
                br_left = br_left,
                emit = emit
            );

            if let Some(children) = children {
                print!(
                    ",\n{space:indent$}    \"children\" =>\n",
                    space = "",
                    indent = indent * 4
                );

                print(children, indent + 2, false);
            }

            if let Some(value) = value {
                print!(
                    ",\n{space:indent$}    \"value\" => ",
                    space = "",
                    indent = indent * 4
                );
                print(value, indent, false);
            }

            print!(
                "\n{space:indent$}]){br_right}",
                space = "",
                indent = indent * 4,
                br_right = br_right
            );
        } else if let Some(l) = value.object::<List>() {
            print!(
                "{space:indent$}{br_left}value!([\n",
                space = "",
                indent = indent * 4,
                br_left = br_left
            );

            let mut iter = l.iter().peekable();

            while let Some(item) = iter.next() {
                print(item, indent + 1, false);
                if iter.peek().is_some() {
                    print!(",\n");
                }
            }

            print!(
                "\n{space:indent$}]){br_right}",
                space = "",
                indent = indent * 4,
                br_right = br_right
            );
        } else {
            assert!(
                ["str", "int", "float", "bool", "void"].contains(&value.name()),
                "No matching Rust primitive for {} found",
                value.name()
            );

            // Rust primitives are mostly equal to Tokay's repr
            print!("{}", value.repr());
        }
    }

    print(&ast, level.to_usize()?, true);
    print!("\n");
    value!(void).into()
});
