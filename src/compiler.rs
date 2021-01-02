use std::collections::HashMap;
use std::cell::RefCell;
use crate::tokay::*;
use crate::value::{Value, RefValue, BorrowByKey, BorrowByIdx, Dict, List};
use crate::builtin;


/** Compiler symbolic scope.

In Tokay code, this relates to any block.
Scoped blocks (parselets) introduce new variable scopes.
*/
struct Scope {
    variables: Option<HashMap<String, usize>>,
    constants: HashMap<String, RefValue>
}


/** Tokay compiler instance, with related objects. */
pub struct Compiler {
    scopes: Vec<Scope>,                     // Current compilation scopes
    values: RefCell<Vec<RefValue>>,         // Constant values collected during compile
    pub parselets: Vec<RefCell<Parselet>>   // Parselets (public because used by macros)
}

impl Compiler {
    pub fn new() -> Self {
        let mut compiler = Self{
            scopes: vec![
                Scope{
                    variables: Some(HashMap::new()),
                    constants: HashMap::new()
                }
            ],
            values: RefCell::new(Vec::new()),
            parselets: Vec::new()
        };

        builtin::register(&mut compiler);

        compiler
    }

    /** Converts the compiled information into a Program. */
    pub fn into_program(mut self) -> Program {
        // Close any open scopes
        while self.scopes.len() > 1 {
            self.pop_scope();
        }

        // Resolve constants and globals from all scopes
        for i in 0..self.parselets.len() {
            self.parselets[i].borrow_mut().resolve(&self, false, true);
        }

        // Finalize
        Parselet::finalize(&self.parselets);

        // Drain parselets into the new program
        Program::new(
            self.parselets.drain(..).map(|p| p.into_inner()).collect(),
            self.values.borrow().to_vec(),
            //self.scopes[0].variables.len()  # fixme: these are the globals...
        )
    }

    /// Introduces a new scope, either for variables or constants only.
    pub fn push_scope(&mut self, variables: bool) {
        self.scopes.insert(0,
            Scope{
                variables: if variables { Some(HashMap::new()) } else { None },
                constants: HashMap::new()
            }
        );
    }

    /** Pops current scope. Returns number of locals defined.

    The final (main) scope cannot be dropped, the function panics when
    this is tried. */
    pub fn pop_scope(&mut self) {
        if self.scopes.len() == 1 {
            panic!("Can't pop main scope");
        }

        self.scopes.remove(0);
    }

    /// Returns current scope depth
    pub fn get_depth(&self) -> usize {
        self.scopes.len()
    }

    /// Returns the total number of locals in current scope.
    pub fn get_locals(&self) -> usize {
        if let Some(locals) = &self.scopes.first().unwrap().variables {
            locals.len()
        }
        else {
            0
        }
    }

    /**
    Retrieve address of a local variable under a given name.
    */
    pub fn get_local(&self, name: &str) -> Option<usize>
    {
        for scope in &self.scopes {
            // Check for scope with variables
            if let Some(variables) = &scope.variables {
                if let Some(addr) = variables.get(name) {
                    return Some(*addr)
                }

                break
            }
        }

        None
    }

    /** Insert new local variable under given name in current scope. */
    pub fn new_local(&mut self, name: &str) -> usize {
        for scope in &mut self.scopes {
            // Check for scope with variables
            if let Some(variables) = &mut scope.variables {
                if let Some(addr) = variables.get(name) {
                    return *addr
                }

                let addr = variables.len();
                variables.insert(name.to_string(), addr);
                return addr
            }
        }

        unreachable!("This should not be possible")
    }

    /**
    Retrieve address of a global variable.
    */
    pub fn get_global(&self, name: &str) -> Option<usize>
    {
        let variables = self.scopes.last().unwrap().variables.as_ref().unwrap();

        if let Some(addr) = variables.get(name) {
            Some(*addr)
        }
        else {
            None
        }
    }

    /** Set constant to name in current scope. */
    pub fn set_constant(&mut self, name: &str, value: RefValue) {
        self.scopes.first_mut().unwrap().constants.insert(
            name.to_string(), value
        );
    }

    /** Get constant value, either from current or preceding scope. */
    pub fn get_constant(&self, name: &str) -> Option<RefValue> {
        for scope in &self.scopes {
            if let Some(value) = scope.constants.get(name) {
                return Some(value.clone())
            }
        }

        None
    }

    /** Defines a new static value.

    Statics are moved into the program later on. */
    pub fn define_value(&self, value: RefValue) -> usize
    {
        let mut values = self.values.borrow_mut();
        // todo: check for existing value, and reuse it again instead of
        // naively adding the same value multiple times
        values.push(value);
        values.len() - 1
    }

    /** Defines a new parselet code element.

    Parselets are moved into the program later on. */
    pub fn define_parselet(&mut self, parselet: Parselet) -> usize
    {
        self.parselets.push(RefCell::new(parselet));
        self.parselets.len() - 1
    }

    /** Check if a str defines a constant or not. */
    pub fn is_constant(name: &str) -> bool {
        let ch = name.chars().nth(0).unwrap();
        ch.is_uppercase() || ch == '_'
    }

    pub fn gen_store(&mut self, name: &str) -> Op {
        if let Some(addr) = self.get_local(name) {
            Op::StoreFast(addr)
        }
        else if let Some(addr) = self.get_global(name) {
            Op::StoreGlobal(addr)
        }
        else {
            Op::StoreFast(self.new_local(name))
        }
    }

    pub fn gen_load(&mut self, name: &str) -> Op {
        if let Some(addr) = self.get_local(name) {
            Op::LoadFast(addr)
        }
        else if let Some(addr) = self.get_global(name) {
            Op::LoadGlobal(addr)
        }
        else {
            Op::LoadFast(self.new_local(name))
        }
    }

    /* Tokay AST node traversal */

    // Traverse something from the AST
    pub fn traverse(&mut self, value: &Value) -> Vec<Op> {
        let mut ret = Vec::new();

        if let Some(list) = value.get_list() {
            for item in list.iter() {
                let item = item.borrow();

                ret.extend(
                    self.traverse_node(&item.get_dict().unwrap())
                );
            }
        }
        else if let Some(dict) = value.get_dict() {
            ret.extend(self.traverse_node(dict));
        }
        else {
            unimplemented!("traverse() cannot traverse {:?}", value);
        }

        ret
    }

    // Traverse a value node into a RefValue instance
    fn traverse_node_value(&mut self, node: &Dict) -> RefValue {
        let emit = node.borrow_by_key("emit");
        let emit = emit.get_string().unwrap();

        match emit {
            "value_string" => {
                let value = node.borrow_by_key("value").to_string();
                Value::String(value)
            },
            "value_integer" => {
                let value = node.borrow_by_key("value").to_string();
                Value::Integer(
                    match value.parse::<i64>() {
                        Ok(i) => i,
                        Err(_) => 0
                    }
                )
            }
            "value_float" => {
                let value = node.borrow_by_key("value").to_string();
                Value::Float(
                    match value.parse::<f64>() {
                        Ok(f) => f,
                        Err(_) => 0.0
                    }
                )
            }
            "value_true" => Value::True,
            "value_false" => Value::False,
            "value_null" => Value::Null,
            "value_void" => Value::Void,
            "parselet" => {
                Value::Parselet(
                    self.traverse_node_parselet(node)
                )
            },
            _ => unimplemented!("unhandled value node {}", emit)
        }.into_ref()
    }

    // Traverse a parselet node into a parselet address
    fn traverse_node_parselet(&mut self, node: &Dict) -> usize {
        // todo: handle parameters BEFORE scope push
        self.push_scope(true);

        let parselets_start = self.parselets.len();

        let children = node.borrow_by_key("children");
        let body = self.traverse_node(&children.get_dict().unwrap());
        assert_eq!(body.len(), 1);

        let parselet = self.define_parselet(
            Parselet::new(
                body.into_iter().next().unwrap_or(Op::Nop), self.get_locals()
            )
        );

        // Resolve parselets defined from parselets_start
        for i in parselets_start..self.parselets.len() {
            self.parselets[i].borrow_mut().resolve(
                // resolve local variables in last defined parselet
                // (which was added some lines before!)
                &self, i == self.parselets.len() - 1, false
            );
        }

        self.pop_scope();

        parselet
    }

    // Main traversal function, running recursively through the AST
    pub fn traverse_node(&mut self, node: &Dict) -> Vec<Op> {
        // Normal node processing...
        let emit = node.borrow_by_key("emit");
        let emit = emit.get_string().unwrap();

        let mut ret = Vec::new();

        println!("emit = {:?}", emit);

        let op = match emit {
            // assign ---------------------------------------------------------
            "assign" => {
                let children = node.borrow_by_key("children");
                let children = children.get_list();

                let (lvalue, rvalue) = children.unwrap().borrow_first_2();

                let rvalue = rvalue.get_dict().unwrap();
                let lvalue = lvalue.get_dict().unwrap();

                ret.extend(self.traverse_node(rvalue));
                ret.extend(self.traverse_node(lvalue));

                None
            },

            // assign_constant ------------------------------------------------
            "assign_constant" => {
                let children = node.borrow_by_key("children");
                let children = children.get_list();

                let (constant, value) = children.unwrap().borrow_first_2();

                let constant = constant.get_dict().unwrap();
                let constant = constant.borrow_by_key("value");

                let value = self.traverse_node_value(value.get_dict().unwrap());
                self.set_constant(
                    constant.get_string().unwrap(),
                    value
                );

                None
            },

            // block ----------------------------------------------------------
            "block" => {
                if let Some(children) = node.get("children") {
                    let body = self.traverse(&children.borrow());
                    Some(Block::new(body))
                }
                else {
                    None
                }
            },

            // try_call --------------------------------------------------
            "try_call" => {
                let children = node.borrow_by_key("children");
                let ident = children.get_dict().unwrap();
                let ident = ident.borrow_by_key("value");
                let ident = ident.to_string();

                let mut item = Op::Name(ident);
                item.resolve(self, true, false);
                Some(item)
            },

            // lvalue ---------------------------------------------------------
            "lvalue" => {
                let mut list = List::new(); //todo... this looks ugly.

                let children = node.borrow_by_key("children");
                let children = children.get_list().unwrap_or_else(
                    || {
                        list.push(node["children"].clone());
                        &list
                    });

                for (i, item) in children.iter().enumerate() {
                    let item = item.borrow();
                    let item = item.get_dict().unwrap();

                    let emit = item.borrow_by_key("emit");
                    let emit = emit.get_string().unwrap();

                    match emit {
                        capture if capture.starts_with("capture") => {
                            let children = item.borrow_by_key("children");

                            match capture {
                                "capture" => {
                                    ret.extend(self.traverse(&children));
                                    ret.push(Op::StoreCapture)
                                }

                                "capture_index" => {
                                    let children = children.get_dict().unwrap();
                                    let index = self.traverse_node_value(children);
                                    ret.push(Op::StoreFastCapture(index.borrow().to_addr()));
                                }

                                "capture_alias" => {
                                    unimplemented!("//todo");
                                }

                                _ => {
                                    unreachable!();
                                }
                            }
                        }

                        "identifier" => {
                            let name = item.borrow_by_key("value");
                            let name = name.get_string().unwrap();

                            if self.get_constant(name).is_some() {
                                panic!("Cannot assign to {} as it is declared as constant", name)
                            }

                            if i < children.len() - 1 {
                                ret.push(self.gen_load(name))
                            }
                            else {
                                ret.push(self.gen_store(name))
                            }
                        },
                        other => {
                            unimplemented!(
                                "{:?} not implemented for lvalue", other);
                        }
                    }
                }

                None
            },

            // main -----------------------------------------------------------
            "main" => {
                let children = node.borrow_by_key("children");
                let main = self.traverse(&children);

                if main.len() > 0 {
                    self.define_parselet(
                        Parselet::new(
                            Block::new(main),
                            self.get_locals()
                        )
                    );
                }

                None
            },

            // match ----------------------------------------------------------
            "match" => {
                let value = node.borrow_by_key("value");
                Some(Match::new(value.get_string().unwrap().clone()))
            },

            // match_silent ---------------------------------------------------
            "match_silent" => {
                let value = node.borrow_by_key("value");
                Some(Match::new_silent(value.get_string().unwrap().clone()))
            },

            // modifier -------------------------------------------------------
            modifier if modifier.starts_with("mod_") => {
                let children = node.borrow_by_key("children");
                let op = self.traverse_node(children.get_dict().unwrap());
                assert_eq!(op.len(), 1);

                let op = op.into_iter().next().unwrap();

                match &modifier[4..] {
                    "peek" => Some(Op::Peek(op.into_box())),
                    "not" => Some(Op::Not(op.into_box())),
                    "kleene" => Some(op.into_kleene()),
                    "positive" => Some(op.into_positive()),
                    "optional" => Some(op.into_optional()),
                    _ => unimplemented!("{} not implemented", modifier)
                }
            },

            // operator ------------------------------------------------------

            op if op.starts_with("op_") => {
                let parts: Vec<&str> = emit.split("_").collect();

                if parts[1] == "binary" {
                    let children = node.borrow_by_key("children");
                    let children = children.get_list().unwrap();
                    assert_eq!(children.len(), 2);

                    let (left, right) = children.borrow_first_2();
                    ret.extend(self.traverse_node(&left.get_dict().unwrap()));
                    ret.extend(self.traverse_node(&right.get_dict().unwrap()));

                    match parts[2] {
                        "add" => Some(Op::Add),
                        "sub" => Some(Op::Sub),
                        "mul" => Some(Op::Mul),
                        "div" => Some(Op::Div),
                        _ => {
                            unimplemented!("op_binary_{}", parts[2]);
                        }
                    }
                }
                else if parts[1] == "unary" {
                    let children = node.borrow_by_key("children");
                    let children = children.get_dict().unwrap();
                    ret.extend(self.traverse_node(children));

                    //fixme: unary operators
                    unimplemented!("unary missing");
                    None
                }
                else if parts[1] == "accept" || parts[1] == "return" {
                    let children = node.borrow_by_key("children");
                    ret.extend(
                        self.traverse_node(
                            &children.get_dict().unwrap()
                        )
                    );

                    Some(Op::LoadAccept)
                }
                else {
                    None
                }
            }

            // parselet ------------------------------------------------------

            "parselet" => {
                None
            }

            // rvalue ---------------------------------------------------------
            "rvalue" => {
                let mut list = List::new(); //todo... this looks ugly.

                let children = node.borrow_by_key("children");
                let children = children.get_list().unwrap_or_else(
                    || {
                        list.push(node["children"].clone());
                        &list
                    });

                for (i, item) in children.iter().enumerate() {
                    let item = item.borrow();
                    let item = item.get_dict().unwrap();

                    let emit = item.borrow_by_key("emit");
                    let emit = emit.get_string().unwrap();

                    match emit {
                        capture if capture.starts_with("capture") => {
                            let children = item.borrow_by_key("children");

                            match capture {
                                "capture" => {
                                    ret.extend(self.traverse(&children));
                                    ret.push(Op::LoadCapture)
                                }

                                "capture_index" => {
                                    let children = children.get_dict().unwrap();
                                    let index = self.traverse_node_value(children);
                                    ret.push(Op::LoadFastCapture(index.borrow().to_addr()));
                                }

                                "capture_alias" => {
                                    unimplemented!("//todo");
                                }

                                _ => {
                                    unreachable!();
                                }
                            }
                        }

                        "identifier" => {
                            let name = item.borrow_by_key("value");
                            let name = name.get_string().unwrap();

                            if let Some(value) = self.get_constant(name) {
                                ret.push(
                                    Op::LoadStatic(self.define_value(value))
                                );
                            }
                            else {
                                ret.push(self.gen_load(name));
                            }
                        }

                        val if val.starts_with("value_") => {
                            ret.extend(self.traverse_node(item))
                        }

                        other => {
                            unreachable!();
                        }
                    }
                }

                None
            }

            // sequence ------------------------------------------------------

            "sequence" => {
                let children = node.borrow_by_key("children");
                let items = self.traverse(&children);
                //todo: Handle aliases...

                if items.len() > 0 {
                    Some(
                        Sequence::new(
                            items.into_iter()
                                .map(|item| (item, None))
                                .collect()
                        )
                    )
                }
                else {
                    None
                }
            },

            // value ---------------------------------------------------------

            val if val.starts_with("value_") => {
                let value = self.traverse_node_value(node);
                Some(Op::LoadStatic(self.define_value(value)))
            },

            // ---------------------------------------------------------------

            _ => {
                // When there are children, try to traverse recursively
                if let Some(children) = node.get("children") {
                    ret.extend(self.traverse(&children.borrow()));
                    None
                }
                // Otherwise, report unhandled node!
                else {
                    unreachable!("No handling for {:?}", emit);
                }
            }
        };

        if let Some(op) = op {
            ret.push(op);
        }

        ret
    }
}

/* A minimalistic Tokay compiler as Rust macros. */

#[macro_export]
macro_rules! tokay_item {

    // Assign a value
    ( $compiler:expr, ( $name:ident = $value:literal ) ) => {
        {
            let name = stringify!($name).to_string();
            let value = Value::String($value.to_string()).into_ref();

            if Compiler::is_constant(&name) {
                $compiler.set_constant(
                    &name,
                    value
                );

                None
            }
            else {
                let addr = $compiler.define_value(value);

                Some(
                    Sequence::new(
                        vec![
                            (Op::LoadStatic(addr), None),
                            ($compiler.gen_store(&name), None)
                        ]
                    )
                )
            }

            //println!("assign {} = {}", stringify!($name), stringify!($value));
        }
    };

    // Assign whitespace
    ( $compiler:expr, ( _ = { $( $item:tt ),* } ) ) => {
        {
            $compiler.push_scope(true);
            let parselets_start = $compiler.parselets.len();

            let items = vec![
                $(
                    tokay_item!($compiler, $item)
                ),*
            ];

            let body = Block::new(
                items.into_iter()
                    .filter(|item| item.is_some())
                    .map(|item| item.unwrap())
                    .collect()
            );

            let body = Repeat::new(body, 0, 0, true);

            let parselet = $compiler.define_parselet(
                Parselet::new_silent(body, $compiler.get_locals())
            );

            // Resolve parselets defined from parselets_start
            for i in parselets_start..$compiler.parselets.len() {
                $compiler.parselets[i].borrow_mut().resolve(
                    // resolve local variables in last defined parselet
                    // (which was added some lines before!)
                    &$compiler, i == $compiler.parselets.len() - 1, false
                );
            }

            $compiler.pop_scope();

            $compiler.set_constant(
                "_",
                Value::Parselet(parselet).into_ref()
            );

            //println!("assign _ = {}", stringify!($item));
            None
        }
    };

    // Assign parselet
    ( $compiler:expr, ( $name:ident = { $( $item:tt ),* } ) ) => {
        {
            let name = stringify!($name).to_string();

            $compiler.push_scope(true);
            let parselets_start = $compiler.parselets.len();

            let items = vec![
                $(
                    tokay_item!($compiler, $item)
                ),*
            ];

            let body = Block::new(
                items.into_iter()
                    .filter(|item| item.is_some())
                    .map(|item| item.unwrap())
                    .collect()
            );

            let parselet = $compiler.define_parselet(
                Parselet::new(body, $compiler.get_locals())
            );

            // Resolve parselets defined from parselets_start
            for i in parselets_start..$compiler.parselets.len() {
                $compiler.parselets[i].borrow_mut().resolve(
                    // resolve local variables in last defined parselet
                    // (which was added some lines before!)
                    &$compiler, i == $compiler.parselets.len() - 1, false
                );
            }

            $compiler.pop_scope();

            let parselet = Value::Parselet(parselet).into_ref();

            if Compiler::is_constant(&name) {
                $compiler.set_constant(
                    &name,
                    parselet
                );

                None
            }
            else {
                let addr = $compiler.define_value(parselet);

                Some(
                    Sequence::new(
                        vec![
                            (Op::LoadStatic(addr), None),
                            ($compiler.gen_store(&name), None)
                        ]
                    )
                )
            }

            //println!("assign {} = {}", stringify!($name), stringify!($item));
        }
    };

    // Sequence
    ( $compiler:expr, [ $( $item:tt ),* ] ) => {
        {
            //println!("sequence");
            let items = vec![
                $(
                    tokay_item!($compiler, $item)
                ),*
            ];

            Some(
                Sequence::new(
                    items.into_iter()
                        .filter(|item| item.is_some())
                        .map(|item| (item.unwrap(), None))
                        .collect()
                )
            )
        }
    };

    // Block
    ( $compiler:expr, { $( $item:tt ),* } ) => {
        {
            /*
            $(
                println!("{:?}", stringify!($item));
            )*
            */

            let items = vec![
                $(
                    tokay_item!($compiler, $item)
                ),*
            ];

            Some(
                Block::new(
                    items.into_iter()
                        .filter(|item| item.is_some())
                        .map(|item| item.unwrap())
                        .collect()
                )
            )
        }
    };

    // Kleene
    ( $compiler:expr, (kle $item:tt) ) => {
        Some(tokay_item!($compiler, $item).unwrap().into_kleene())
    };

    // Positive
    ( $compiler:expr, (pos $item:tt) ) => {
        Some(tokay_item!($compiler, $item).unwrap().into_positive())
    };

    // Optional
    ( $compiler:expr, (opt $item:tt) ) => {
        Some(tokay_item!($compiler, $item).unwrap().into_optional())
    };

    // Call
    ( $compiler:expr, $ident:ident ) => {
        {
            //println!("call = {}", stringify!($ident));
            let name = stringify!($ident);

            if Compiler::is_constant(name) {
                let mut item = Op::Name(name.to_string());
                item.resolve(&$compiler, true, false);
                Some(item)
            }
            else {
                Some(
                    Sequence::new(
                        vec![
                            ($compiler.gen_load(name), None),
                            (Op::TryCall, None)
                        ]
                    )
                )
            }
        }
    };

    // Whitespace
    ( $compiler:expr, _ ) => {
        {
            //println!("expr = {}", stringify!($expr));
            let mut item = Op::Name("_".to_string());
            item.resolve(&$compiler, false, false);
            Some(item)
        }
    };

    // Match / Touch
    ( $compiler:expr, $literal:literal ) => {
        {
            Some(Match::new_silent($literal))
        }
    };

    // Fallback
    ( $compiler:expr, $expr:tt ) => {
        {
            //println!("expr = {}", stringify!($expr));
            Some($expr)
        }
    };
}


#[macro_export]
macro_rules! tokay {
    ( $( $items:tt ),* ) => {
        {
            let mut compiler = Compiler::new();
            let main = tokay_item!(compiler, $( $items ),*);

            if let Some(main) = main {
                compiler.define_parselet(
                    Parselet::new(
                        main,
                        compiler.get_locals()
                    )
                );
            }

            compiler.into_program()
        }
    }
}
