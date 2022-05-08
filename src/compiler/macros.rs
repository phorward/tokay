/*! Macros for Rust-internal Tokay program definition

    This is a minimalistic Tokay compiler implemented with Rust macros to
    bootstrap the self-hosted Tokay parser defined in the parser-module.

    It can also be used for low-level tests of the VM's parsing behavior,
    see tests.
*/

/** Macro to define a Tokay program embedded in Rust code without using the Tokay parser.
In fact, this macro is used to bootstrap the Tokay parser as a Tokay program itself.
*/
#[macro_export]
macro_rules! tokay {

    // Tokay
    ( { $( $items:tt ),+ } ) => {
        {
            let mut compiler = Compiler::new();
            compiler.debug = 0;  // unset debug always

            //tokay_dump!({ $( $items ),* });

            compiler.push_parselet();  // Main
            compiler.mark_consuming();

            let main = tokay!(compiler, { $( $items ),* });

            let parselet = compiler.pop_parselet(
                Some("__main__".to_string()),
                Vec::new(),
                main.unwrap_or(ImlOp::Nop)
            );

            compiler.define_value(parselet.into());  // Define main parselet

            match compiler.to_program() {
                Ok(program) => {
                    if compiler.debug > 0 {
                        program.dump();
                    }
                    program
                },
                Err(errors) => {
                    for error in errors {
                        println!("{}", error);
                    }

                    panic!("Errors in compile!");
                }
            }
        }
    };

    // Assign a value
    ( $compiler:expr, ( $name:ident = $value:literal ) ) => {
        {
            let name = stringify!($name).to_string();
            let value = Value::Str($value.to_string()).into();

            $compiler.set_constant(&name, value);

            //println!("assign {} = {}", stringify!($name), stringify!($value));
            None
        }
    };

    // Assign an identifier
    ( $compiler:expr, ( $name:ident = $value:ident ) ) => {
        {
            let name = stringify!($name).to_string();
            let value = $compiler.get_constant(stringify!($value)).unwrap();

            $compiler.set_constant(&name, value);

            None
        }
    };

    // Assign whitespace
    ( $compiler:expr, ( _ = { $( $item:tt ),* } ) ) => {
        {
            $compiler.push_parselet();
            $compiler.mark_consuming();

            let items = vec![
                $(
                    tokay!($compiler, $item)
                ),*
            ];

            let body = ImlAlternation::new(
                items.into_iter()
                    .filter(|item| item.is_some())
                    .map(|item| item.unwrap())
                    .collect()
            );

            let mut parselet = $compiler.pop_parselet(
                Some("_".to_string()),
                Vec::new(),
                body
            );

            parselet.severity = 0;  // mark as silent parselet
            $compiler.set_constant("_", parselet.into());

            //println!("assign _ = {}", stringify!($item));
            None
        }
    };

    // Assign parselet
    ( $compiler:expr, ( $name:ident = { $( $item:tt ),* } ) ) => {
        {
            let name = stringify!($name).to_string();

            if !crate::utils::identifier_is_consumable(&name) {
                panic!("Parselet identifier must begin with an upper-case letter or underscore!");
            }

            $compiler.push_parselet();
            $compiler.mark_consuming();

            let items = vec![
                $(
                    tokay!($compiler, $item)
                ),*
            ];

            let body = ImlAlternation::new(
                items.into_iter()
                    .filter(|item| item.is_some())
                    .map(|item| item.unwrap())
                    .collect()
            );

            let parselet = $compiler.pop_parselet(
                Some(stringify!($name).to_string()),
                Vec::new(),
                body
            );

            $compiler.set_constant(&name, parselet.into());

            None
        }
    };

    // Sequence
    ( $compiler:expr, [ $( $item:tt ),* ] ) => {
        {
            //println!("sequence");
            let items = vec![
                $(
                    tokay!($compiler, $item)
                ),*
            ];

            Some(
                ImlSequence::new(
                    items.into_iter()
                        .filter(|item| item.is_some())
                        .map(|item| item.unwrap())
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
                    tokay!($compiler, $item)
                ),*
            ];

            Some(
                ImlAlternation::new(
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
        Some(tokay!($compiler, $item).unwrap().into_kleene())
    };

    // Positive
    ( $compiler:expr, (pos $item:tt) ) => {
        Some(tokay!($compiler, $item).unwrap().into_positive())
    };

    // Optional
    ( $compiler:expr, (opt $item:tt) ) => {
        Some(tokay!($compiler, $item).unwrap().into_optional())
    };

    // Not
    ( $compiler:expr, (not $item:tt) ) => {
        Some(ImlNot::new(tokay!($compiler, $item).unwrap()))
    };

    // Peek
    ( $compiler:expr, (peek $item:tt) ) => {
        Some(ImlPeek::new(tokay!($compiler, $item).unwrap()))
    };

    // Expect
    ( $compiler:expr, (expect $item:tt) ) => {
        Some(
            ImlExpect::new(
                tokay!($compiler, $item).unwrap(),
                Some(format!("Expecting {}", stringify!($item)))
            )
        )
    };

    // Expect with literal
    ( $compiler:expr, (expect $item:tt, $msg:literal) ) => {
        Some(ImlExpect::new(tokay!($compiler, $item).unwrap(), Some($msg.to_string())))
    };

    // Value
    ( $compiler:expr, (value $value:tt) ) => {
        Some(ImlOp::from(Op::LoadStatic($compiler.define_value($crate::value!($value).into()))))
    };

    // Token
    ( $compiler:expr, (token $token:tt) ) => {
        {
            Some(ImlOp::from(Op::CallStatic($compiler.define_value(RefValue::from($token).into()))))
        }
    };

    // Call with parameters
    ( $compiler:expr, (call $ident:ident [ $( $param:tt ),* ] ) ) => {
        {
            let mut items = vec![
                $(
                    tokay!($compiler, $param).unwrap()
                ),*
            ];

            let name = stringify!($ident).to_string();

            let item = Usage::Call{
                name,
                args: items.len(),
                nargs: 0,
                offset: None
            }.resolve_or_dispose(&mut $compiler);

            items.extend(item);

            //println!("call = {} {:?}", stringify!($ident), items);
            Some(ImlOp::from_vec(items))
        }
    };

    // Call without parameters
    ( $compiler:expr, $ident:ident ) => {
        {
            //println!("call = {}", stringify!($ident));
            let name = stringify!($ident);

            let item = Usage::CallOrCopy{
                name: name.to_string(),
                offset: None
            }.resolve_or_dispose(&mut $compiler);

            Some(ImlOp::from_vec(item))
        }
    };

    // Whitespace
    ( $compiler:expr, _ ) => {
        {
            //println!("expr = {}", stringify!($expr));
            let item = Usage::CallOrCopy{
                name: "_".to_string(),
                offset: None
            }.resolve_or_dispose(&mut $compiler);

            assert!(item.len() == 1); // Can only process statics here!
            Some(item.into_iter().next().unwrap())
        }
    };

    // Match
    ( $compiler:expr, (MATCH $literal:literal) ) => {
        {
            let token = RefValue::from(Token::Match($literal.to_string()));
            Some(ImlOp::from(Op::CallStatic($compiler.define_value(token.into()))))
        }
    };

    // Touch
    ( $compiler:expr, $literal:literal ) => {
        {
            let token = RefValue::from(Token::Touch($literal.to_string()));
            Some(ImlOp::from(Op::CallStatic($compiler.define_value(token.into()))))
        }
    };

    // Fallback
    ( $compiler:expr, $expr:tt ) => {
        {
            //println!("expr = {}", stringify!($expr));
            Some(ImlOp::from($expr))
        }
    };
}

/*
// Grammar dump macro, used to generate examples/tokay.tok.
#[macro_export]
macro_rules! tokay_dump {

    // Tokay
    ( { $( $item:tt ),+ } ) => {
        $(
            println!("{}", tokay_dump!(0, $item));
        )*
    };

    // Assign a value
    ( $indent:expr, ( $name:ident = $value:literal ) ) => {
        format!("{:indent$}{} : {}",
            stringify!($name),
            stringify!($value),
            indent=$indent * 4
        )
    };

    // Assign whitespace
    ( $indent:expr, ( _ = { $( $item:tt ),* } ) ) => {
        format!(
            "_ : @{}\n",
            tokay_dump!($indent, { $( $item ),* })
        )
    };

    // Assign parselet
    ( $indent:expr, ( $name:ident = { $( $item:tt ),* } ) ) => {
        format!(
            "{} : @{}\n",
            stringify!($name),
            tokay_dump!($indent, { $( $item ),* })
        )
    };

    // Sequence
    ( $indent:expr, [ $( $item:tt ),* ] ) => {
        {
            let mut ret = (if $indent > 2 {"("} else {""}).to_string();
            ret += &[ $( tokay_dump!($indent + 1, $item) ),* ].join(" ");
            ret += if $indent > 2 {")"} else {""};
            ret
        }
    };

    // Block
    ( $indent:expr, { $( $item:tt ),* } ) => {
        {
            let mut ret = "{\n".to_string();
            let mut indent = $indent;

            if indent > 1 {
                indent -= 1;
            }
            else {
                indent = 1;
            }

            $(
                ret += &format!(
                    "{}{}\n",
                    "    ".repeat(indent),
                    tokay_dump!(indent + 1, $item)
                );
            )*

            ret += &format!(
                "{}}}",
                "    ".repeat(indent - 1)
            );

            ret
        }
    };

    // Kleene
    ( $indent:expr, (kle $item:tt) ) => {
        format!("{}*", tokay_dump!($indent + 1, $item))
    };

    // Positive
    ( $indent:expr, (pos $item:tt) ) => {
        format!("{}+", tokay_dump!($indent + 1, $item))
    };

    // Optional
    ( $indent:expr, (opt $item:tt) ) => {
        format!("{}?", tokay_dump!($indent + 1, $item))
    };

    // Not
    ( $indent:expr, (not $item:tt) ) => {
        format!("not {}", tokay_dump!($indent + 1, $item))
    };

    // Peek
    ( $indent:expr, (peek $item:tt) ) => {
        format!("peek {}", tokay_dump!($indent + 1, $item))
    };

    // Expect
    ( $indent:expr, (expect $item:tt) ) => {
        format!("expect {}", tokay_dump!($indent + 1, $item))
    };

    // Expect with literal
    ( $indent:expr, (expect $item:tt, $msg:literal) ) => {
        format!("expect {}, \"{}\"", tokay_dump!($indent + 1, $item), $msg)
    };

    // Value
    ( $indent:expr, (value $value:tt) ) => {
        stringify!($value).to_string()
    };

    // Token
    ( $indent:expr, (token $token:tt) ) => {
        $token.into_value().repr()
    };

    // Call with parameters
    ( $indent:expr, (call $ident:ident [ $( $param:tt ),* ] ) ) => {
        format!(" {}({})", stringify!($ident), [ $( tokay_dump!($indent, $param) ),* ].join(", "))
    };

    // Call without parameters
    ( $indent:expr, $ident:ident ) => {
        stringify!($ident).to_string()
    };

    // Whitespace
    ( $indent:expr, _ ) => {
        "_".to_string()
    };

    // Touch
    ( $indent:expr, $literal:literal ) => {
        format!("'{}'", $literal)
    };

    // $<offset>
    ( $indent:expr, (Op::LoadFastCapture($offset:literal)) ) => {
        format!("${}", $offset)
    };

    // Fallback
    ( $indent:expr, $expr:tt ) => {
        "".to_string()
    };
}
*/
