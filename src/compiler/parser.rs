/*! Parser and grammar for Tokay, implemented using Tokay itself.

This module implements a Tokay parser in Tokay itself, using the compiler macros from the macros-module.
This is the general place to change syntax and modify the design of the abstract syntax tree.

There is also a grammar in examples/tokay.tok, which is this grammar expressed in Tokay.
When changing things here, do it there as well as this might become the reference parser someday.
*/
use charclass::charclass;

use super::*;
use crate::error::Error;
use crate::reader::Reader;
use crate::tokay;
use crate::value::{Dict, RefValue, Str, Token};

pub struct Parser(Program);

impl Parser {
    pub fn new() -> Self {
        Self(tokay!({
        // ----------------------------------------------------------------------------

        // Whitespace & EOL

        (_ = {  // true whitespace
            [(token (Token::Chars(charclass![' ', '\t'])))],
            ["#", (opt (token (Token::Chars(charclass!['\n'].negate()))))],
            ["\\", "\n"]
        }),

        (___ = {  // check for non-trailing identifier
            [(peek (not (token (Token::Char(charclass!['A' => 'Z', 'a' => 'z'] + charclass!['_']))))), _]
        }),

        (T_EOL = {  // end-of-line
            ["\n", _, (Op::Skip)],  // unix/linux
            ["\r", (opt "\n"), _, (Op::Skip)],  // classic mac & windows
            [";", _, (Op::Skip)],  // allows for multiple lines in one row
            [(token (Token::EOF)), (Op::Skip)],  // Input file may end without a new line
            [(peek "}"), (Op::Skip)]  // peek for '}' to allow for blocks in one line.
        }),

        // Escape sequences

        (T_OctDigit = {  // T_OctDigit is used by T_EscapeSequence
            (token (Token::Char(charclass!['0' => '7'])))
        }),

        (T_HexDigit = {    // T_HexDigit is used by T_EscapeSequence
            (token (Token::Char(charclass!['0' => '9', 'A' => 'F', 'a' => 'f'])))
        }),

        (T_EscapeSequence = {   // Parsing escape sequences
            ["a", (value "\x07")],
            ["b", (value "\x08")],
            ["f", (value "\x0c")],
            ["n", (value "\n")],
            ["r", (value "\r")],
            ["t", (value "\t")],
            ["v", (value "\x0b")],
            [T_OctDigit, T_OctDigit, T_OctDigit,
                (Op::Rust(Rust(|context| code_to_char(context, 0, 8))))],
            ["x", T_HexDigit, T_HexDigit,
                (Op::Rust(Rust(|context| code_to_char(context, 1, 16))))],
            ["u", T_HexDigit, T_HexDigit, T_HexDigit, T_HexDigit,
                (Op::Rust(Rust(|context| code_to_char(context, 1, 16))))],
            ["U", T_HexDigit, T_HexDigit, T_HexDigit, T_HexDigit,
                T_HexDigit, T_HexDigit, T_HexDigit, T_HexDigit,
                (Op::Rust(Rust(|context| code_to_char(context, 1, 16))))],
            Any
        }),

        // Prime Tokens

        (T_Identifier = {  // any identifier
            (call ast[(value "identifier"), (call Ident[])])
        }),

        (T_Consumable = {  // consumable identifier
            [
                (token (Token::Char(charclass!['A' => 'Z'] + charclass!['_']))),
                (opt (token (Token::Chars(charclass!['A' => 'Z', 'a' => 'z', '0' => '9'] + charclass!['_'])))),
                (call ast[(value "identifier"), (Op::LoadFastCapture(0))])
            ]
        }),

        (T_Alias = {  // T_Alias is an identifier treated as string value
            [
                (token (Token::Char(charclass!['A' => 'Z', 'a' => 'z'] + charclass!['_']))),
                (opt (token (Token::Chars(charclass!['A' => 'Z', 'a' => 'z', '0' => '9'] + charclass!['_'])))),
                (call ast[(value "value_string"), (Op::LoadFastCapture(0))])
            ]
        }),

        (T_String = {
            [
                "\"",  // a string
                (kle {
                    ["\\", T_EscapeSequence],
                    [(token (Token::Chars(charclass!['\\', '\"'].negate())))],
                    [EOF, (call error[(value "Unclosed string, expecting '\"'")])]
                }),
                (call str_join[(value ""), (Op::LoadFastCapture(2))]),
                (expect "\"")
            ]
        }),

        (T_Touch = {
            [
                "\'",  // a touch
                (kle {
                    ["\\", T_EscapeSequence],
                    [(token (Token::Chars(charclass!['\\', '\''].negate())))],
                    [EOF, (call error[(value "Unclosed match, expecting '\''")])]
                }),
                (call str_join[(value ""), (Op::LoadFastCapture(2))]),
                (expect "\'")
            ]
        }),

        (T_Integer = {
            [(call ast[(value "value_integer"), (call Int[])])]
        }),

        (T_Float = {
            [(call ast[(value "value_float"), (call Float[])])]
        }),

        // Character classes

        (CclChar = {
            ["\\", T_EscapeSequence],
            (token (Token::Char(charclass![']'].negate()))),
            [EOF, (call error[(value "Unclosed character-class, expecting ']'")])]
        }),

        (CclRange = {
            [CclChar, "-", CclChar,
                (call ast[
                    (value "range"),
                    [
                        (Op::LoadFastCapture(1)),
                        (Op::LoadFastCapture(3)),
                        (Op::BinaryOp("add"))
                    ]
                ])
            ],
            [CclChar, (call ast[(value "char")])]
        }),

        (Ccl = {
            ['^', (kle CclRange), (call ast[(value "ccl_neg")])],
            [(kle CclRange), (call ast[(value "ccl")])]
        }),

        // Statics, Variables & Constants

        (Subscript = {
            ["[", _, Expression, "]", _, (call ast[(value "index")])]
        }),

        (Attribute = {
            [".", _, (expect T_Alias), (call ast[(value "attribute")])]
        }),

        (Capture = {
            ["$", T_Alias, _, (call ast[(value "capture_alias")])],
            ["$", T_Integer, _, (call ast[(value "capture_index")])],
            ["$", "(", _, (kle [T_EOL, _]), Expression, ")", _, (call ast[(value "capture_expr")])],
            ["$", (call error[(value "'$': Expecting identifier, integer or (expression)")])]
        }),

        (Variable = {
            T_Identifier,
            Capture
        }),

        (Lvalue = {
            [Variable, _, (kle {
                // Attribute,  // Attribute assignment not required for now.
                Subscript
            }), (call ast[(value "lvalue")])]
        }),

        (Load = {
            [Lvalue, "++", (call ast[(value "inplace_post_inc")])],
            [Lvalue, "--", (call ast[(value "inplace_post_dec")])],
            ["++", (expect Lvalue), (call ast[(value "inplace_pre_inc")])],
            ["--", (expect Lvalue), (call ast[(value "inplace_pre_dec")])],
            Variable
        }),

        // Calls

        (CallParameter = {
            [T_Identifier, _, "=", _, (expect Expression), (call ast[(value "param_named")])],
            [Expression, (call ast[(value "param")])]
        }),

        (CallParameters = {
            (pos [CallParameter, (opt [",", _]), (kle [T_EOL, _])])
        }),

        // Literals

        (Literal = {
            /* below calls to ___ avoid to wrongly interpret e.g. "truex" as "true" and "x" */
            ["true", ___, (call ast[(value "value_true")])],
            ["false", ___, (call ast[(value "value_false")])],
            ["void", ___, (call ast[(value "value_void")])],
            ["null", ___, (call ast[(value "value_null")])],
            [T_String, (call ast[(value "value_string")])],
            T_Float,
            T_Integer
        }),

        // Inline sequences are used to construct lists and dicts as well

        (InlineSequenceItem = {
            [T_Alias, _, "=>", _, (expect Expression), (call ast[(value "alias")])],
            [Expression, "=>", _, (expect Expression), (call ast[(value "alias")])],
            Expression
        }),

        (InlineSequence = {
            // Special case: Expression followed by "," is considered as a list with a single item (syntactic sugar)
            [Expression, (kle [T_EOL, _]), ",", _, (kle [T_EOL, _]), (peek ")"), (call ast[(value "list")])],
            // A sequence is a list of items optionally separated by ","
            [(pos [InlineSequenceItem, (kle [T_EOL, _]), (opt [",", _]), (kle [T_EOL, _])]), (call ast[(value "sequence")])],
            // The empty sequences generates an empty list
            [Void, (call ast[(value "list")])]
        }),

        (InlineSequences = {
            // Multiple sequences delimited by "|" are an alternative form of the block syntax
            ["(", _, (kle [T_EOL, _]), InlineSequence,
                (pos [(kle [T_EOL, _]), "|", _, (kle [T_EOL, _]), InlineSequence]), (expect ")"),
                    (call ast[(value "block")])],
            // In case there's only a single sequence, handle it just as a sequence without a block
            ["(", _, (kle [T_EOL, _]), InlineSequence, (expect ")")]
        }),

        // Tokens

        (TokenLiteral = {
            ["'", T_Touch, "'", (call ast[(value "value_token_match")])],
            [T_Touch, (call ast[(value "value_token_touch")])],
            [".", (call ast[(value "value_token_any")])],
            ['[', Ccl, ']', (call ast[(value "value_token_ccl")])]
        }),

        (TokenCall = {
            TokenLiteral,
            [T_Consumable, "(", _, (kle [T_EOL, _]), (opt CallParameters), (kle [T_EOL, _]), (expect ")"),
                (call ast[(value "call")])],
            [T_Consumable, (call ast[(value "call")])],
            Parselet,
            InlineSequences,
            Block
        }),

        (Token = {
            // Token call modifiers
            [TokenCall, "+", (call ast[(value "op_mod_pos")])],
            [TokenCall, "*", (call ast[(value "op_mod_kle")])],
            [TokenCall, "?", (call ast[(value "op_mod_opt")])],
            // todo: {min}, {min, max} maybe with expression?
            TokenCall,
            ["peek", ___, (expect Token), (call ast[(value "op_mod_peek")])],
            ["not", ___, (expect Token), (call ast[(value "op_mod_not")])],
            ["expect", ___, (expect Token), (call ast[(value "op_mod_expect")])]
        }),

        // Expression & Flow

        (Atomic = {
            ["(", _, (kle [T_EOL, _]), Expression, (kle [T_EOL, _]), ")"], // no expect ")" here!
            Literal,
            Token,

            // if
            ["if", ___, Expression, (kle [T_EOL, _]), (expect Statement),
                (opt [(kle [T_EOL, _]), "else", ___, (kle [T_EOL, _]), (expect Statement)]),
                    (call ast[(value "op_if")])],

            // for
            //["for", ___, T_Identifier, _, "in", ___, Expression, Statement,
            //    (call ast[(value "op_for_in")])],
            ["for", ___, StatementOrEmpty, ";", _, StatementOrEmpty, ";", _, StatementOrEmpty,
                StatementOrEmpty, (call ast[(value "op_for")])],
            ["for", ___, (call error[(value "'for': Expecting start; condition; iter; statement")])],

            // loop
            ["loop", ___, Expression, _, Statement, (call ast[(value "op_loop")])],
            ["loop", ___, (expect Statement), (call ast[(value "op_loop")])],

            // standard load
            Load
        }),

        (Rvalue = {
            [Rvalue, "(", _, (kle [T_EOL, _]), (opt CallParameters), (expect ")"),
                (call ast[(value "call")])],
            [Rvalue, (kle {
                Attribute,
                Subscript
            }), (call ast[(value "rvalue")])],
            Atomic
        }),

        (Unary = {
            ["-", (not "-"), _, Unary, (call ast[(value "op_unary_neg")])],
            ["!", _, Unary, (call ast[(value "op_unary_not")])],
            [Rvalue, _]
        }),

        (MulDiv = {
            [MulDiv, "*", _, (expect Unary), (call ast[(value "op_binary_mul")])],
            [MulDiv, "/", _, (expect Unary), (call ast[(value "op_binary_div")])],
            Unary
        }),

        (AddSub = {
            [AddSub, "+", (not "+"), _, (expect MulDiv), // avoid matching "++"
                (call ast[(value "op_binary_add")])],
            [AddSub, "-", (not "-"), _, (expect MulDiv), // avoid matching "--"
                (call ast[(value "op_binary_sub")])],
            MulDiv
        }),

        (Compare = {
            [Compare, "==", _, (expect AddSub), (call ast[(value "op_compare_eq")])],
            [Compare, "!=", _, (expect AddSub), (call ast[(value "op_compare_neq")])],
            [Compare, "<=", _, (expect AddSub), (call ast[(value "op_compare_lteq")])],
            [Compare, ">=", _, (expect AddSub), (call ast[(value "op_compare_gteq")])],
            [Compare, "<", _, (expect AddSub), (call ast[(value "op_compare_lt")])],
            [Compare, ">", _, (expect AddSub), (call ast[(value "op_compare_gt")])],
            AddSub
        }),

        (LogicalAnd = {
            [LogicalAnd, "&&", _, (expect Compare), (call ast[(value "op_logical_and")])],
            Compare
        }),

        (LogicalOr = {
            [LogicalOr, "||", _, (expect LogicalAnd), (call ast[(value "op_logical_or")])],
            LogicalAnd
        }),

        (Expression = {

            // assignment
            [Lvalue, _, "=", (not {">", "="}), //avoid wrongly matching "=>" or "=="
                _, (expect Expression), (call ast[(value "assign_hold")])],
            [Lvalue, _, "+=", _, (expect Expression), (call ast[(value "assign_add_hold")])],
            [Lvalue, _, "-=", _, (expect Expression), (call ast[(value "assign_sub_hold")])],
            [Lvalue, _, "*=", _, (expect Expression), (call ast[(value "assign_mul_hold")])],
            [Lvalue, _, "/=", _, (expect Expression), (call ast[(value "assign_div_hold")])],

            // normal expression starting with LogicalOr
            LogicalOr
        }),

        (StatementOrEmpty = {
            Statement,
            (call ast[(value "op_nop")])
        }),

        (Statement = {
            ["accept", ___, (opt Expression), (call ast[(value "op_accept")])],
            ["break", ___, (opt Expression), (call ast[(value "op_break")])],
            ["continue", ___, (opt Expression), (call ast[(value "op_continue")])],
            ["exit", ___, (opt Expression), (call ast[(value "op_exit")])],
            ["next", ___, (call ast[(value "op_next")])],
            ["push", ___, (opt Expression), (call ast[(value "op_push")])],
            ["reject", ___, (call ast[(value "op_reject")])],
            ["repeat", ___, (opt Expression), (call ast[(value "op_repeat")])],
            ["return", ___, (opt Expression), (call ast[(value "op_accept")])],
            // todo: escape?

            [Lvalue, _, "=", (not {">", "="}), //avoid wrongly matching "=>" or "==" here
                _, (expect Expression), (call ast[(value "assign")])],
            [Lvalue, _, "+=", _, (expect Expression), (call ast[(value "assign_add")])],
            [Lvalue, _, "-=", _, (expect Expression), (call ast[(value "assign_sub")])],
            [Lvalue, _, "*=", _, (expect Expression), (call ast[(value "assign_mul")])],
            [Lvalue, _, "/=", _, (expect Expression), (call ast[(value "assign_div")])],

            Expression
        }),

        // Parselet

        (Argument = {
            [T_Identifier, _, (opt ["=", _, (opt Expression)]), (call ast[(value "arg")])]
        }),

        (Arguments = {
            (pos [Argument, (opt [",", _])])
        }),

        (Parselet = {
            ["@", _, (opt Arguments), Block, (call ast[(value "value_parselet")])],
            ["@", _, (opt Arguments), Token, (call ast[(value "value_parselet")])]
        }),

        (Block = {
            ["{", _, (kle [T_EOL, _]), "}", (call ast[(value "value_void")])],
            ["{", _, (pos Instruction), _, (expect "}"), (call ast[(value "block")])]
        }),

        // Sequences

        (SequenceItem = {
            [T_Alias, _, "=>", _, (expect Expression), (call ast[(value "alias")])],
            [Expression, "=>", _, (expect Expression), (call ast[(value "alias")])],
            Statement
        }),

        (Sequence = {
            [(pos [SequenceItem, (opt [",", _])]), (call ast[(value "sequence")])]
        }),

        (Sequences = {
            [Sequence, (pos ["|", _, Sequence]), (call ast[(value "block")])],
            Sequence
        }),

        (SequenceOrExpression = {
            [Expression, (peek T_EOL)],
            Sequences
        }),

        (Instruction = {
            ["begin", ___, Sequence, (expect T_EOL), (call ast[(value "begin")])],
            ["end", ___, Sequence, (expect T_EOL), (call ast[(value "end")])],

            [T_Identifier, _, ":", _, (expect SequenceOrExpression), (expect T_EOL),
                (call ast[(value "constant")])],
            Sequences,
            [T_EOL, (Op::Skip)]
        }),

        (Tokay = {
            (pos Instruction),
            [(token (Token::any())),
                (call error[(value "Parse error, unexpected token"), (value true)])]
        }),

        [_, (opt Tokay),
            (expect (token (Token::EOF)), "Parse error, expecting end-of-file"),
            (call ast[(value "main")])]

        // --- Test Environment -----------------------------------------------

        /*
        (T_EOL = {  // end-of-line
            [";", (Op::Skip)],
            [(token (Token::EOF)), (Op::Skip)],
            [(peek "}"), (Op::Skip)]
        }),

        (T_Integer = {
            [(token (Token::Chars(charclass!['0' => '9']))), (call ast[(value "value_integer")])]
        }),

        (Instruction = {
            T_Integer,
            [T_EOL, (Op::Skip)]
        }),

        (Block = {
            ["{", (pos Instruction), (expect "}"), (call ast[(value "block")])],
            ["{", (kle T_EOL), (expect "}"), (call ast[(value "value_void")])]
        }),

        [Block,
            (expect (token (Token::EOF)), "Parse error, expecting end-of-file"),
            (call ast[(value "main")])]
        */

        /*
        (T_Float = {
            [(token (Token::Chars(charclass!['0' => '9']))), ".", (opt (token (Token::Chars(charclass!['0' => '9'])))),
                (call ast[(value "value_float"), (Op::LoadFastCapture(0))])],
            [(opt (token (Token::Chars(charclass!['0' => '9'])))), ".", (token (Token::Chars(charclass!['0' => '9']))),
                (call ast[(value "value_float"), (Op::LoadFastCapture(0))])]
        }),

        T_Float
        */

        // ----------------------------------------------------------------------------
                    }))
    }

    pub fn parse(&self, mut reader: Reader) -> Result<RefValue, Error> {
        //self.0.dump();
        let mut runtime = Runtime::new(&self.0, &mut reader);

        if let Ok(level) = std::env::var("TOKAY_PARSER_DEBUG") {
            runtime.debug = level.parse::<u8>().unwrap_or_default();
        } else {
            runtime.debug = 0;
        }

        match self.0.run(&mut runtime) {
            Ok(Some(ast)) => {
                if ast.borrow().object::<Dict>().is_some() {
                    Ok(ast)
                } else {
                    Err(Error::new(None, "Parse error".to_string()))
                }
            }
            Ok(None) => Ok(crate::value!(void)),
            Err(error) => Err(error),
        }
    }
}

fn code_to_char(context: &mut Context, skip: u8, base: u32) -> Result<Accept, Reject> {
    let value = context.get_capture(0).unwrap();
    let value = value.borrow();
    let slice = &value.object::<Str>().unwrap().as_str()[skip as usize..];

    let code = if slice.len() <= 2 {
        u8::from_str_radix(slice, base).unwrap_or_default() as char
    } else {
        std::char::from_u32(u32::from_str_radix(slice, base).unwrap_or_default())
            .unwrap_or_default()
    };

    Ok(Accept::Return(Some(RefValue::from(format!("{}", code)))))
}

#[test]
// EOL
fn parser_eol() {
    for eol in ["\n", "\r", "\r\n", ";"] {
        let tok = format!("a = 1{}a + 2", eol);
        println!("EOL test {:?}", tok);
        assert_eq!(crate::run(&tok, ""), Ok(Some(crate::value!(3))));
    }
}

// Tests for parsing and packrat features ---------------------------------------------------------

/*
    Below are some tests that provide indirect left-recursion.

    They currently don't work properly due to the following reason:
    For indirect left-recursion in packrat parsing, one rule in the
    grammar's graph must be declared as "leading", so that subsequent,
    even left-recursive parselets are considered as not left-recursive.


    An implementation of an solution for this issue can be found in
    the pegen parser generator from Python:

    https://github.com/python/cpython/blob/main/Tools/peg_generator/pegen/parser_generator.py

    Tokay won't take care of this right now as it is an edge-case
    and also more complex, as Tokay does not directly implements a
    grammar.
*/

#[test]
fn parser_indirectleftrec() {
    let program = tokay!({
        (X = {
            [Y, (MATCH "c")]
        }),
        (Y = {
            [Z, (MATCH "b")]
            //Void
        }),
        (Z = {
            X,
            Y,
            (MATCH "a")
        }),
        Z
    });

    println!("{:#?}", program.run_from_str("aaabc"));
}

#[test]
fn parser_leftrec() {
    /*
    let program = tokay!({
        (X = {
            [X, (MATCH "b")],
            (MATCH "a")
        }),

        X
    });
    */

    let program = tokay!({
        (Y = {
            X,
            (MATCH "a")
        }),
        (X = {
            [Y, (MATCH "b")]
        }),
        X
    });

    /*
    let program = tokay!({
        (Factor = {
            ["(", (pos [Expression]), ")"],
            (token (Token::Chars(charclass!['0'..='9'])))
        }),
        (Expression = {
            [Expression, "+", Expression],
            Factor
        }),
        Expression
    });
    */

    println!("{:#?}", program.run_from_str("abb"));
}
