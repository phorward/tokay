//! Tokay parser, implemented in Tokay itself.

use super::*;
use crate::error::Error;
use crate::reader::Reader;
use crate::value;
use crate::value::{Dict, RefValue};

pub struct Parser(Program);

impl Parser {
    pub fn new() -> Self {
        // fixme: Make this lazy_static, so its created only once!
        let ast =
            // First of all: DON'T PANIC!

            // Below code blob between the markers GENERATE and ETARENEG (which is "GENERATE" reversed!)
            // is injected by Tokay itself, by running the program `tokay.tok` with tokay, parsing itself.
            // This generates an abstract syntax tree (AST) representation of `tokay.tok` in form of
            // value!-macro calls, which is injected below between the two markers.

            // If something goes wrong, it is important to keep a working copy of this file in Git
            // to have a working version of the parser at hand before its automatical replacement.
            // The best way is to test grammar changes with `tokay.tok` intensely before rebuilding the
            // parser, to ensure all runs well.

            // To update this file, `cd build` and run `make parser` in a shell.

            /*GENERATE cargo run -- "`sed 's/ast("main")/ast2rust(ast("main"), level=3)/g' compiler/tokay.tok`" -- compiler/tokay.tok */
            value!([
                "emit" => "main",
                "children" =>
                    (value!([
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "_"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "op_mod_pos",
                                                            "children" =>
                                                                (value!([
                                                                    "emit" => "value_token_ccl",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "ccl",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "char",
                                                                                        "value" => "\t"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "char",
                                                                                        "value" => " "
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "#"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_kle",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "value_token_ccl",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "ccl_neg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "char",
                                                                                                "value" => "\n"
                                                                                            ]))
                                                                                    ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "\\"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_opt",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "\r"
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "\n"
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "___"
                                    ])),
                                    (value!([
                                        "emit" => "op_mod_kle",
                                        "children" =>
                                            (value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_EOL"
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "T_EOL"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "\n"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "\r"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_opt",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "\n"
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => ";"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "op_accept",
                                                            "children" =>
                                                                (value!([
                                                                    "emit" => "value_instance",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Peek"
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "instarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "value_token_touch",
                                                                                        "value" => "}"
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "op_accept",
                                                            "children" =>
                                                                (value!([
                                                                    "emit" => "value_instance",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Peek"
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "instarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "EOF"
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "T_OctDigit"
                                    ])),
                                    (value!([
                                        "emit" => "value_token_ccl",
                                        "children" =>
                                            (value!([
                                                "emit" => "ccl",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "range",
                                                        "value" => "07"
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "T_HexDigit"
                                    ])),
                                    (value!([
                                        "emit" => "value_token_ccl",
                                        "children" =>
                                            (value!([
                                                "emit" => "ccl",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "range",
                                                            "value" => "09"
                                                        ])),
                                                        (value!([
                                                            "emit" => "range",
                                                            "value" => "AF"
                                                        ])),
                                                        (value!([
                                                            "emit" => "range",
                                                            "value" => "af"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "T_EscapeSequence"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "a"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_string",
                                                                        "value" => ""
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "b"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_string",
                                                                        "value" => ""
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "f"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_string",
                                                                        "value" => ""
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "n"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_string",
                                                                        "value" => "\n"
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "r"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_string",
                                                                        "value" => "\r"
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "t"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_string",
                                                                        "value" => "\t"
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "v"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_string",
                                                                        "value" => ""
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_OctDigit"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_OctDigit"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_OctDigit"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "chr"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "op_binary_add",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "op_binary_add",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "op_binary_mul",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            (value!([
                                                                                                                                "emit" => "call",
                                                                                                                                "children" =>
                                                                                                                                    (value!([
                                                                                                                                        (value!([
                                                                                                                                            "emit" => "identifier",
                                                                                                                                            "value" => "int"
                                                                                                                                        ])),
                                                                                                                                        (value!([
                                                                                                                                            "emit" => "callarg",
                                                                                                                                            "children" =>
                                                                                                                                                (value!([
                                                                                                                                                    "emit" => "capture_index",
                                                                                                                                                    "children" =>
                                                                                                                                                        (value!([
                                                                                                                                                            "emit" => "value_integer",
                                                                                                                                                            "value" => 1
                                                                                                                                                        ]))
                                                                                                                                                ]))
                                                                                                                                        ]))
                                                                                                                                    ]))
                                                                                                                            ])),
                                                                                                                            (value!([
                                                                                                                                "emit" => "value_integer",
                                                                                                                                "value" => 64
                                                                                                                            ]))
                                                                                                                        ]))
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "op_binary_mul",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            (value!([
                                                                                                                                "emit" => "call",
                                                                                                                                "children" =>
                                                                                                                                    (value!([
                                                                                                                                        (value!([
                                                                                                                                            "emit" => "identifier",
                                                                                                                                            "value" => "int"
                                                                                                                                        ])),
                                                                                                                                        (value!([
                                                                                                                                            "emit" => "callarg",
                                                                                                                                            "children" =>
                                                                                                                                                (value!([
                                                                                                                                                    "emit" => "capture_index",
                                                                                                                                                    "children" =>
                                                                                                                                                        (value!([
                                                                                                                                                            "emit" => "value_integer",
                                                                                                                                                            "value" => 2
                                                                                                                                                        ]))
                                                                                                                                                ]))
                                                                                                                                        ]))
                                                                                                                                    ]))
                                                                                                                            ])),
                                                                                                                            (value!([
                                                                                                                                "emit" => "value_integer",
                                                                                                                                "value" => 8
                                                                                                                            ]))
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "call",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "identifier",
                                                                                                                    "value" => "int"
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "callarg",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "capture_index",
                                                                                                                            "children" =>
                                                                                                                                (value!([
                                                                                                                                    "emit" => "value_integer",
                                                                                                                                    "value" => 3
                                                                                                                                ]))
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "x"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_HexDigit"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_HexDigit"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "chr"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "call",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "int"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "callarg",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                "emit" => "op_binary_add",
                                                                                                                "children" =>
                                                                                                                    (value!([
                                                                                                                        (value!([
                                                                                                                            "emit" => "value_string",
                                                                                                                            "value" => "0x"
                                                                                                                        ])),
                                                                                                                        (value!([
                                                                                                                            "emit" => "call",
                                                                                                                            "children" =>
                                                                                                                                (value!([
                                                                                                                                    (value!([
                                                                                                                                        "emit" => "rvalue",
                                                                                                                                        "children" =>
                                                                                                                                            (value!([
                                                                                                                                                (value!([
                                                                                                                                                    "emit" => "capture_index",
                                                                                                                                                    "children" =>
                                                                                                                                                        (value!([
                                                                                                                                                            "emit" => "value_integer",
                                                                                                                                                            "value" => 0
                                                                                                                                                        ]))
                                                                                                                                                ])),
                                                                                                                                                (value!([
                                                                                                                                                    "emit" => "attribute",
                                                                                                                                                    "children" =>
                                                                                                                                                        (value!([
                                                                                                                                                            "emit" => "value_string",
                                                                                                                                                            "value" => "substr"
                                                                                                                                                        ]))
                                                                                                                                                ]))
                                                                                                                                            ]))
                                                                                                                                    ])),
                                                                                                                                    (value!([
                                                                                                                                        "emit" => "callarg",
                                                                                                                                        "children" =>
                                                                                                                                            (value!([
                                                                                                                                                "emit" => "value_integer",
                                                                                                                                                "value" => 1
                                                                                                                                            ]))
                                                                                                                                    ]))
                                                                                                                                ]))
                                                                                                                        ]))
                                                                                                                    ]))
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "u"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_HexDigit"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_HexDigit"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_HexDigit"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_HexDigit"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "chr"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "call",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "int"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "callarg",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                "emit" => "op_binary_add",
                                                                                                                "children" =>
                                                                                                                    (value!([
                                                                                                                        (value!([
                                                                                                                            "emit" => "value_string",
                                                                                                                            "value" => "0x"
                                                                                                                        ])),
                                                                                                                        (value!([
                                                                                                                            "emit" => "call",
                                                                                                                            "children" =>
                                                                                                                                (value!([
                                                                                                                                    (value!([
                                                                                                                                        "emit" => "rvalue",
                                                                                                                                        "children" =>
                                                                                                                                            (value!([
                                                                                                                                                (value!([
                                                                                                                                                    "emit" => "capture_index",
                                                                                                                                                    "children" =>
                                                                                                                                                        (value!([
                                                                                                                                                            "emit" => "value_integer",
                                                                                                                                                            "value" => 0
                                                                                                                                                        ]))
                                                                                                                                                ])),
                                                                                                                                                (value!([
                                                                                                                                                    "emit" => "attribute",
                                                                                                                                                    "children" =>
                                                                                                                                                        (value!([
                                                                                                                                                            "emit" => "value_string",
                                                                                                                                                            "value" => "substr"
                                                                                                                                                        ]))
                                                                                                                                                ]))
                                                                                                                                            ]))
                                                                                                                                    ])),
                                                                                                                                    (value!([
                                                                                                                                        "emit" => "callarg",
                                                                                                                                        "children" =>
                                                                                                                                            (value!([
                                                                                                                                                "emit" => "value_integer",
                                                                                                                                                "value" => 1
                                                                                                                                            ]))
                                                                                                                                    ]))
                                                                                                                                ]))
                                                                                                                        ]))
                                                                                                                    ]))
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "U"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_HexDigit"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_HexDigit"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_HexDigit"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_HexDigit"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_HexDigit"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_HexDigit"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_HexDigit"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_HexDigit"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "chr"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "call",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "int"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "callarg",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                "emit" => "op_binary_add",
                                                                                                                "children" =>
                                                                                                                    (value!([
                                                                                                                        (value!([
                                                                                                                            "emit" => "value_string",
                                                                                                                            "value" => "0x"
                                                                                                                        ])),
                                                                                                                        (value!([
                                                                                                                            "emit" => "call",
                                                                                                                            "children" =>
                                                                                                                                (value!([
                                                                                                                                    (value!([
                                                                                                                                        "emit" => "rvalue",
                                                                                                                                        "children" =>
                                                                                                                                            (value!([
                                                                                                                                                (value!([
                                                                                                                                                    "emit" => "capture_index",
                                                                                                                                                    "children" =>
                                                                                                                                                        (value!([
                                                                                                                                                            "emit" => "value_integer",
                                                                                                                                                            "value" => 0
                                                                                                                                                        ]))
                                                                                                                                                ])),
                                                                                                                                                (value!([
                                                                                                                                                    "emit" => "attribute",
                                                                                                                                                    "children" =>
                                                                                                                                                        (value!([
                                                                                                                                                            "emit" => "value_string",
                                                                                                                                                            "value" => "substr"
                                                                                                                                                        ]))
                                                                                                                                                ]))
                                                                                                                                            ]))
                                                                                                                                    ])),
                                                                                                                                    (value!([
                                                                                                                                        "emit" => "callarg",
                                                                                                                                        "children" =>
                                                                                                                                            (value!([
                                                                                                                                                "emit" => "value_integer",
                                                                                                                                                "value" => 1
                                                                                                                                            ]))
                                                                                                                                    ]))
                                                                                                                                ]))
                                                                                                                        ]))
                                                                                                                    ]))
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "value_token_any"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "T_Identifier"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "call",
                                                        "children" =>
                                                            (value!([
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "ast"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "value_string",
                                                                            "value" => "identifier"
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Ident"
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "T_Consumable"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "sequence",
                                                        "children" =>
                                                            (value!([
                                                                (value!([
                                                                    "emit" => "value_token_ccl",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "ccl",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "range",
                                                                                        "value" => "AZ"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "char",
                                                                                        "value" => "_"
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "op_mod_kle",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "value_token_ccl",
                                                                            "children" =>
                                                                                (value!([
                                                                                    "emit" => "ccl",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            (value!([
                                                                                                "emit" => "range",
                                                                                                "value" => "09"
                                                                                            ])),
                                                                                            (value!([
                                                                                                "emit" => "range",
                                                                                                "value" => "AZ"
                                                                                            ])),
                                                                                            (value!([
                                                                                                "emit" => "char",
                                                                                                "value" => "_"
                                                                                            ])),
                                                                                            (value!([
                                                                                                "emit" => "range",
                                                                                                "value" => "az"
                                                                                            ]))
                                                                                        ]))
                                                                                ]))
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "call",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "ast"
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "callarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "value_string",
                                                                                        "value" => "identifier"
                                                                                    ]))
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "callarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "capture_index",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "value_integer",
                                                                                                "value" => 0
                                                                                            ]))
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "T_Alias"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "sequence",
                                                        "children" =>
                                                            (value!([
                                                                (value!([
                                                                    "emit" => "value_token_ccl",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "ccl",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "range",
                                                                                        "value" => "AZ"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "char",
                                                                                        "value" => "_"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "range",
                                                                                        "value" => "az"
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "op_mod_kle",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "value_token_ccl",
                                                                            "children" =>
                                                                                (value!([
                                                                                    "emit" => "ccl",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            (value!([
                                                                                                "emit" => "range",
                                                                                                "value" => "09"
                                                                                            ])),
                                                                                            (value!([
                                                                                                "emit" => "range",
                                                                                                "value" => "AZ"
                                                                                            ])),
                                                                                            (value!([
                                                                                                "emit" => "char",
                                                                                                "value" => "_"
                                                                                            ])),
                                                                                            (value!([
                                                                                                "emit" => "range",
                                                                                                "value" => "az"
                                                                                            ]))
                                                                                        ]))
                                                                                ]))
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "call",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "ast"
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "callarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "value_string",
                                                                                        "value" => "value_string"
                                                                                    ]))
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "callarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "capture_index",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "value_integer",
                                                                                                "value" => 0
                                                                                            ]))
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "T_String"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "sequence",
                                                        "children" =>
                                                            (value!([
                                                                (value!([
                                                                    "emit" => "value_token_touch",
                                                                    "value" => "\""
                                                                ])),
                                                                (value!([
                                                                    "emit" => "op_mod_kle",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "block",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "sequence",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                (value!([
                                                                                                    "emit" => "value_token_touch",
                                                                                                    "value" => "\\"
                                                                                                ])),
                                                                                                (value!([
                                                                                                    "emit" => "identifier",
                                                                                                    "value" => "T_EscapeSequence"
                                                                                                ]))
                                                                                            ]))
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "value_token_ccl",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "ccl_neg",
                                                                                                "children" =>
                                                                                                    (value!([
                                                                                                        (value!([
                                                                                                            "emit" => "char",
                                                                                                            "value" => "\\"
                                                                                                        ])),
                                                                                                        (value!([
                                                                                                            "emit" => "char",
                                                                                                            "value" => "\""
                                                                                                        ]))
                                                                                                    ]))
                                                                                            ]))
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "sequence",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                (value!([
                                                                                                    "emit" => "identifier",
                                                                                                    "value" => "EOF"
                                                                                                ])),
                                                                                                (value!([
                                                                                                    "emit" => "call",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            (value!([
                                                                                                                "emit" => "identifier",
                                                                                                                "value" => "error"
                                                                                                            ])),
                                                                                                            (value!([
                                                                                                                "emit" => "callarg",
                                                                                                                "children" =>
                                                                                                                    (value!([
                                                                                                                        "emit" => "value_string",
                                                                                                                        "value" => "Unclosed string, expecting '\"'"
                                                                                                                    ]))
                                                                                                            ]))
                                                                                                        ]))
                                                                                                ]))
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "call",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "str_join"
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "callarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "value_string",
                                                                                        "value" => ""
                                                                                    ]))
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "callarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "capture_index",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "value_integer",
                                                                                                "value" => 2
                                                                                            ]))
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "value_instance",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Expect"
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "instarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "value_token_touch",
                                                                                        "value" => "\""
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "T_Touch"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "sequence",
                                                        "children" =>
                                                            (value!([
                                                                (value!([
                                                                    "emit" => "value_token_touch",
                                                                    "value" => "'"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "op_mod_kle",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "block",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "sequence",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                (value!([
                                                                                                    "emit" => "value_token_touch",
                                                                                                    "value" => "\\"
                                                                                                ])),
                                                                                                (value!([
                                                                                                    "emit" => "identifier",
                                                                                                    "value" => "T_EscapeSequence"
                                                                                                ]))
                                                                                            ]))
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "value_token_ccl",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "ccl_neg",
                                                                                                "children" =>
                                                                                                    (value!([
                                                                                                        (value!([
                                                                                                            "emit" => "char",
                                                                                                            "value" => "\\"
                                                                                                        ])),
                                                                                                        (value!([
                                                                                                            "emit" => "char",
                                                                                                            "value" => "'"
                                                                                                        ]))
                                                                                                    ]))
                                                                                            ]))
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "sequence",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                (value!([
                                                                                                    "emit" => "identifier",
                                                                                                    "value" => "EOF"
                                                                                                ])),
                                                                                                (value!([
                                                                                                    "emit" => "call",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            (value!([
                                                                                                                "emit" => "identifier",
                                                                                                                "value" => "error"
                                                                                                            ])),
                                                                                                            (value!([
                                                                                                                "emit" => "callarg",
                                                                                                                "children" =>
                                                                                                                    (value!([
                                                                                                                        "emit" => "value_string",
                                                                                                                        "value" => "Unclosed match, expecting '''"
                                                                                                                    ]))
                                                                                                            ]))
                                                                                                        ]))
                                                                                                ]))
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "call",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "str_join"
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "callarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "value_string",
                                                                                        "value" => ""
                                                                                    ]))
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "callarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "capture_index",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "value_integer",
                                                                                                "value" => 2
                                                                                            ]))
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "value_instance",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Expect"
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "instarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "value_token_touch",
                                                                                        "value" => "'"
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "T_Integer"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "call",
                                                        "children" =>
                                                            (value!([
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "ast"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "value_string",
                                                                            "value" => "value_integer"
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Int"
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "T_Float"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "call",
                                                        "children" =>
                                                            (value!([
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "ast"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "value_string",
                                                                            "value" => "value_float"
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Float"
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "CclChar"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "\\"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_EscapeSequence"
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "value_token_ccl",
                                                            "children" =>
                                                                (value!([
                                                                    "emit" => "ccl_neg",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "char",
                                                                            "value" => ">"
                                                                        ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "EOF"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "error"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "Unclosed character-class, expecting ']'"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "CclRange"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "CclChar"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "-"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "CclChar"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "range"
                                                                                        ]))
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "op_binary_add",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "capture_index",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                "emit" => "value_integer",
                                                                                                                "value" => 1
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "capture_index",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                "emit" => "value_integer",
                                                                                                                "value" => 3
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "CclChar"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "char"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Ccl"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "^"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_kle",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "CclRange"
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "ccl_neg"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "op_mod_kle",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "CclRange"
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "ccl"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Subscript"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "sequence",
                                                        "children" =>
                                                            (value!([
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "_"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "value_token_touch",
                                                                    "value" => "["
                                                                ])),
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "_"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "Expression"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "value_token_touch",
                                                                    "value" => "]"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "call",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "ast"
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "callarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "value_string",
                                                                                        "value" => "item"
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Attribute"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "sequence",
                                                        "children" =>
                                                            (value!([
                                                                (value!([
                                                                    "emit" => "value_token_touch",
                                                                    "value" => "."
                                                                ])),
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "_"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "T_Alias"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "call",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "ast"
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "callarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "value_string",
                                                                                        "value" => "attribute"
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Capture"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "$"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_Alias"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "capture_alias"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "$"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_Integer"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "capture_index"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "$"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "("
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "___"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expression"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => ")"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "capture_expr"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "$"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "error"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "'$...': Expecting identifier, integer or (expression)"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Variable"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_Identifier"
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "Capture"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Lvalue"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "sequence",
                                                        "children" =>
                                                            (value!([
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "Variable"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "op_mod_kle",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Subscript"
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "call",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "ast"
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "callarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "value_string",
                                                                                        "value" => "lvalue"
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Load"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Lvalue"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "++"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "inplace_post_inc"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Lvalue"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "--"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "inplace_post_dec"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "++"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Lvalue"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "inplace_pre_inc"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "--"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Lvalue"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "inplace_pre_dec"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "Variable"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Parselet"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "sequence",
                                                        "children" =>
                                                            (value!([
                                                                (value!([
                                                                    "emit" => "value_token_touch",
                                                                    "value" => "@"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "_"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "op_mod_opt",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "ParseletGenerics"
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "_"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "op_mod_opt",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "ParseletArguments"
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "value_instance",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Expect"
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "instarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "call",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                (value!([
                                                                                                    "emit" => "identifier",
                                                                                                    "value" => "Block"
                                                                                                ])),
                                                                                                (value!([
                                                                                                    "emit" => "callarg",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            "emit" => "value_string",
                                                                                                            "value" => "body"
                                                                                                        ]))
                                                                                                ]))
                                                                                            ]))
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "call",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "ast"
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "callarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "value_string",
                                                                                        "value" => "value_parselet"
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "ParseletGeneric"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "sequence",
                                                        "children" =>
                                                            (value!([
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "T_Identifier"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "_"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "op_mod_opt",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "sequence",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "value_token_touch",
                                                                                        "value" => ":"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "_"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "value_instance",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                (value!([
                                                                                                    "emit" => "identifier",
                                                                                                    "value" => "Expect"
                                                                                                ])),
                                                                                                (value!([
                                                                                                    "emit" => "instarg",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            "emit" => "identifier",
                                                                                                            "value" => "Atomic"
                                                                                                        ]))
                                                                                                ]))
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "call",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "ast"
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "callarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "value_string",
                                                                                        "value" => "gen"
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "ParseletGenerics"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "sequence",
                                                        "children" =>
                                                            (value!([
                                                                (value!([
                                                                    "emit" => "value_token_touch",
                                                                    "value" => "<"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "_"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "___"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "op_mod_kle",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "sequence",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "ParseletGeneric"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "___"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "op_mod_opt",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "sequence",
                                                                                                "children" =>
                                                                                                    (value!([
                                                                                                        (value!([
                                                                                                            "emit" => "value_token_touch",
                                                                                                            "value" => ","
                                                                                                        ])),
                                                                                                        (value!([
                                                                                                            "emit" => "identifier",
                                                                                                            "value" => "_"
                                                                                                        ])),
                                                                                                        (value!([
                                                                                                            "emit" => "identifier",
                                                                                                            "value" => "___"
                                                                                                        ]))
                                                                                                    ]))
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "___"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "value_instance",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Expect"
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "instarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "value_token_touch",
                                                                                        "value" => ">"
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "_"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "___"
                                                                ]))
                                                            ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "ParseletArgument"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "sequence",
                                                        "children" =>
                                                            (value!([
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "T_Identifier"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "_"
                                                                ])),
                                                                (value!([
                                                                    "emit" => "op_mod_opt",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "sequence",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "value_token_touch",
                                                                                        "value" => "="
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "_"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "value_instance",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                (value!([
                                                                                                    "emit" => "identifier",
                                                                                                    "value" => "Expect"
                                                                                                ])),
                                                                                                (value!([
                                                                                                    "emit" => "instarg",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            "emit" => "identifier",
                                                                                                            "value" => "Expression"
                                                                                                        ]))
                                                                                                ]))
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "call",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "ast"
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "callarg",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "value_string",
                                                                                        "value" => "sig"
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "ParseletArguments"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "op_mod_pos",
                                                        "children" =>
                                                            (value!([
                                                                "emit" => "sequence",
                                                                "children" =>
                                                                    (value!([
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "ParseletArgument"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "op_mod_opt",
                                                                            "children" =>
                                                                                (value!([
                                                                                    "emit" => "sequence",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            (value!([
                                                                                                "emit" => "value_token_touch",
                                                                                                "value" => ","
                                                                                            ])),
                                                                                            (value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "_"
                                                                                            ]))
                                                                                        ]))
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "StaticParseletInstance"
                                    ])),
                                    (value!([
                                        "emit" => "block",
                                        "children" =>
                                            (value!([
                                                (value!([
                                                    "emit" => "identifier",
                                                    "value" => "T_Consumable"
                                                ])),
                                                (value!([
                                                    "emit" => "identifier",
                                                    "value" => "Parselet"
                                                ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "ParseletInstanceArgument"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_Identifier"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => ":"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Atomic"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "instarg_named"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Atomic"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "instarg"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "ParseletInstance"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "StaticParseletInstance"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "<"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_pos",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "sequence",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "ParseletInstanceArgument"
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "op_mod_opt",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    "emit" => "sequence",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            (value!([
                                                                                                                "emit" => "value_token_touch",
                                                                                                                "value" => ","
                                                                                                            ])),
                                                                                                            (value!([
                                                                                                                "emit" => "identifier",
                                                                                                                "value" => "_"
                                                                                                            ]))
                                                                                                        ]))
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => ">"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "value_instance"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "StaticParseletInstance"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "InlineAssignment"
                                    ])),
                                    (value!([
                                        "emit" => "call",
                                        "children" =>
                                            (value!([
                                                (value!([
                                                    "emit" => "value_instance",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "identifier",
                                                                "value" => "Assignment"
                                                            ])),
                                                            (value!([
                                                                "emit" => "instarg",
                                                                "children" =>
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expression"
                                                                    ]))
                                                            ]))
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "callarg",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "value_string",
                                                            "value" => "copy"
                                                        ]))
                                                ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "InlineSequenceItem"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_Alias"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "=>"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "InlineAssignment"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "alias"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "LogicalOr"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "=>"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "InlineAssignment"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "alias"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "InlineAssignment"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "InlineSequence"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "sequence",
                                                        "children" =>
                                                            (value!([
                                                                (value!([
                                                                    "emit" => "op_mod_pos",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "sequence",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "InlineSequenceItem"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "___"
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "op_if",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "op_logical_or",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        (value!([
                                                                                            "emit" => "op_logical_and",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "comparison",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "call",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            (value!([
                                                                                                                                "emit" => "identifier",
                                                                                                                                "value" => "type"
                                                                                                                            ])),
                                                                                                                            (value!([
                                                                                                                                "emit" => "callarg",
                                                                                                                                "children" =>
                                                                                                                                    (value!([
                                                                                                                                        "emit" => "capture_index",
                                                                                                                                        "children" =>
                                                                                                                                            (value!([
                                                                                                                                                "emit" => "value_integer",
                                                                                                                                                "value" => 1
                                                                                                                                            ]))
                                                                                                                                    ]))
                                                                                                                            ]))
                                                                                                                        ]))
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "cmp_eq",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "value_string",
                                                                                                                            "value" => "list"
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "comparison",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "rvalue",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            (value!([
                                                                                                                                "emit" => "capture_index",
                                                                                                                                "children" =>
                                                                                                                                    (value!([
                                                                                                                                        "emit" => "value_integer",
                                                                                                                                        "value" => 1
                                                                                                                                    ]))
                                                                                                                            ])),
                                                                                                                            (value!([
                                                                                                                                "emit" => "attribute",
                                                                                                                                "children" =>
                                                                                                                                    (value!([
                                                                                                                                        "emit" => "value_string",
                                                                                                                                        "value" => "len"
                                                                                                                                    ]))
                                                                                                                            ]))
                                                                                                                        ]))
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "cmp_gt",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "value_integer",
                                                                                                                            "value" => 1
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "comparison",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "rvalue",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "capture_index",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "value_integer",
                                                                                                                            "value" => 1
                                                                                                                        ]))
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "item",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "value_string",
                                                                                                                            "value" => "emit"
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "cmp_eq",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                "emit" => "value_string",
                                                                                                                "value" => "alias"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "call",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "ast"
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "callarg",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    "emit" => "value_string",
                                                                                                    "value" => "sequence"
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "InlineSequences"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "InlineSequence"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_pos",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "sequence",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "___"
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "|"
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "_"
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "___"
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "value_instance",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "Expect"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "instarg",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                "emit" => "identifier",
                                                                                                                "value" => "InlineSequence"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "block"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "InlineSequence"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "InlineList"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "InlineAssignment"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "___"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_pos",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "sequence",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => ","
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "_"
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "InlineAssignment"
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "___"
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_opt",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "sequence",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => ","
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "_"
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "___"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "list"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "op_mod_opt",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "InlineAssignment"
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "___"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "sequence",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "value_token_touch",
                                                                                    "value" => ","
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "_"
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "___"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "list"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "CallArgument"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_Identifier"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "="
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Not"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_ccl",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    "emit" => "ccl",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            (value!([
                                                                                                                "emit" => "char",
                                                                                                                "value" => ">"
                                                                                                            ])),
                                                                                                            (value!([
                                                                                                                "emit" => "char",
                                                                                                                "value" => "="
                                                                                                            ]))
                                                                                                        ]))
                                                                                                ]))
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "InlineSequences"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "callarg_named"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "InlineSequences"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "callarg"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "CallArguments"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "sequence",
                                                        "children" =>
                                                            (value!([
                                                                (value!([
                                                                    "emit" => "op_binary_add",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "CallArgument"
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "value_instance",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Repeat"
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "instarg",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    "emit" => "sequence",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            (value!([
                                                                                                                "emit" => "sequence",
                                                                                                                "children" =>
                                                                                                                    (value!([
                                                                                                                        (value!([
                                                                                                                            "emit" => "value_token_touch",
                                                                                                                            "value" => ","
                                                                                                                        ])),
                                                                                                                        (value!([
                                                                                                                            "emit" => "identifier",
                                                                                                                            "value" => "_"
                                                                                                                        ]))
                                                                                                                    ]))
                                                                                                            ])),
                                                                                                            (value!([
                                                                                                                "emit" => "identifier",
                                                                                                                "value" => "___"
                                                                                                            ])),
                                                                                                            (value!([
                                                                                                                "emit" => "identifier",
                                                                                                                "value" => "CallArgument"
                                                                                                            ]))
                                                                                                        ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "instarg_named",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "min"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "value_integer",
                                                                                                        "value" => 0
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "instarg_named",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "blur"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "value_false"
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "op_mod_opt",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "sequence",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "value_token_touch",
                                                                                        "value" => ","
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "_"
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "___"
                                                                ]))
                                                            ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "TokenLiteral"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "'"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_Touch"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "'"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "value_token_match"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_Touch"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "value_token_touch"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "Chars"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "<"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Ccl"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => ">"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "value_token_ccls"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "Chars"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "value_token_anys"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "Char"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "<"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Ccl"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => ">"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "value_token_ccl"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "Char"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "value_token_any"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "Void"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "value_token_void"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Token"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "("
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "___"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => ")"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "dict"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "("
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "___"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "block",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "InlineList"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "InlineSequences"
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "___"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => ")"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "@"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "("
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "___"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "block",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "InlineList"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "InlineSequences"
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "___"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => ")"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "area"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "Block"
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "TokenLiteral"
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ParseletInstance"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "("
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "___"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_opt",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "CallArguments"
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "___"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => ")"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "call"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "ParseletInstance"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "TokenModifier"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Token"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "+"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_mod_pos"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Token"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "*"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_mod_kle"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Token"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "?"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_mod_opt"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "Token"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Literal"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "true"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "value_true"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "false"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "value_false"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "void"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "value_void"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "null"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "value_null"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_String"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "value_string"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_Float"
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_Integer"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Atomic"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "Literal"
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "TokenModifier"
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "if"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Expression"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "___"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Statement"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_opt",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "sequence",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "___"
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "value_instance",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "Keyword"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "instarg",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                "emit" => "value_token_touch",
                                                                                                                "value" => "else"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "_"
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "___"
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "value_instance",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "Expect"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "instarg",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                "emit" => "identifier",
                                                                                                                "value" => "Statement"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_if"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "for"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Lvalue"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_instance",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "Expect"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "instarg",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                "emit" => "value_token_touch",
                                                                                                                "value" => "in"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Expression"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "___"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Statement"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_for"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "loop"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expression"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "___"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Block"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_loop"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "loop"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Block"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_loop"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "Load"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Rvalue"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Rvalue"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "("
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "___"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_opt",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "CallArguments"
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => ")"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "call"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Rvalue"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_kle",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "block",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Attribute"
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Subscript"
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "rvalue"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "Atomic"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Unary"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "-"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Not"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "-"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Unary"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_unary_neg"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "!"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Unary"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_unary_not"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "*"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Unary"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_deref"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Rvalue"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "MulDiv"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "MulDiv"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "*"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Not"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_ccl",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    "emit" => "ccl",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            "emit" => "char",
                                                                                                            "value" => "="
                                                                                                        ]))
                                                                                                ]))
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Unary"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_binary_mul"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "MulDiv"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "//"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Not"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_ccl",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    "emit" => "ccl",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            "emit" => "char",
                                                                                                            "value" => "="
                                                                                                        ]))
                                                                                                ]))
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Unary"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_binary_divi"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "MulDiv"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "/"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Not"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_ccl",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    "emit" => "ccl",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            "emit" => "char",
                                                                                                            "value" => "="
                                                                                                        ]))
                                                                                                ]))
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Unary"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_binary_div"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "MulDiv"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "%"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Not"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_ccl",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    "emit" => "ccl",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            "emit" => "char",
                                                                                                            "value" => "="
                                                                                                        ]))
                                                                                                ]))
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Unary"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_binary_mod"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "Unary"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "AddSub"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "AddSub"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "+"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Not"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_ccl",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    "emit" => "ccl",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            (value!([
                                                                                                                "emit" => "char",
                                                                                                                "value" => "+"
                                                                                                            ])),
                                                                                                            (value!([
                                                                                                                "emit" => "char",
                                                                                                                "value" => "="
                                                                                                            ]))
                                                                                                        ]))
                                                                                                ]))
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "MulDiv"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_binary_add"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "AddSub"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "-"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Not"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_ccl",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    "emit" => "ccl",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            (value!([
                                                                                                                "emit" => "char",
                                                                                                                "value" => "-"
                                                                                                            ])),
                                                                                                            (value!([
                                                                                                                "emit" => "char",
                                                                                                                "value" => "="
                                                                                                            ]))
                                                                                                        ]))
                                                                                                ]))
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "MulDiv"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_binary_sub"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "MulDiv"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Comparison"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "AddSub"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_pos",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "block",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        (value!([
                                                                                            "emit" => "sequence",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "value_token_touch",
                                                                                                        "value" => "=="
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "_"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "value_instance",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "identifier",
                                                                                                                    "value" => "Expect"
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "instarg",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "identifier",
                                                                                                                            "value" => "AddSub"
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "call",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "identifier",
                                                                                                                    "value" => "ast"
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "callarg",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "value_string",
                                                                                                                            "value" => "cmp_eq"
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "sequence",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "value_token_touch",
                                                                                                        "value" => "!="
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "_"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "value_instance",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "identifier",
                                                                                                                    "value" => "Expect"
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "instarg",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "identifier",
                                                                                                                            "value" => "AddSub"
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "call",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "identifier",
                                                                                                                    "value" => "ast"
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "callarg",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "value_string",
                                                                                                                            "value" => "cmp_neq"
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "sequence",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "value_token_touch",
                                                                                                        "value" => "<="
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "_"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "value_instance",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "identifier",
                                                                                                                    "value" => "Expect"
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "instarg",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "identifier",
                                                                                                                            "value" => "AddSub"
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "call",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "identifier",
                                                                                                                    "value" => "ast"
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "callarg",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "value_string",
                                                                                                                            "value" => "cmp_lteq"
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "sequence",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "value_token_touch",
                                                                                                        "value" => ">="
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "_"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "value_instance",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "identifier",
                                                                                                                    "value" => "Expect"
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "instarg",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "identifier",
                                                                                                                            "value" => "AddSub"
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "call",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "identifier",
                                                                                                                    "value" => "ast"
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "callarg",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "value_string",
                                                                                                                            "value" => "cmp_gteq"
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "sequence",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "value_token_touch",
                                                                                                        "value" => "<"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "_"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "value_instance",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "identifier",
                                                                                                                    "value" => "Expect"
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "instarg",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "identifier",
                                                                                                                            "value" => "AddSub"
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "call",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "identifier",
                                                                                                                    "value" => "ast"
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "callarg",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "value_string",
                                                                                                                            "value" => "cmp_lt"
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "sequence",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "value_token_touch",
                                                                                                        "value" => ">"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "_"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "value_instance",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "identifier",
                                                                                                                    "value" => "Expect"
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "instarg",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "identifier",
                                                                                                                            "value" => "AddSub"
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "call",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "identifier",
                                                                                                                    "value" => "ast"
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "callarg",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "value_string",
                                                                                                                            "value" => "cmp_gt"
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "comparison"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "AddSub"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "LogicalAnd"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "LogicalAnd"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "&&"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Comparison"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_logical_and"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "Comparison"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "LogicalOr"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "LogicalOr"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "||"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "LogicalAnd"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_logical_or"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "LogicalAnd"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Expression"
                                    ])),
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "LogicalOr"
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "ExpressionList"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expression"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_pos",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "sequence",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => ","
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "_"
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Expression"
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_opt",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "sequence",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => ","
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "_"
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "list"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "op_mod_opt",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Expression"
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "sequence",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "value_token_touch",
                                                                                    "value" => ","
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "_"
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "list"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "Expression"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Assignment"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                (value!([
                                                    "emit" => "gen",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "Source"
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "sig",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "identifier",
                                                                "value" => "mode"
                                                            ])),
                                                            (value!([
                                                                "emit" => "value_string",
                                                                "value" => "hold"
                                                            ]))
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "body",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "sequence",
                                                                "children" =>
                                                                    (value!([
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Lvalue"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_token_touch",
                                                                            "value" => "+="
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_instance",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "Expect"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "instarg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "Self"
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "call",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "ast"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "callarg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "op_binary_add",
                                                                                                "children" =>
                                                                                                    (value!([
                                                                                                        (value!([
                                                                                                            "emit" => "value_string",
                                                                                                            "value" => "assign_add_"
                                                                                                        ])),
                                                                                                        (value!([
                                                                                                            "emit" => "identifier",
                                                                                                            "value" => "mode"
                                                                                                        ]))
                                                                                                    ]))
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "sequence",
                                                                "children" =>
                                                                    (value!([
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Lvalue"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_token_touch",
                                                                            "value" => "-="
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_instance",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "Expect"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "instarg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "Self"
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "call",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "ast"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "callarg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "op_binary_add",
                                                                                                "children" =>
                                                                                                    (value!([
                                                                                                        (value!([
                                                                                                            "emit" => "value_string",
                                                                                                            "value" => "assign_sub_"
                                                                                                        ])),
                                                                                                        (value!([
                                                                                                            "emit" => "identifier",
                                                                                                            "value" => "mode"
                                                                                                        ]))
                                                                                                    ]))
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "sequence",
                                                                "children" =>
                                                                    (value!([
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Lvalue"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_token_touch",
                                                                            "value" => "*="
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_instance",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "Expect"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "instarg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "Self"
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "call",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "ast"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "callarg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "op_binary_add",
                                                                                                "children" =>
                                                                                                    (value!([
                                                                                                        (value!([
                                                                                                            "emit" => "value_string",
                                                                                                            "value" => "assign_mul_"
                                                                                                        ])),
                                                                                                        (value!([
                                                                                                            "emit" => "identifier",
                                                                                                            "value" => "mode"
                                                                                                        ]))
                                                                                                    ]))
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "sequence",
                                                                "children" =>
                                                                    (value!([
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Lvalue"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_token_touch",
                                                                            "value" => "/="
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_instance",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "Expect"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "instarg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "Self"
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "call",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "ast"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "callarg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "op_binary_add",
                                                                                                "children" =>
                                                                                                    (value!([
                                                                                                        (value!([
                                                                                                            "emit" => "value_string",
                                                                                                            "value" => "assign_div_"
                                                                                                        ])),
                                                                                                        (value!([
                                                                                                            "emit" => "identifier",
                                                                                                            "value" => "mode"
                                                                                                        ]))
                                                                                                    ]))
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "sequence",
                                                                "children" =>
                                                                    (value!([
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Lvalue"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_token_touch",
                                                                            "value" => "//="
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_instance",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "Expect"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "instarg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "Self"
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "call",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "ast"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "callarg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "op_binary_add",
                                                                                                "children" =>
                                                                                                    (value!([
                                                                                                        (value!([
                                                                                                            "emit" => "value_string",
                                                                                                            "value" => "assign_divi_"
                                                                                                        ])),
                                                                                                        (value!([
                                                                                                            "emit" => "identifier",
                                                                                                            "value" => "mode"
                                                                                                        ]))
                                                                                                    ]))
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "sequence",
                                                                "children" =>
                                                                    (value!([
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Lvalue"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_token_touch",
                                                                            "value" => "%="
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_instance",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "Expect"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "instarg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "Self"
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "call",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "ast"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "callarg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "op_binary_add",
                                                                                                "children" =>
                                                                                                    (value!([
                                                                                                        (value!([
                                                                                                            "emit" => "value_string",
                                                                                                            "value" => "assign_mod_"
                                                                                                        ])),
                                                                                                        (value!([
                                                                                                            "emit" => "identifier",
                                                                                                            "value" => "mode"
                                                                                                        ]))
                                                                                                    ]))
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "sequence",
                                                                "children" =>
                                                                    (value!([
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Lvalue"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_token_touch",
                                                                            "value" => "="
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_instance",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "Not"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "instarg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "value_token_ccl",
                                                                                                "children" =>
                                                                                                    (value!([
                                                                                                        "emit" => "ccl",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "char",
                                                                                                                    "value" => ">"
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "char",
                                                                                                                    "value" => "="
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ]))
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_instance",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "Expect"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "instarg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "Self"
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "call",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "ast"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "callarg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "op_binary_add",
                                                                                                "children" =>
                                                                                                    (value!([
                                                                                                        (value!([
                                                                                                            "emit" => "value_string",
                                                                                                            "value" => "assign_"
                                                                                                        ])),
                                                                                                        (value!([
                                                                                                            "emit" => "identifier",
                                                                                                            "value" => "mode"
                                                                                                        ]))
                                                                                                    ]))
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "identifier",
                                                                "value" => "Source"
                                                            ]))
                                                        ]))
                                                ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Statement"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "accept"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_opt",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Expression"
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_accept"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "break"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_opt",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Expression"
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_break"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "continue"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_opt",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Expression"
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_continue"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "exit"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_opt",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Expression"
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_exit"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "next"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_next"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "push"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_opt",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Expression"
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_push"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "reject"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_reject"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "repeat"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_repeat"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "reset"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_reset"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "return"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_opt",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Expression"
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "op_accept"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Assignment"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "ExpressionList"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "drop"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Block"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                (value!([
                                                    "emit" => "sig",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "identifier",
                                                                "value" => "emit"
                                                            ])),
                                                            (value!([
                                                                "emit" => "value_string",
                                                                "value" => "block"
                                                            ]))
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "body",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "sequence",
                                                                "children" =>
                                                                    (value!([
                                                                        (value!([
                                                                            "emit" => "value_token_touch",
                                                                            "value" => "{"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "___"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_token_touch",
                                                                            "value" => "}"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "call",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "ast"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "callarg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "value_string",
                                                                                                "value" => "value_void"
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "sequence",
                                                                "children" =>
                                                                    (value!([
                                                                        (value!([
                                                                            "emit" => "value_token_touch",
                                                                            "value" => "{"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "op_mod_kle",
                                                                            "children" =>
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Tokay"
                                                                                ]))
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_instance",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "Expect"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "instarg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "value_token_touch",
                                                                                                "value" => "}"
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "call",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "ast"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "callarg",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "emit"
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ]))
                                                        ]))
                                                ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "SequenceItem"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_Alias"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "=>"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "ExpressionList"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "alias"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expression"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => "=>"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "ExpressionList"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "alias"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "Statement"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Sequence"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        "emit" => "sequence",
                                                        "children" =>
                                                            (value!([
                                                                (value!([
                                                                    "emit" => "op_mod_pos",
                                                                    "children" =>
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "SequenceItem"
                                                                        ]))
                                                                ])),
                                                                (value!([
                                                                    "emit" => "op_if",
                                                                    "children" =>
                                                                        (value!([
                                                                            (value!([
                                                                                "emit" => "op_logical_or",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        (value!([
                                                                                            "emit" => "op_logical_and",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "comparison",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "call",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            (value!([
                                                                                                                                "emit" => "identifier",
                                                                                                                                "value" => "type"
                                                                                                                            ])),
                                                                                                                            (value!([
                                                                                                                                "emit" => "callarg",
                                                                                                                                "children" =>
                                                                                                                                    (value!([
                                                                                                                                        "emit" => "capture_index",
                                                                                                                                        "children" =>
                                                                                                                                            (value!([
                                                                                                                                                "emit" => "value_integer",
                                                                                                                                                "value" => 1
                                                                                                                                            ]))
                                                                                                                                    ]))
                                                                                                                            ]))
                                                                                                                        ]))
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "cmp_eq",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "value_string",
                                                                                                                            "value" => "list"
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "comparison",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "rvalue",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            (value!([
                                                                                                                                "emit" => "capture_index",
                                                                                                                                "children" =>
                                                                                                                                    (value!([
                                                                                                                                        "emit" => "value_integer",
                                                                                                                                        "value" => 1
                                                                                                                                    ]))
                                                                                                                            ])),
                                                                                                                            (value!([
                                                                                                                                "emit" => "attribute",
                                                                                                                                "children" =>
                                                                                                                                    (value!([
                                                                                                                                        "emit" => "value_string",
                                                                                                                                        "value" => "len"
                                                                                                                                    ]))
                                                                                                                            ]))
                                                                                                                        ]))
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "cmp_gt",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "value_integer",
                                                                                                                            "value" => 1
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "comparison",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "rvalue",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                (value!([
                                                                                                                    "emit" => "capture_index",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "value_integer",
                                                                                                                            "value" => 1
                                                                                                                        ]))
                                                                                                                ])),
                                                                                                                (value!([
                                                                                                                    "emit" => "item",
                                                                                                                    "children" =>
                                                                                                                        (value!([
                                                                                                                            "emit" => "value_string",
                                                                                                                            "value" => "emit"
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "cmp_eq",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                "emit" => "value_string",
                                                                                                                "value" => "alias"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ])),
                                                                            (value!([
                                                                                "emit" => "call",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "ast"
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "callarg",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    "emit" => "value_string",
                                                                                                    "value" => "sequence"
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Sequences"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Sequence"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_pos",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "sequence",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "|"
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "_"
                                                                                        ])),
                                                                                        (value!([
                                                                                            "emit" => "value_instance",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "Expect"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "instarg",
                                                                                                        "children" =>
                                                                                                            (value!([
                                                                                                                "emit" => "identifier",
                                                                                                                "value" => "Sequence"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "block"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "Sequence"
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "constant",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "Tokay"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                "emit" => "body",
                                                "children" =>
                                                    (value!([
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_EOL"
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "begin"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Sequences"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "T_EOL"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "begin"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Keyword"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "end"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Sequences"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "T_EOL"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "end"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "T_Identifier"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => ":"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "block",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "sequence",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            (value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "Literal"
                                                                                            ])),
                                                                                            (value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "_"
                                                                                            ])),
                                                                                            (value!([
                                                                                                "emit" => "value_instance",
                                                                                                "children" =>
                                                                                                    (value!([
                                                                                                        (value!([
                                                                                                            "emit" => "identifier",
                                                                                                            "value" => "Peek"
                                                                                                        ])),
                                                                                                        (value!([
                                                                                                            "emit" => "instarg",
                                                                                                            "children" =>
                                                                                                                (value!([
                                                                                                                    "emit" => "identifier",
                                                                                                                    "value" => "T_EOL"
                                                                                                                ]))
                                                                                                        ]))
                                                                                                    ]))
                                                                                            ]))
                                                                                        ]))
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "sequence",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            (value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "Token"
                                                                                            ])),
                                                                                            (value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "_"
                                                                                            ])),
                                                                                            (value!([
                                                                                                "emit" => "value_instance",
                                                                                                "children" =>
                                                                                                    (value!([
                                                                                                        (value!([
                                                                                                            "emit" => "identifier",
                                                                                                            "value" => "Peek"
                                                                                                        ])),
                                                                                                        (value!([
                                                                                                            "emit" => "instarg",
                                                                                                            "children" =>
                                                                                                                (value!([
                                                                                                                    "emit" => "identifier",
                                                                                                                    "value" => "T_EOL"
                                                                                                                ]))
                                                                                                        ]))
                                                                                                    ]))
                                                                                            ]))
                                                                                        ]))
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Sequences"
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_instance",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Expect"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "instarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "T_EOL"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "ast"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "value_string",
                                                                                            "value" => "constant"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Sequences"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_mod_opt",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "T_EOL"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (value!([
                            "emit" => "sequence",
                            "children" =>
                                (value!([
                                    (value!([
                                        "emit" => "identifier",
                                        "value" => "_"
                                    ])),
                                    (value!([
                                        "emit" => "op_mod_kle",
                                        "children" =>
                                            (value!([
                                                "emit" => "identifier",
                                                "value" => "Tokay"
                                            ]))
                                    ])),
                                    (value!([
                                        "emit" => "value_instance",
                                        "children" =>
                                            (value!([
                                                (value!([
                                                    "emit" => "identifier",
                                                    "value" => "Expect"
                                                ])),
                                                (value!([
                                                    "emit" => "instarg",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "EOF"
                                                        ]))
                                                ]))
                                            ]))
                                    ])),
                                    (value!([
                                        "emit" => "call",
                                        "children" =>
                                            (value!([
                                                (value!([
                                                    "emit" => "identifier",
                                                    "value" => "ast"
                                                ])),
                                                (value!([
                                                    "emit" => "callarg",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "value_string",
                                                            "value" => "main"
                                                        ]))
                                                ]))
                                            ]))
                                    ]))
                                ]))
                        ]))
                    ]))
            ])
            /*ETARENEG*/
        ;

        let mut compiler = Compiler::new();
        compiler.debug = 0; // unset debug always

        Self(
            compiler
                .compile_from_ast(&ast, Some("parser".to_string()))
                .expect("Tokay grammar cannot be compiled!")
                .expect("Tokay grammar contains no main?"),
        )
    }

    pub fn parse(&self, mut reader: Reader) -> Result<RefValue, Error> {
        //self.0.dump();
        let mut thread = Thread::new(&self.0, vec![&mut reader]);

        if let Ok(level) = std::env::var("TOKAY_PARSER_DEBUG") {
            thread.debug = level.parse::<u8>().unwrap_or_default();
        } else {
            thread.debug = 0;
        }

        match thread.run() {
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

//fixme: Remove this into tests...
/*
#[test]
fn parser_indirectleftrec() {
    /*
        X: Y 'c'
        Y: Z 'b'
        Z: X | Y | 'a'
        Z
    */

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
*/

#[test]
// EOL
fn parser_eol() {
    for eol in ["\n", "\r", "\r\n", ";"] {
        let tok = format!("a = 1{}a + 2", eol);
        println!("EOL test {:?}", tok);
        assert_eq!(crate::run(&tok, ""), Ok(Some(crate::value!(3))));
    }
}
