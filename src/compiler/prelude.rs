/** Tokay default prelude

The prelude is a default stub of Tokay standard parselets, which implements
fundamental parts of the Tokay language itself.

It's defined in src/prelude.tok and pre-compiled as an AST within the code.
*/
use super::*;
use crate::value;

impl Compiler {
    pub(super) fn load_prelude(&mut self) {
        // fixme: Make this lazy_static, so its created only once!
        let ast =
            /*GENERATE cargo run -- "`sed 's/ast("main")/ast2rust(ast("main"), level=3)/g' compiler/tokay.tok`" -- prelude.tok */
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
                                        "value" => "Repeat"
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
                                                            "value" => "P"
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "arg",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "identifier",
                                                                "value" => "min"
                                                            ])),
                                                            (value!([
                                                                "emit" => "value_integer",
                                                                "value" => 1
                                                            ]))
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "arg",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "identifier",
                                                                "value" => "max"
                                                            ])),
                                                            (value!([
                                                                "emit" => "value_void",
                                                                "value" => "void"
                                                            ]))
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "body",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "begin",
                                                                "children" =>
                                                                    (value!([
                                                                        "emit" => "sequence",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "block",
                                                                                "children" =>
                                                                                    (value!([
                                                                                        "emit" => "assign",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                (value!([
                                                                                                    "emit" => "lvalue",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            "emit" => "identifier",
                                                                                                            "value" => "res"
                                                                                                        ]))
                                                                                                ])),
                                                                                                (value!([
                                                                                                    "emit" => "call",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            "emit" => "identifier",
                                                                                                            "value" => "list"
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
                                                                            "value" => "P"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "block",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "assign_add",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                (value!([
                                                                                                    "emit" => "lvalue",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            "emit" => "identifier",
                                                                                                            "value" => "res"
                                                                                                        ]))
                                                                                                ])),
                                                                                                (value!([
                                                                                                    "emit" => "capture_index",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            "emit" => "value_integer",
                                                                                                            "value" => 1
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
                                                                                                                "emit" => "op_unary_not",
                                                                                                                "children" =>
                                                                                                                    (value!([
                                                                                                                        "emit" => "identifier",
                                                                                                                        "value" => "max"
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
                                                                                                                                        "emit" => "identifier",
                                                                                                                                        "value" => "res"
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
                                                                                                                            "emit" => "cmp_lt",
                                                                                                                            "children" =>
                                                                                                                                (value!([
                                                                                                                                    "emit" => "identifier",
                                                                                                                                    "value" => "max"
                                                                                                                                ]))
                                                                                                                        ]))
                                                                                                                    ]))
                                                                                                            ]))
                                                                                                        ]))
                                                                                                ])),
                                                                                                (value!([
                                                                                                    "emit" => "op_repeat",
                                                                                                    "value" => "repeat"
                                                                                                ]))
                                                                                            ]))
                                                                                    ]))
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
                                                                                        "emit" => "op_unary_not",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "res"
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
                                                                                                                "emit" => "identifier",
                                                                                                                "value" => "res"
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
                                                                                                    "emit" => "cmp_lt",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            "emit" => "identifier",
                                                                                                            "value" => "min"
                                                                                                        ]))
                                                                                                ]))
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "op_reject",
                                                                            "value" => "reject"
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "identifier",
                                                                "value" => "res"
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
                                        "value" => "Pos"
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
                                                            "value" => "P"
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "body",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "value_generic",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Repeat"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "genarg",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "P"
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
                                        "value" => "Kle"
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
                                                            "value" => "P"
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "body",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "value_generic",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Repeat"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "genarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "P"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "callarg_named",
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
                                        "value" => "Opt"
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
                                                            "value" => "P"
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "body",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "block",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "sequence",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "P"
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "sequence",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Void"
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
                                        "value" => "List"
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
                                                            "value" => "P"
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "gen",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "identifier",
                                                                "value" => "Separator"
                                                            ])),
                                                            (value!([
                                                                "emit" => "inline_sequence",
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
                                                ])),
                                                (value!([
                                                    "emit" => "gen",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "identifier",
                                                                "value" => "empty"
                                                            ])),
                                                            (value!([
                                                                "emit" => "value_true",
                                                                "value" => "true"
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
                                                                            "emit" => "value_token_self",
                                                                            "value" => "Self"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Separator"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "P"
                                                                        ])),
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
                                                            ])),
                                                            (value!([
                                                                "emit" => "op_if",
                                                                "children" =>
                                                                    (value!([
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "empty"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "inline_sequence",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "value_token_self",
                                                                                        "value" => "Self"
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "Separator"
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
                                                                            "value" => "P"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "list",
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
                                        "value" => "Not"
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
                                                            "value" => "P"
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
                                                                            "value" => "P"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "op_reject",
                                                                            "value" => "reject"
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "identifier",
                                                                "value" => "Void"
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
                                        "value" => "Peek"
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
                                                            "value" => "P"
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "body",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "P"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "op_reset",
                                                                        "value" => "reset"
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
                                        "value" => "Expect"
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
                                                            "value" => "P"
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "arg",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "identifier",
                                                                "value" => "msg"
                                                            ])),
                                                            (value!([
                                                                "emit" => "value_void",
                                                                "value" => "void"
                                                            ]))
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "body",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "op_accept",
                                                                "children" =>
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "P"
                                                                    ]))
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
                                                                                    "emit" => "op_logical_or",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            (value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "msg"
                                                                                            ])),
                                                                                            (value!([
                                                                                                "emit" => "op_binary_add",
                                                                                                "children" =>
                                                                                                    (value!([
                                                                                                        (value!([
                                                                                                            "emit" => "op_binary_add",
                                                                                                            "children" =>
                                                                                                                (value!([
                                                                                                                    (value!([
                                                                                                                        "emit" => "op_binary_add",
                                                                                                                        "children" =>
                                                                                                                            (value!([
                                                                                                                                (value!([
                                                                                                                                    "emit" => "value_string",
                                                                                                                                    "value" => "Expecting "
                                                                                                                                ])),
                                                                                                                                (value!([
                                                                                                                                    "emit" => "op_deref",
                                                                                                                                    "children" =>
                                                                                                                                        (value!([
                                                                                                                                            "emit" => "identifier",
                                                                                                                                            "value" => "P"
                                                                                                                                        ]))
                                                                                                                                ]))
                                                                                                                            ]))
                                                                                                                    ])),
                                                                                                                    (value!([
                                                                                                                        "emit" => "value_string",
                                                                                                                        "value" => ", but got "
                                                                                                                    ]))
                                                                                                                ]))
                                                                                                        ])),
                                                                                                        (value!([
                                                                                                            "emit" => "call",
                                                                                                            "children" =>
                                                                                                                (value!([
                                                                                                                    (value!([
                                                                                                                        "emit" => "identifier",
                                                                                                                        "value" => "repr"
                                                                                                                    ])),
                                                                                                                    (value!([
                                                                                                                        "emit" => "callarg",
                                                                                                                        "children" =>
                                                                                                                            (value!([
                                                                                                                                "emit" => "block",
                                                                                                                                "children" =>
                                                                                                                                    (value!([
                                                                                                                                        (value!([
                                                                                                                                            "emit" => "inline_sequence",
                                                                                                                                            "children" =>
                                                                                                                                                (value!([
                                                                                                                                                    "emit" => "identifier",
                                                                                                                                                    "value" => "Token"
                                                                                                                                                ]))
                                                                                                                                        ])),
                                                                                                                                        (value!([
                                                                                                                                            "emit" => "inline_sequence",
                                                                                                                                            "children" =>
                                                                                                                                                (value!([
                                                                                                                                                    "emit" => "value_token_any",
                                                                                                                                                    "value" => "Char"
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
                                        "value" => "Number"
                                    ])),
                                    (value!([
                                        "emit" => "block",
                                        "children" =>
                                            (value!([
                                                (value!([
                                                    "emit" => "sequence",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "Float"
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "sequence",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "Int"
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
                                        "emit" => "block",
                                        "children" =>
                                            (value!([
                                                (value!([
                                                    "emit" => "sequence",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "Word"
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "sequence",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "Number"
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "sequence",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "AsciiPunctuation"
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

        self.compile_from_ast(&ast)
            .expect("prelude cannot be compiled!")
            .expect("prelude contains no main?");
    }
}
