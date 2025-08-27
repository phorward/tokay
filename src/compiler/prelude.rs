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
            /*GENERATE cargo run -- "`sed 's/ast("main")/ast2rust(ast("main"), level=3)/g' src/compiler/tokay.tok`" -- src/prelude.tok */
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
                                                                            "emit" => "op_reject"
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "identifier",
                                                                "value" => "Empty"
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
                                                                        "emit" => "op_reset"
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
                                                    "emit" => "sig",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "identifier",
                                                                "value" => "msg"
                                                            ])),
                                                            (value!([
                                                                "emit" => "value_void"
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
                                                                                                                                                    "emit" => "block",
                                                                                                                                                    "children" =>
                                                                                                                                                        (value!([
                                                                                                                                                            (value!([
                                                                                                                                                                "emit" => "identifier",
                                                                                                                                                                "value" => "Token"
                                                                                                                                                            ])),
                                                                                                                                                            (value!([
                                                                                                                                                                "emit" => "value_token_any"
                                                                                                                                                            ])),
                                                                                                                                                            (value!([
                                                                                                                                                                "emit" => "value_string",
                                                                                                                                                                "value" => "end-of-file"
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
                                    ]))
                                ]))
                        ])),
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
                                                    "emit" => "gen",
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
                                                    "emit" => "gen",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "identifier",
                                                                "value" => "max"
                                                            ])),
                                                            (value!([
                                                                "emit" => "value_void"
                                                            ]))
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "gen",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "identifier",
                                                                "value" => "blur"
                                                            ])),
                                                            (value!([
                                                                "emit" => "value_true"
                                                            ]))
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "body",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "assign_drop",
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
                                                                            "emit" => "list"
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "assign_drop",
                                                                "children" =>
                                                                    (value!([
                                                                        (value!([
                                                                            "emit" => "lvalue",
                                                                            "children" =>
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "cnt"
                                                                                ]))
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_integer",
                                                                            "value" => 0
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "op_loop",
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
                                                                                                "emit" => "identifier",
                                                                                                "value" => "P"
                                                                                            ])),
                                                                                            (value!([
                                                                                                "emit" => "block",
                                                                                                "children" =>
                                                                                                    (value!([
                                                                                                        (value!([
                                                                                                            "emit" => "call",
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
                                                                                                                                            "value" => "push"
                                                                                                                                        ]))
                                                                                                                                ]))
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
                                                                                                                                        "value" => 1
                                                                                                                                    ]))
                                                                                                                            ]))
                                                                                                                    ]))
                                                                                                                ]))
                                                                                                        ])),
                                                                                                        (value!([
                                                                                                            "emit" => "assign_add_drop",
                                                                                                            "children" =>
                                                                                                                (value!([
                                                                                                                    (value!([
                                                                                                                        "emit" => "lvalue",
                                                                                                                        "children" =>
                                                                                                                            (value!([
                                                                                                                                "emit" => "identifier",
                                                                                                                                "value" => "cnt"
                                                                                                                            ]))
                                                                                                                    ])),
                                                                                                                    (value!([
                                                                                                                        "emit" => "value_integer",
                                                                                                                        "value" => 1
                                                                                                                    ]))
                                                                                                                ]))
                                                                                                        ])),
                                                                                                        (value!([
                                                                                                            "emit" => "op_if",
                                                                                                            "children" =>
                                                                                                                (value!([
                                                                                                                    (value!([
                                                                                                                        "emit" => "op_logical_and",
                                                                                                                        "children" =>
                                                                                                                            (value!([
                                                                                                                                (value!([
                                                                                                                                    "emit" => "identifier",
                                                                                                                                    "value" => "max"
                                                                                                                                ])),
                                                                                                                                (value!([
                                                                                                                                    "emit" => "comparison",
                                                                                                                                    "children" =>
                                                                                                                                        (value!([
                                                                                                                                            (value!([
                                                                                                                                                "emit" => "identifier",
                                                                                                                                                "value" => "cnt"
                                                                                                                                            ])),
                                                                                                                                            (value!([
                                                                                                                                                "emit" => "cmp_eq",
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
                                                                                                                        "emit" => "op_break"
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
                                                                                                "emit" => "comparison",
                                                                                                "children" =>
                                                                                                    (value!([
                                                                                                        (value!([
                                                                                                            "emit" => "identifier",
                                                                                                            "value" => "cnt"
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
                                                                                            ])),
                                                                                            (value!([
                                                                                                "emit" => "op_reject"
                                                                                            ]))
                                                                                        ]))
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "op_break"
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "op_if",
                                                                "children" =>
                                                                    (value!([
                                                                        (value!([
                                                                            "emit" => "comparison",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "cnt"
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
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "op_reject"
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "op_if",
                                                                "children" =>
                                                                    (value!([
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "blur"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "block",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "op_if",
                                                                                        "children" =>
                                                                                            (value!([
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
                                                                                                                "emit" => "cmp_eq",
                                                                                                                "children" =>
                                                                                                                    (value!([
                                                                                                                        "emit" => "value_integer",
                                                                                                                        "value" => 0
                                                                                                                    ]))
                                                                                                            ]))
                                                                                                        ]))
                                                                                                ])),
                                                                                                (value!([
                                                                                                    "emit" => "op_accept",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            "emit" => "value_void"
                                                                                                        ]))
                                                                                                ]))
                                                                                            ]))
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "op_if",
                                                                                        "children" =>
                                                                                            (value!([
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
                                                                                                                "emit" => "cmp_eq",
                                                                                                                "children" =>
                                                                                                                    (value!([
                                                                                                                        "emit" => "value_integer",
                                                                                                                        "value" => 1
                                                                                                                    ]))
                                                                                                            ]))
                                                                                                        ]))
                                                                                                ])),
                                                                                                (value!([
                                                                                                    "emit" => "op_accept",
                                                                                                    "children" =>
                                                                                                        (value!([
                                                                                                            "emit" => "rvalue",
                                                                                                            "children" =>
                                                                                                                (value!([
                                                                                                                    (value!([
                                                                                                                        "emit" => "identifier",
                                                                                                                        "value" => "res"
                                                                                                                    ])),
                                                                                                                    (value!([
                                                                                                                        "emit" => "item",
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
                                                    "emit" => "gen",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "identifier",
                                                                "value" => "blur"
                                                            ])),
                                                            (value!([
                                                                "emit" => "value_true"
                                                            ]))
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "body",
                                                    "children" =>
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
                                                                                "emit" => "identifier",
                                                                                "value" => "P"
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
                                                                                    "emit" => "identifier",
                                                                                    "value" => "blur"
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
                                                    "emit" => "gen",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "identifier",
                                                                "value" => "blur"
                                                            ])),
                                                            (value!([
                                                                "emit" => "value_true"
                                                            ]))
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "body",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "op_logical_or",
                                                            "children" =>
                                                                (value!([
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
                                                                                            "emit" => "identifier",
                                                                                            "value" => "P"
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
                                                                                                "emit" => "identifier",
                                                                                                "value" => "blur"
                                                                                            ]))
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "value_void"
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
                                                                        "emit" => "identifier",
                                                                        "value" => "P"
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Empty"
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
                                                                "emit" => "value_true"
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
                                                                            "emit" => "sequence",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "identifier",
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
                                        "value" => "Keyword"
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
                                                                                            "emit" => "block",
                                                                                            "children" =>
                                                                                                (value!([
                                                                                                    (value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "Alphanumeric"
                                                                                                    ])),
                                                                                                    (value!([
                                                                                                        "emit" => "value_token_touch",
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
                                                    "emit" => "identifier",
                                                    "value" => "Float"
                                                ])),
                                                (value!([
                                                    "emit" => "identifier",
                                                    "value" => "Int"
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
                                                    "emit" => "identifier",
                                                    "value" => "AsciiPunctuation"
                                                ])),
                                                (value!([
                                                    "emit" => "identifier",
                                                    "value" => "Word"
                                                ])),
                                                (value!([
                                                    "emit" => "identifier",
                                                    "value" => "Number"
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
                                        "value" => "max"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                (value!([
                                                    "emit" => "sig",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "value"
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "body",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "rvalue",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "iter"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "value"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "attribute",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "max"
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
                                        "value" => "min"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                (value!([
                                                    "emit" => "sig",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "value"
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "body",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "rvalue",
                                                            "children" =>
                                                                (value!([
                                                                    (value!([
                                                                        "emit" => "call",
                                                                        "children" =>
                                                                            (value!([
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "iter"
                                                                                ])),
                                                                                (value!([
                                                                                    "emit" => "callarg",
                                                                                    "children" =>
                                                                                        (value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "value"
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (value!([
                                                                        "emit" => "attribute",
                                                                        "children" =>
                                                                            (value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "min"
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
                                        "value" => "sum"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                (value!([
                                                    "emit" => "sig",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "value"
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "body",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "assign_drop",
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
                                                                            "emit" => "value_void"
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "op_for",
                                                                "children" =>
                                                                    (value!([
                                                                        (value!([
                                                                            "emit" => "lvalue",
                                                                            "children" =>
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "i"
                                                                                ]))
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "value"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "block",
                                                                            "children" =>
                                                                                (value!([
                                                                                    "emit" => "assign_add_drop",
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
                                                                                                "emit" => "identifier",
                                                                                                "value" => "i"
                                                                                            ]))
                                                                                        ]))
                                                                                ]))
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
                                        "value" => "avg"
                                    ])),
                                    (value!([
                                        "emit" => "value_parselet",
                                        "children" =>
                                            (value!([
                                                (value!([
                                                    "emit" => "sig",
                                                    "children" =>
                                                        (value!([
                                                            "emit" => "identifier",
                                                            "value" => "value"
                                                        ]))
                                                ])),
                                                (value!([
                                                    "emit" => "body",
                                                    "children" =>
                                                        (value!([
                                                            (value!([
                                                                "emit" => "assign_drop",
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
                                                                            "emit" => "value_void"
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "assign_drop",
                                                                "children" =>
                                                                    (value!([
                                                                        (value!([
                                                                            "emit" => "lvalue",
                                                                            "children" =>
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "cnt"
                                                                                ]))
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "value_integer",
                                                                            "value" => 0
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "op_for",
                                                                "children" =>
                                                                    (value!([
                                                                        (value!([
                                                                            "emit" => "lvalue",
                                                                            "children" =>
                                                                                (value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "i"
                                                                                ]))
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "value"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "block",
                                                                            "children" =>
                                                                                (value!([
                                                                                    (value!([
                                                                                        "emit" => "assign_add_drop",
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
                                                                                                    "emit" => "identifier",
                                                                                                    "value" => "i"
                                                                                                ]))
                                                                                            ]))
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "inplace_post_inc",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                "emit" => "lvalue",
                                                                                                "children" =>
                                                                                                    (value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "cnt"
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
                                                                                                "value" => "cnt"
                                                                                            ]))
                                                                                    ])),
                                                                                    (value!([
                                                                                        "emit" => "op_logical_and",
                                                                                        "children" =>
                                                                                            (value!([
                                                                                                (value!([
                                                                                                    "emit" => "op_logical_and",
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
                                                                                                                                                            "emit" => "identifier",
                                                                                                                                                            "value" => "res"
                                                                                                                                                        ]))
                                                                                                                                                ]))
                                                                                                                                            ]))
                                                                                                                                    ])),
                                                                                                                                    (value!([
                                                                                                                                        "emit" => "cmp_neq",
                                                                                                                                        "children" =>
                                                                                                                                            (value!([
                                                                                                                                                "emit" => "value_string",
                                                                                                                                                "value" => "int"
                                                                                                                                            ]))
                                                                                                                                    ]))
                                                                                                                                ]))
                                                                                                                        ])),
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
                                                                                                                                                            "emit" => "identifier",
                                                                                                                                                            "value" => "res"
                                                                                                                                                        ]))
                                                                                                                                                ]))
                                                                                                                                            ]))
                                                                                                                                    ])),
                                                                                                                                    (value!([
                                                                                                                                        "emit" => "cmp_neq",
                                                                                                                                        "children" =>
                                                                                                                                            (value!([
                                                                                                                                                "emit" => "value_string",
                                                                                                                                                "value" => "float"
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
                                                                                                                                                "emit" => "identifier",
                                                                                                                                                "value" => "res"
                                                                                                                                            ]))
                                                                                                                                    ]))
                                                                                                                                ]))
                                                                                                                        ])),
                                                                                                                        (value!([
                                                                                                                            "emit" => "cmp_neq",
                                                                                                                            "children" =>
                                                                                                                                (value!([
                                                                                                                                    "emit" => "value_string",
                                                                                                                                    "value" => "bool"
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
                                                                                                                                    "emit" => "identifier",
                                                                                                                                    "value" => "res"
                                                                                                                                ]))
                                                                                                                        ]))
                                                                                                                    ]))
                                                                                                            ])),
                                                                                                            (value!([
                                                                                                                "emit" => "cmp_neq",
                                                                                                                "children" =>
                                                                                                                    (value!([
                                                                                                                        "emit" => "value_string",
                                                                                                                        "value" => "null"
                                                                                                                    ]))
                                                                                                            ]))
                                                                                                        ]))
                                                                                                ]))
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "op_accept",
                                                                            "children" =>
                                                                                (value!([
                                                                                    "emit" => "value_void"
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (value!([
                                                                "emit" => "op_binary_div",
                                                                "children" =>
                                                                    (value!([
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "res"
                                                                        ])),
                                                                        (value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "cnt"
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
            ])
            /*ETARENEG*/
        ;

        self.compile_from_ast(&ast, Some("prelude".to_string()))
            .expect("prelude cannot be compiled!")
            .expect("prelude contains no main?");
    }
}
