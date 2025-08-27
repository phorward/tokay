// This is the "old" way to integrate the Tokay parser.
// It is replaced by the feature `use_cbor_parser` and the pre-compiled VM program in `_tokay.cbor`.

// Below code blob between the markers GENERATE and ETARENEG (which is "GENERATE" reversed!)
// is injected by Tokay itself, by running the program `tokay.tok` with Tokay, parsing itself.

// This generates an abstract syntax tree (AST) representation of `tokay.tok` in form of
// value!-macro calls, which is injected below between the two markers.

// If something goes wrong, it is important to keep a working copy of this file in Git
// to have a working version of the parser at hand before its automatical replacement.
// The best way is to test grammar changes with `tokay.tok` intensely before rebuilding the
// parser, to ensure all runs well.

// To update this file, run `make parser-ast` in a shell.

/*GENERATE cargo run -- "`sed 's/ast("main")/ast2rust(ast("main"))/g' src/compiler/tokay.tok`" -- src/compiler/tokay.tok */
crate::value!([
    "emit" => "main",
    "children" =>
        (crate::value!([
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "_"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "op_mod_pos",
                                                "children" =>
                                                    (crate::value!([
                                                        "emit" => "value_token_ccl",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "ccl",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "char",
                                                                            "value" => "\t"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "char",
                                                                            "value" => " "
                                                                        ]))
                                                                    ]))
                                                            ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "#"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_kle",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "value_token_ccl",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            "emit" => "ccl_neg",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "char",
                                                                                    "value" => "\n"
                                                                                ]))
                                                                        ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "\\"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_opt",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "value_token_touch",
                                                                    "value" => "\r"
                                                                ]))
                                                        ])),
                                                        (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "___"
                        ])),
                        (crate::value!([
                            "emit" => "op_mod_kle",
                            "children" =>
                                (crate::value!([
                                    "emit" => "sequence",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "T_EOL"
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "_"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "T_EOL"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "\n"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "\r"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_opt",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "value_token_touch",
                                                                    "value" => "\n"
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => ";"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "op_accept",
                                                "children" =>
                                                    (crate::value!([
                                                        "emit" => "value_instance",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "Peek"
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "instarg",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            "emit" => "value_token_touch",
                                                                            "value" => "}"
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "op_accept",
                                                "children" =>
                                                    (crate::value!([
                                                        "emit" => "value_instance",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "Peek"
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "instarg",
                                                                    "children" =>
                                                                        (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "T_OctDigit"
                        ])),
                        (crate::value!([
                            "emit" => "value_token_ccl",
                            "children" =>
                                (crate::value!([
                                    "emit" => "ccl",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "range",
                                            "value" => "07"
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "T_HexDigit"
                        ])),
                        (crate::value!([
                            "emit" => "value_token_ccl",
                            "children" =>
                                (crate::value!([
                                    "emit" => "ccl",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "range",
                                                "value" => "09"
                                            ])),
                                            (crate::value!([
                                                "emit" => "range",
                                                "value" => "AF"
                                            ])),
                                            (crate::value!([
                                                "emit" => "range",
                                                "value" => "af"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "T_EscapeSequence"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "a"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_string",
                                                            "value" => ""
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "b"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_string",
                                                            "value" => ""
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "f"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_string",
                                                            "value" => ""
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "n"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_string",
                                                            "value" => "\n"
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "r"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_string",
                                                            "value" => "\r"
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "t"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_string",
                                                            "value" => "\t"
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "v"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_string",
                                                            "value" => ""
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_OctDigit"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_OctDigit"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_OctDigit"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "chr"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "op_binary_add",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "op_binary_add",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "op_binary_mul",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                (crate::value!([
                                                                                                                    "emit" => "call",
                                                                                                                    "children" =>
                                                                                                                        (crate::value!([
                                                                                                                            (crate::value!([
                                                                                                                                "emit" => "identifier",
                                                                                                                                "value" => "int"
                                                                                                                            ])),
                                                                                                                            (crate::value!([
                                                                                                                                "emit" => "callarg",
                                                                                                                                "children" =>
                                                                                                                                    (crate::value!([
                                                                                                                                        "emit" => "capture_index",
                                                                                                                                        "children" =>
                                                                                                                                            (crate::value!([
                                                                                                                                                "emit" => "value_integer",
                                                                                                                                                "value" => 1
                                                                                                                                            ]))
                                                                                                                                    ]))
                                                                                                                            ]))
                                                                                                                        ]))
                                                                                                                ])),
                                                                                                                (crate::value!([
                                                                                                                    "emit" => "value_integer",
                                                                                                                    "value" => 64
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "op_binary_mul",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                (crate::value!([
                                                                                                                    "emit" => "call",
                                                                                                                    "children" =>
                                                                                                                        (crate::value!([
                                                                                                                            (crate::value!([
                                                                                                                                "emit" => "identifier",
                                                                                                                                "value" => "int"
                                                                                                                            ])),
                                                                                                                            (crate::value!([
                                                                                                                                "emit" => "callarg",
                                                                                                                                "children" =>
                                                                                                                                    (crate::value!([
                                                                                                                                        "emit" => "capture_index",
                                                                                                                                        "children" =>
                                                                                                                                            (crate::value!([
                                                                                                                                                "emit" => "value_integer",
                                                                                                                                                "value" => 2
                                                                                                                                            ]))
                                                                                                                                    ]))
                                                                                                                            ]))
                                                                                                                        ]))
                                                                                                                ])),
                                                                                                                (crate::value!([
                                                                                                                    "emit" => "value_integer",
                                                                                                                    "value" => 8
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "call",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "int"
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "callarg",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "capture_index",
                                                                                                                "children" =>
                                                                                                                    (crate::value!([
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
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "x"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_HexDigit"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_HexDigit"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "chr"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "call",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "int"
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "callarg",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    "emit" => "op_binary_add",
                                                                                                    "children" =>
                                                                                                        (crate::value!([
                                                                                                            (crate::value!([
                                                                                                                "emit" => "value_string",
                                                                                                                "value" => "0x"
                                                                                                            ])),
                                                                                                            (crate::value!([
                                                                                                                "emit" => "call",
                                                                                                                "children" =>
                                                                                                                    (crate::value!([
                                                                                                                        (crate::value!([
                                                                                                                            "emit" => "rvalue",
                                                                                                                            "children" =>
                                                                                                                                (crate::value!([
                                                                                                                                    (crate::value!([
                                                                                                                                        "emit" => "capture_index",
                                                                                                                                        "children" =>
                                                                                                                                            (crate::value!([
                                                                                                                                                "emit" => "value_integer",
                                                                                                                                                "value" => 0
                                                                                                                                            ]))
                                                                                                                                    ])),
                                                                                                                                    (crate::value!([
                                                                                                                                        "emit" => "attribute",
                                                                                                                                        "children" =>
                                                                                                                                            (crate::value!([
                                                                                                                                                "emit" => "value_string",
                                                                                                                                                "value" => "substr"
                                                                                                                                            ]))
                                                                                                                                    ]))
                                                                                                                                ]))
                                                                                                                        ])),
                                                                                                                        (crate::value!([
                                                                                                                            "emit" => "callarg",
                                                                                                                            "children" =>
                                                                                                                                (crate::value!([
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
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "u"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_HexDigit"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_HexDigit"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_HexDigit"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_HexDigit"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "chr"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "call",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "int"
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "callarg",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    "emit" => "op_binary_add",
                                                                                                    "children" =>
                                                                                                        (crate::value!([
                                                                                                            (crate::value!([
                                                                                                                "emit" => "value_string",
                                                                                                                "value" => "0x"
                                                                                                            ])),
                                                                                                            (crate::value!([
                                                                                                                "emit" => "call",
                                                                                                                "children" =>
                                                                                                                    (crate::value!([
                                                                                                                        (crate::value!([
                                                                                                                            "emit" => "rvalue",
                                                                                                                            "children" =>
                                                                                                                                (crate::value!([
                                                                                                                                    (crate::value!([
                                                                                                                                        "emit" => "capture_index",
                                                                                                                                        "children" =>
                                                                                                                                            (crate::value!([
                                                                                                                                                "emit" => "value_integer",
                                                                                                                                                "value" => 0
                                                                                                                                            ]))
                                                                                                                                    ])),
                                                                                                                                    (crate::value!([
                                                                                                                                        "emit" => "attribute",
                                                                                                                                        "children" =>
                                                                                                                                            (crate::value!([
                                                                                                                                                "emit" => "value_string",
                                                                                                                                                "value" => "substr"
                                                                                                                                            ]))
                                                                                                                                    ]))
                                                                                                                                ]))
                                                                                                                        ])),
                                                                                                                        (crate::value!([
                                                                                                                            "emit" => "callarg",
                                                                                                                            "children" =>
                                                                                                                                (crate::value!([
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
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "U"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_HexDigit"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_HexDigit"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_HexDigit"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_HexDigit"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_HexDigit"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_HexDigit"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_HexDigit"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_HexDigit"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "chr"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "call",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "int"
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "callarg",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    "emit" => "op_binary_add",
                                                                                                    "children" =>
                                                                                                        (crate::value!([
                                                                                                            (crate::value!([
                                                                                                                "emit" => "value_string",
                                                                                                                "value" => "0x"
                                                                                                            ])),
                                                                                                            (crate::value!([
                                                                                                                "emit" => "call",
                                                                                                                "children" =>
                                                                                                                    (crate::value!([
                                                                                                                        (crate::value!([
                                                                                                                            "emit" => "rvalue",
                                                                                                                            "children" =>
                                                                                                                                (crate::value!([
                                                                                                                                    (crate::value!([
                                                                                                                                        "emit" => "capture_index",
                                                                                                                                        "children" =>
                                                                                                                                            (crate::value!([
                                                                                                                                                "emit" => "value_integer",
                                                                                                                                                "value" => 0
                                                                                                                                            ]))
                                                                                                                                    ])),
                                                                                                                                    (crate::value!([
                                                                                                                                        "emit" => "attribute",
                                                                                                                                        "children" =>
                                                                                                                                            (crate::value!([
                                                                                                                                                "emit" => "value_string",
                                                                                                                                                "value" => "substr"
                                                                                                                                            ]))
                                                                                                                                    ]))
                                                                                                                                ]))
                                                                                                                        ])),
                                                                                                                        (crate::value!([
                                                                                                                            "emit" => "callarg",
                                                                                                                            "children" =>
                                                                                                                                (crate::value!([
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
                                            (crate::value!([
                                                "emit" => "value_token_any"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "T_Identifier"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "call",
                                            "children" =>
                                                (crate::value!([
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "ast"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "callarg",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "value_string",
                                                                "value" => "identifier"
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "callarg",
                                                        "children" =>
                                                            (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "T_Consumable"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "sequence",
                                            "children" =>
                                                (crate::value!([
                                                    (crate::value!([
                                                        "emit" => "value_token_ccl",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "ccl",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "range",
                                                                            "value" => "AZ"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "char",
                                                                            "value" => "_"
                                                                        ]))
                                                                    ]))
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "op_mod_kle",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "value_token_ccl",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        "emit" => "ccl",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                (crate::value!([
                                                                                    "emit" => "range",
                                                                                    "value" => "09"
                                                                                ])),
                                                                                (crate::value!([
                                                                                    "emit" => "range",
                                                                                    "value" => "AZ"
                                                                                ])),
                                                                                (crate::value!([
                                                                                    "emit" => "char",
                                                                                    "value" => "_"
                                                                                ])),
                                                                                (crate::value!([
                                                                                    "emit" => "range",
                                                                                    "value" => "az"
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "call",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "ast"
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            "emit" => "value_string",
                                                                            "value" => "identifier"
                                                                        ]))
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            "emit" => "capture_index",
                                                                            "children" =>
                                                                                (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "T_Alias"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "sequence",
                                            "children" =>
                                                (crate::value!([
                                                    (crate::value!([
                                                        "emit" => "value_token_ccl",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "ccl",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "range",
                                                                            "value" => "AZ"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "char",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "range",
                                                                            "value" => "az"
                                                                        ]))
                                                                    ]))
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "op_mod_kle",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "value_token_ccl",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        "emit" => "ccl",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                (crate::value!([
                                                                                    "emit" => "range",
                                                                                    "value" => "09"
                                                                                ])),
                                                                                (crate::value!([
                                                                                    "emit" => "range",
                                                                                    "value" => "AZ"
                                                                                ])),
                                                                                (crate::value!([
                                                                                    "emit" => "char",
                                                                                    "value" => "_"
                                                                                ])),
                                                                                (crate::value!([
                                                                                    "emit" => "range",
                                                                                    "value" => "az"
                                                                                ]))
                                                                            ]))
                                                                    ]))
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "call",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "ast"
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            "emit" => "value_string",
                                                                            "value" => "value_string"
                                                                        ]))
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            "emit" => "capture_index",
                                                                            "children" =>
                                                                                (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "T_String"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "sequence",
                                            "children" =>
                                                (crate::value!([
                                                    (crate::value!([
                                                        "emit" => "value_token_touch",
                                                        "value" => "\""
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "op_mod_kle",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "block",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "sequence",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    (crate::value!([
                                                                                        "emit" => "value_token_touch",
                                                                                        "value" => "\\"
                                                                                    ])),
                                                                                    (crate::value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "T_EscapeSequence"
                                                                                    ]))
                                                                                ]))
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "value_token_ccl",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "ccl_neg",
                                                                                    "children" =>
                                                                                        (crate::value!([
                                                                                            (crate::value!([
                                                                                                "emit" => "char",
                                                                                                "value" => "\\"
                                                                                            ])),
                                                                                            (crate::value!([
                                                                                                "emit" => "char",
                                                                                                "value" => "\""
                                                                                            ]))
                                                                                        ]))
                                                                                ]))
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "sequence",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    (crate::value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "EOF"
                                                                                    ])),
                                                                                    (crate::value!([
                                                                                        "emit" => "call",
                                                                                        "children" =>
                                                                                            (crate::value!([
                                                                                                (crate::value!([
                                                                                                    "emit" => "identifier",
                                                                                                    "value" => "error"
                                                                                                ])),
                                                                                                (crate::value!([
                                                                                                    "emit" => "callarg",
                                                                                                    "children" =>
                                                                                                        (crate::value!([
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
                                                    (crate::value!([
                                                        "emit" => "call",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "str_join"
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            "emit" => "value_string",
                                                                            "value" => ""
                                                                        ]))
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            "emit" => "capture_index",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "value_integer",
                                                                                    "value" => 2
                                                                                ]))
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "value_instance",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "Expect"
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "instarg",
                                                                    "children" =>
                                                                        (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "T_Touch"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "sequence",
                                            "children" =>
                                                (crate::value!([
                                                    (crate::value!([
                                                        "emit" => "value_token_touch",
                                                        "value" => "'"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "op_mod_kle",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "block",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "sequence",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    (crate::value!([
                                                                                        "emit" => "value_token_touch",
                                                                                        "value" => "\\"
                                                                                    ])),
                                                                                    (crate::value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "T_EscapeSequence"
                                                                                    ]))
                                                                                ]))
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "value_token_ccl",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "ccl_neg",
                                                                                    "children" =>
                                                                                        (crate::value!([
                                                                                            (crate::value!([
                                                                                                "emit" => "char",
                                                                                                "value" => "\\"
                                                                                            ])),
                                                                                            (crate::value!([
                                                                                                "emit" => "char",
                                                                                                "value" => "'"
                                                                                            ]))
                                                                                        ]))
                                                                                ]))
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "sequence",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    (crate::value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "EOF"
                                                                                    ])),
                                                                                    (crate::value!([
                                                                                        "emit" => "call",
                                                                                        "children" =>
                                                                                            (crate::value!([
                                                                                                (crate::value!([
                                                                                                    "emit" => "identifier",
                                                                                                    "value" => "error"
                                                                                                ])),
                                                                                                (crate::value!([
                                                                                                    "emit" => "callarg",
                                                                                                    "children" =>
                                                                                                        (crate::value!([
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
                                                    (crate::value!([
                                                        "emit" => "call",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "str_join"
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            "emit" => "value_string",
                                                                            "value" => ""
                                                                        ]))
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            "emit" => "capture_index",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "value_integer",
                                                                                    "value" => 2
                                                                                ]))
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "value_instance",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "Expect"
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "instarg",
                                                                    "children" =>
                                                                        (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "T_Integer"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "call",
                                            "children" =>
                                                (crate::value!([
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "ast"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "callarg",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "value_string",
                                                                "value" => "value_integer"
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "callarg",
                                                        "children" =>
                                                            (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "T_Float"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "call",
                                            "children" =>
                                                (crate::value!([
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "ast"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "callarg",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "value_string",
                                                                "value" => "value_float"
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "callarg",
                                                        "children" =>
                                                            (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "CclChar"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "\\"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_EscapeSequence"
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "value_token_ccl",
                                                "children" =>
                                                    (crate::value!([
                                                        "emit" => "ccl_neg",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "char",
                                                                "value" => ">"
                                                            ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "EOF"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "error"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "CclRange"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "CclChar"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "-"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "CclChar"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "range"
                                                                            ]))
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "op_binary_add",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "capture_index",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    "emit" => "value_integer",
                                                                                                    "value" => 1
                                                                                                ]))
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "capture_index",
                                                                                            "children" =>
                                                                                                (crate::value!([
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
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "CclChar"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Ccl"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "^"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_kle",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "CclRange"
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "ccl_neg"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "op_mod_kle",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "CclRange"
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Subscript"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "sequence",
                                            "children" =>
                                                (crate::value!([
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "_"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "value_token_touch",
                                                        "value" => "["
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "_"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "Expression"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "value_token_touch",
                                                        "value" => "]"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "call",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "ast"
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Attribute"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "sequence",
                                            "children" =>
                                                (crate::value!([
                                                    (crate::value!([
                                                        "emit" => "value_token_touch",
                                                        "value" => "."
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "_"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "T_Alias"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "call",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "ast"
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Capture"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "$"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_Alias"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "capture_alias"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "$"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_Integer"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "capture_index"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "$"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "("
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "___"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Expression"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => ")"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "capture_expr"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "$"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "error"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Variable"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "T_Identifier"
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "Capture"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Lvalue"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "sequence",
                                            "children" =>
                                                (crate::value!([
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "Variable"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "op_mod_kle",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "Subscript"
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "call",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "ast"
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Load"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Lvalue"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "++"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "inplace_post_inc"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Lvalue"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "--"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "inplace_post_dec"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "++"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Lvalue"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "inplace_pre_inc"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "--"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Lvalue"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "inplace_pre_dec"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "Variable"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Parselet"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "sequence",
                                            "children" =>
                                                (crate::value!([
                                                    (crate::value!([
                                                        "emit" => "value_token_touch",
                                                        "value" => "@"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "_"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "op_mod_opt",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "ParseletGenerics"
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "_"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "op_mod_opt",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "ParseletArguments"
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "value_instance",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "Expect"
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "instarg",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            "emit" => "call",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    (crate::value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "Block"
                                                                                    ])),
                                                                                    (crate::value!([
                                                                                        "emit" => "callarg",
                                                                                        "children" =>
                                                                                            (crate::value!([
                                                                                                "emit" => "value_string",
                                                                                                "value" => "body"
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "call",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "ast"
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "ParseletGeneric"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "sequence",
                                            "children" =>
                                                (crate::value!([
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "T_Identifier"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "_"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "op_mod_opt",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "sequence",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "value_token_touch",
                                                                            "value" => ":"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "value_instance",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    (crate::value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "Expect"
                                                                                    ])),
                                                                                    (crate::value!([
                                                                                        "emit" => "instarg",
                                                                                        "children" =>
                                                                                            (crate::value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "Atomic"
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "call",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "ast"
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "ParseletGenerics"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "sequence",
                                            "children" =>
                                                (crate::value!([
                                                    (crate::value!([
                                                        "emit" => "value_token_touch",
                                                        "value" => "<"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "_"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "___"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "op_mod_kle",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "sequence",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "ParseletGeneric"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "___"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "op_mod_opt",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "sequence",
                                                                                    "children" =>
                                                                                        (crate::value!([
                                                                                            (crate::value!([
                                                                                                "emit" => "value_token_touch",
                                                                                                "value" => ","
                                                                                            ])),
                                                                                            (crate::value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "_"
                                                                                            ])),
                                                                                            (crate::value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "___"
                                                                                            ]))
                                                                                        ]))
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "___"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "value_instance",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "Expect"
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "instarg",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            "emit" => "value_token_touch",
                                                                            "value" => ">"
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "_"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "___"
                                                    ]))
                                                ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "ParseletArgument"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "sequence",
                                            "children" =>
                                                (crate::value!([
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "T_Identifier"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "_"
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "op_mod_opt",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "sequence",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "value_token_touch",
                                                                            "value" => "="
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "value_instance",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    (crate::value!([
                                                                                        "emit" => "identifier",
                                                                                        "value" => "Expect"
                                                                                    ])),
                                                                                    (crate::value!([
                                                                                        "emit" => "instarg",
                                                                                        "children" =>
                                                                                            (crate::value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "Expression"
                                                                                            ]))
                                                                                    ]))
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "call",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "ast"
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "callarg",
                                                                    "children" =>
                                                                        (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "ParseletArguments"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "op_mod_pos",
                                            "children" =>
                                                (crate::value!([
                                                    "emit" => "sequence",
                                                    "children" =>
                                                        (crate::value!([
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "ParseletArgument"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "op_mod_opt",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        "emit" => "sequence",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                (crate::value!([
                                                                                    "emit" => "value_token_touch",
                                                                                    "value" => ","
                                                                                ])),
                                                                                (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "StaticParseletInstance"
                        ])),
                        (crate::value!([
                            "emit" => "block",
                            "children" =>
                                (crate::value!([
                                    (crate::value!([
                                        "emit" => "identifier",
                                        "value" => "T_Consumable"
                                    ])),
                                    (crate::value!([
                                        "emit" => "identifier",
                                        "value" => "Parselet"
                                    ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "ParseletInstanceArgument"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_Identifier"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => ":"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Atomic"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "instarg_named"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Atomic"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "ParseletInstance"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "StaticParseletInstance"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "<"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_pos",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "sequence",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "ParseletInstanceArgument"
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "op_mod_opt",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        "emit" => "sequence",
                                                                                        "children" =>
                                                                                            (crate::value!([
                                                                                                (crate::value!([
                                                                                                    "emit" => "value_token_touch",
                                                                                                    "value" => ","
                                                                                                ])),
                                                                                                (crate::value!([
                                                                                                    "emit" => "identifier",
                                                                                                    "value" => "_"
                                                                                                ]))
                                                                                            ]))
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => ">"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "value_instance"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "StaticParseletInstance"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "InlineAssignment"
                        ])),
                        (crate::value!([
                            "emit" => "call",
                            "children" =>
                                (crate::value!([
                                    (crate::value!([
                                        "emit" => "value_instance",
                                        "children" =>
                                            (crate::value!([
                                                (crate::value!([
                                                    "emit" => "identifier",
                                                    "value" => "Assignment"
                                                ])),
                                                (crate::value!([
                                                    "emit" => "instarg",
                                                    "children" =>
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Expression"
                                                        ]))
                                                ]))
                                            ]))
                                    ])),
                                    (crate::value!([
                                        "emit" => "callarg",
                                        "children" =>
                                            (crate::value!([
                                                "emit" => "value_string",
                                                "value" => "copy"
                                            ]))
                                    ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "InlineSequenceItem"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_Alias"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "=>"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "InlineAssignment"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "alias"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "LogicalOr"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "=>"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "InlineAssignment"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "alias"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "InlineAssignment"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "InlineSequence"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "sequence",
                                            "children" =>
                                                (crate::value!([
                                                    (crate::value!([
                                                        "emit" => "op_mod_pos",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "sequence",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "InlineSequenceItem"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "___"
                                                                        ]))
                                                                    ]))
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "op_if",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "op_logical_or",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            (crate::value!([
                                                                                "emit" => "op_logical_and",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "comparison",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "call",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                (crate::value!([
                                                                                                                    "emit" => "identifier",
                                                                                                                    "value" => "type"
                                                                                                                ])),
                                                                                                                (crate::value!([
                                                                                                                    "emit" => "callarg",
                                                                                                                    "children" =>
                                                                                                                        (crate::value!([
                                                                                                                            "emit" => "capture_index",
                                                                                                                            "children" =>
                                                                                                                                (crate::value!([
                                                                                                                                    "emit" => "value_integer",
                                                                                                                                    "value" => 1
                                                                                                                                ]))
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "cmp_eq",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "value_string",
                                                                                                                "value" => "list"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "comparison",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "rvalue",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                (crate::value!([
                                                                                                                    "emit" => "capture_index",
                                                                                                                    "children" =>
                                                                                                                        (crate::value!([
                                                                                                                            "emit" => "value_integer",
                                                                                                                            "value" => 1
                                                                                                                        ]))
                                                                                                                ])),
                                                                                                                (crate::value!([
                                                                                                                    "emit" => "attribute",
                                                                                                                    "children" =>
                                                                                                                        (crate::value!([
                                                                                                                            "emit" => "value_string",
                                                                                                                            "value" => "len"
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "cmp_gt",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "value_integer",
                                                                                                                "value" => 1
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "comparison",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "rvalue",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "capture_index",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "value_integer",
                                                                                                                "value" => 1
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "item",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "value_string",
                                                                                                                "value" => "emit"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "cmp_eq",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    "emit" => "value_string",
                                                                                                    "value" => "alias"
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "call",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "ast"
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "callarg",
                                                                                "children" =>
                                                                                    (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "InlineSequences"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "InlineSequence"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_pos",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "sequence",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "___"
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "|"
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "_"
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "___"
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "value_instance",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Expect"
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "instarg",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    "emit" => "identifier",
                                                                                                    "value" => "InlineSequence"
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "block"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "InlineSequence"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "InlineList"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "InlineAssignment"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "___"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_pos",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "sequence",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => ","
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "_"
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "___"
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "InlineAssignment"
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "___"
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_opt",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "sequence",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => ","
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "_"
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "___"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "list"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "op_mod_opt",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "InlineAssignment"
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "___"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => ","
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "___"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "CallArgument"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_Identifier"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "="
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Not"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_ccl",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        "emit" => "ccl",
                                                                                        "children" =>
                                                                                            (crate::value!([
                                                                                                (crate::value!([
                                                                                                    "emit" => "char",
                                                                                                    "value" => ">"
                                                                                                ])),
                                                                                                (crate::value!([
                                                                                                    "emit" => "char",
                                                                                                    "value" => "="
                                                                                                ]))
                                                                                            ]))
                                                                                    ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "InlineSequences"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "callarg_named"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "InlineSequences"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "CallArguments"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "sequence",
                                            "children" =>
                                                (crate::value!([
                                                    (crate::value!([
                                                        "emit" => "op_binary_add",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "CallArgument"
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "value_instance",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Repeat"
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "instarg",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        "emit" => "sequence",
                                                                                        "children" =>
                                                                                            (crate::value!([
                                                                                                (crate::value!([
                                                                                                    "emit" => "sequence",
                                                                                                    "children" =>
                                                                                                        (crate::value!([
                                                                                                            (crate::value!([
                                                                                                                "emit" => "value_token_touch",
                                                                                                                "value" => ","
                                                                                                            ])),
                                                                                                            (crate::value!([
                                                                                                                "emit" => "identifier",
                                                                                                                "value" => "_"
                                                                                                            ]))
                                                                                                        ]))
                                                                                                ])),
                                                                                                (crate::value!([
                                                                                                    "emit" => "identifier",
                                                                                                    "value" => "___"
                                                                                                ])),
                                                                                                (crate::value!([
                                                                                                    "emit" => "identifier",
                                                                                                    "value" => "CallArgument"
                                                                                                ]))
                                                                                            ]))
                                                                                    ]))
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "instarg_named",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "min"
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "value_integer",
                                                                                            "value" => 0
                                                                                        ]))
                                                                                    ]))
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "instarg_named",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "blur"
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "value_false"
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "op_mod_opt",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "sequence",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "value_token_touch",
                                                                            "value" => ","
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "_"
                                                                        ]))
                                                                    ]))
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "identifier",
                                                        "value" => "___"
                                                    ]))
                                                ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "TokenLiteral"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "'"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_Touch"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "'"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "value_token_match"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_Touch"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "value_token_touch"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "Chars"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "<"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Ccl"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => ">"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "value_token_ccls"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "Chars"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "value_token_anys"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "Char"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "<"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Ccl"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => ">"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "value_token_ccl"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "Char"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "value_token_any"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "Void"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Token"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "("
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "___"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => ")"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "dict"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "("
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "___"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "block",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "InlineList"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "InlineSequences"
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "___"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => ")"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "@"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "("
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "___"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "block",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "InlineList"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "InlineSequences"
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "___"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => ")"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "area"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "Block"
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "TokenLiteral"
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "ParseletInstance"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "("
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "___"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_opt",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "CallArguments"
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "___"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => ")"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "call"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "ParseletInstance"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "TokenModifier"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Token"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "+"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_mod_pos"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Token"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "*"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_mod_kle"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Token"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "?"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_mod_opt"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "Token"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Literal"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "true"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "value_true"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "false"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "value_false"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "void"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "value_void"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "null"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "value_null"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_String"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "value_string"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "T_Float"
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "T_Integer"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Atomic"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "Literal"
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "TokenModifier"
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "if"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Expression"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "___"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Statement"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_opt",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "sequence",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "___"
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "value_instance",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Keyword"
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "instarg",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    "emit" => "value_token_touch",
                                                                                                    "value" => "else"
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "_"
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "___"
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "value_instance",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Expect"
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "instarg",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    "emit" => "identifier",
                                                                                                    "value" => "Statement"
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_if"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "for"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Lvalue"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_instance",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Expect"
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "instarg",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    "emit" => "value_token_touch",
                                                                                                    "value" => "in"
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "ExpressionList"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "___"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Statement"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_for"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "loop"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Expression"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "___"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Block"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_loop"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "loop"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "___"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Block"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_loop"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "Load"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Rvalue"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Rvalue"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "("
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "___"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_opt",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "CallArguments"
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => ")"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "call"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Rvalue"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_kle",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "block",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Attribute"
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Subscript"
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "rvalue"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "Atomic"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Unary"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "-"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Not"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "-"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Unary"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_unary_neg"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "!"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Unary"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_unary_not"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "*"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Unary"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_deref"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Rvalue"
                                                        ])),
                                                        (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "MulDiv"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "MulDiv"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "*"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Not"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_ccl",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        "emit" => "ccl",
                                                                                        "children" =>
                                                                                            (crate::value!([
                                                                                                "emit" => "char",
                                                                                                "value" => "="
                                                                                            ]))
                                                                                    ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Unary"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_binary_mul"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "MulDiv"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "//"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Not"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_ccl",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        "emit" => "ccl",
                                                                                        "children" =>
                                                                                            (crate::value!([
                                                                                                "emit" => "char",
                                                                                                "value" => "="
                                                                                            ]))
                                                                                    ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Unary"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_binary_divi"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "MulDiv"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "/"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Not"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_ccl",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        "emit" => "ccl",
                                                                                        "children" =>
                                                                                            (crate::value!([
                                                                                                "emit" => "char",
                                                                                                "value" => "="
                                                                                            ]))
                                                                                    ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Unary"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_binary_div"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "MulDiv"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "%"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Not"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_ccl",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        "emit" => "ccl",
                                                                                        "children" =>
                                                                                            (crate::value!([
                                                                                                "emit" => "char",
                                                                                                "value" => "="
                                                                                            ]))
                                                                                    ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Unary"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_binary_mod"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "Unary"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "AddSub"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "AddSub"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "+"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Not"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_ccl",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        "emit" => "ccl",
                                                                                        "children" =>
                                                                                            (crate::value!([
                                                                                                (crate::value!([
                                                                                                    "emit" => "char",
                                                                                                    "value" => "+"
                                                                                                ])),
                                                                                                (crate::value!([
                                                                                                    "emit" => "char",
                                                                                                    "value" => "="
                                                                                                ]))
                                                                                            ]))
                                                                                    ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "MulDiv"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_binary_add"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "AddSub"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "-"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Not"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_ccl",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        "emit" => "ccl",
                                                                                        "children" =>
                                                                                            (crate::value!([
                                                                                                (crate::value!([
                                                                                                    "emit" => "char",
                                                                                                    "value" => "-"
                                                                                                ])),
                                                                                                (crate::value!([
                                                                                                    "emit" => "char",
                                                                                                    "value" => "="
                                                                                                ]))
                                                                                            ]))
                                                                                    ]))
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "MulDiv"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_binary_sub"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "MulDiv"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Comparison"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "AddSub"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_pos",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "block",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            (crate::value!([
                                                                                "emit" => "sequence",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "=="
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "_"
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "value_instance",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "Expect"
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "instarg",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "identifier",
                                                                                                                "value" => "AddSub"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "call",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "ast"
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "callarg",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "value_string",
                                                                                                                "value" => "cmp_eq"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "sequence",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "!="
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "_"
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "value_instance",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "Expect"
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "instarg",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "identifier",
                                                                                                                "value" => "AddSub"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "call",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "ast"
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "callarg",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "value_string",
                                                                                                                "value" => "cmp_neq"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "sequence",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "<="
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "_"
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "value_instance",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "Expect"
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "instarg",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "identifier",
                                                                                                                "value" => "AddSub"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "call",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "ast"
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "callarg",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "value_string",
                                                                                                                "value" => "cmp_lteq"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "sequence",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => ">="
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "_"
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "value_instance",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "Expect"
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "instarg",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "identifier",
                                                                                                                "value" => "AddSub"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "call",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "ast"
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "callarg",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "value_string",
                                                                                                                "value" => "cmp_gteq"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "sequence",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => "<"
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "_"
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "value_instance",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "Expect"
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "instarg",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "identifier",
                                                                                                                "value" => "AddSub"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "call",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "ast"
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "callarg",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "value_string",
                                                                                                                "value" => "cmp_lt"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "sequence",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "value_token_touch",
                                                                                            "value" => ">"
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "_"
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "value_instance",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "Expect"
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "instarg",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "identifier",
                                                                                                                "value" => "AddSub"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "call",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "ast"
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "callarg",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
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
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "comparison"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "AddSub"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "LogicalAnd"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "LogicalAnd"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "&&"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Comparison"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_logical_and"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "Comparison"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "LogicalOr"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "LogicalOr"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "||"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "LogicalAnd"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_logical_or"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "LogicalAnd"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Expression"
                        ])),
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "LogicalOr"
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "ExpressionList"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Expression"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_pos",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "sequence",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => ","
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "_"
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Expression"
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_opt",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "sequence",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => ","
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "_"
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "list"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "op_mod_opt",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "Expression"
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "sequence",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "value_token_touch",
                                                                        "value" => ","
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "_"
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "list"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "Expression"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Assignment"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    (crate::value!([
                                        "emit" => "gen",
                                        "children" =>
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "Source"
                                            ]))
                                    ])),
                                    (crate::value!([
                                        "emit" => "sig",
                                        "children" =>
                                            (crate::value!([
                                                (crate::value!([
                                                    "emit" => "identifier",
                                                    "value" => "mode"
                                                ])),
                                                (crate::value!([
                                                    "emit" => "value_string",
                                                    "value" => "hold"
                                                ]))
                                            ]))
                                    ])),
                                    (crate::value!([
                                        "emit" => "body",
                                        "children" =>
                                            (crate::value!([
                                                (crate::value!([
                                                    "emit" => "sequence",
                                                    "children" =>
                                                        (crate::value!([
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "Lvalue"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "_"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "value_token_touch",
                                                                "value" => "+="
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "_"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "value_instance",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Expect"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "instarg",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Self"
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "call",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "ast"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "callarg",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "op_binary_add",
                                                                                    "children" =>
                                                                                        (crate::value!([
                                                                                            (crate::value!([
                                                                                                "emit" => "value_string",
                                                                                                "value" => "assign_add_"
                                                                                            ])),
                                                                                            (crate::value!([
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
                                                (crate::value!([
                                                    "emit" => "sequence",
                                                    "children" =>
                                                        (crate::value!([
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "Lvalue"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "_"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "value_token_touch",
                                                                "value" => "-="
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "_"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "value_instance",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Expect"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "instarg",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Self"
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "call",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "ast"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "callarg",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "op_binary_add",
                                                                                    "children" =>
                                                                                        (crate::value!([
                                                                                            (crate::value!([
                                                                                                "emit" => "value_string",
                                                                                                "value" => "assign_sub_"
                                                                                            ])),
                                                                                            (crate::value!([
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
                                                (crate::value!([
                                                    "emit" => "sequence",
                                                    "children" =>
                                                        (crate::value!([
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "Lvalue"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "_"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "value_token_touch",
                                                                "value" => "*="
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "_"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "value_instance",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Expect"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "instarg",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Self"
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "call",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "ast"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "callarg",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "op_binary_add",
                                                                                    "children" =>
                                                                                        (crate::value!([
                                                                                            (crate::value!([
                                                                                                "emit" => "value_string",
                                                                                                "value" => "assign_mul_"
                                                                                            ])),
                                                                                            (crate::value!([
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
                                                (crate::value!([
                                                    "emit" => "sequence",
                                                    "children" =>
                                                        (crate::value!([
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "Lvalue"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "_"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "value_token_touch",
                                                                "value" => "/="
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "_"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "value_instance",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Expect"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "instarg",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Self"
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "call",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "ast"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "callarg",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "op_binary_add",
                                                                                    "children" =>
                                                                                        (crate::value!([
                                                                                            (crate::value!([
                                                                                                "emit" => "value_string",
                                                                                                "value" => "assign_div_"
                                                                                            ])),
                                                                                            (crate::value!([
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
                                                (crate::value!([
                                                    "emit" => "sequence",
                                                    "children" =>
                                                        (crate::value!([
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "Lvalue"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "_"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "value_token_touch",
                                                                "value" => "//="
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "_"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "value_instance",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Expect"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "instarg",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Self"
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "call",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "ast"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "callarg",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "op_binary_add",
                                                                                    "children" =>
                                                                                        (crate::value!([
                                                                                            (crate::value!([
                                                                                                "emit" => "value_string",
                                                                                                "value" => "assign_divi_"
                                                                                            ])),
                                                                                            (crate::value!([
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
                                                (crate::value!([
                                                    "emit" => "sequence",
                                                    "children" =>
                                                        (crate::value!([
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "Lvalue"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "_"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "value_token_touch",
                                                                "value" => "%="
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "_"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "value_instance",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Expect"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "instarg",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Self"
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "call",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "ast"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "callarg",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "op_binary_add",
                                                                                    "children" =>
                                                                                        (crate::value!([
                                                                                            (crate::value!([
                                                                                                "emit" => "value_string",
                                                                                                "value" => "assign_mod_"
                                                                                            ])),
                                                                                            (crate::value!([
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
                                                (crate::value!([
                                                    "emit" => "sequence",
                                                    "children" =>
                                                        (crate::value!([
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "Lvalue"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "_"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "value_token_touch",
                                                                "value" => "="
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "value_instance",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Not"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "instarg",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "value_token_ccl",
                                                                                    "children" =>
                                                                                        (crate::value!([
                                                                                            "emit" => "ccl",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "char",
                                                                                                        "value" => ">"
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "char",
                                                                                                        "value" => "="
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "_"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "value_instance",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Expect"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "instarg",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Self"
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "call",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "ast"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "callarg",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "op_binary_add",
                                                                                    "children" =>
                                                                                        (crate::value!([
                                                                                            (crate::value!([
                                                                                                "emit" => "value_string",
                                                                                                "value" => "assign_"
                                                                                            ])),
                                                                                            (crate::value!([
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
                                                (crate::value!([
                                                    "emit" => "identifier",
                                                    "value" => "Source"
                                                ]))
                                            ]))
                                    ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Statement"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "accept"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_opt",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "Expression"
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_accept"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "break"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_opt",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "Expression"
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_break"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "continue"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_opt",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "Expression"
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_continue"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "exit"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_opt",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "Expression"
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_exit"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "next"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_next"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "push"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_opt",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "Expression"
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_push"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "reject"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_reject"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "repeat"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_repeat"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "reset"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_reset"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "return"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_opt",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "identifier",
                                                                    "value" => "Expression"
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "op_accept"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "call",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Assignment"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "ExpressionList"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "callarg",
                                                            "children" =>
                                                                (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Block"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    (crate::value!([
                                        "emit" => "sig",
                                        "children" =>
                                            (crate::value!([
                                                (crate::value!([
                                                    "emit" => "identifier",
                                                    "value" => "emit"
                                                ])),
                                                (crate::value!([
                                                    "emit" => "value_string",
                                                    "value" => "block"
                                                ]))
                                            ]))
                                    ])),
                                    (crate::value!([
                                        "emit" => "body",
                                        "children" =>
                                            (crate::value!([
                                                (crate::value!([
                                                    "emit" => "sequence",
                                                    "children" =>
                                                        (crate::value!([
                                                            (crate::value!([
                                                                "emit" => "value_token_touch",
                                                                "value" => "{"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "_"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "___"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "value_token_touch",
                                                                "value" => "}"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "call",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "ast"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "callarg",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "value_string",
                                                                                    "value" => "value_void"
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ]))
                                                        ]))
                                                ])),
                                                (crate::value!([
                                                    "emit" => "sequence",
                                                    "children" =>
                                                        (crate::value!([
                                                            (crate::value!([
                                                                "emit" => "value_token_touch",
                                                                "value" => "{"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "_"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "op_mod_kle",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Tokay"
                                                                    ]))
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "_"
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "value_instance",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "Expect"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "instarg",
                                                                            "children" =>
                                                                                (crate::value!([
                                                                                    "emit" => "value_token_touch",
                                                                                    "value" => "}"
                                                                                ]))
                                                                        ]))
                                                                    ]))
                                                            ])),
                                                            (crate::value!([
                                                                "emit" => "call",
                                                                "children" =>
                                                                    (crate::value!([
                                                                        (crate::value!([
                                                                            "emit" => "identifier",
                                                                            "value" => "ast"
                                                                        ])),
                                                                        (crate::value!([
                                                                            "emit" => "callarg",
                                                                            "children" =>
                                                                                (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "SequenceItem"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_Alias"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "=>"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "ExpressionList"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "alias"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Expression"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => "=>"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "ExpressionList"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "alias"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "Statement"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Sequence"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            "emit" => "sequence",
                                            "children" =>
                                                (crate::value!([
                                                    (crate::value!([
                                                        "emit" => "op_mod_pos",
                                                        "children" =>
                                                            (crate::value!([
                                                                "emit" => "identifier",
                                                                "value" => "SequenceItem"
                                                            ]))
                                                    ])),
                                                    (crate::value!([
                                                        "emit" => "op_if",
                                                        "children" =>
                                                            (crate::value!([
                                                                (crate::value!([
                                                                    "emit" => "op_logical_or",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            (crate::value!([
                                                                                "emit" => "op_logical_and",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "comparison",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "call",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                (crate::value!([
                                                                                                                    "emit" => "identifier",
                                                                                                                    "value" => "type"
                                                                                                                ])),
                                                                                                                (crate::value!([
                                                                                                                    "emit" => "callarg",
                                                                                                                    "children" =>
                                                                                                                        (crate::value!([
                                                                                                                            "emit" => "capture_index",
                                                                                                                            "children" =>
                                                                                                                                (crate::value!([
                                                                                                                                    "emit" => "value_integer",
                                                                                                                                    "value" => 1
                                                                                                                                ]))
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "cmp_eq",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "value_string",
                                                                                                                "value" => "list"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "comparison",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "rvalue",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                (crate::value!([
                                                                                                                    "emit" => "capture_index",
                                                                                                                    "children" =>
                                                                                                                        (crate::value!([
                                                                                                                            "emit" => "value_integer",
                                                                                                                            "value" => 1
                                                                                                                        ]))
                                                                                                                ])),
                                                                                                                (crate::value!([
                                                                                                                    "emit" => "attribute",
                                                                                                                    "children" =>
                                                                                                                        (crate::value!([
                                                                                                                            "emit" => "value_string",
                                                                                                                            "value" => "len"
                                                                                                                        ]))
                                                                                                                ]))
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "cmp_gt",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "value_integer",
                                                                                                                "value" => 1
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "comparison",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "rvalue",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    (crate::value!([
                                                                                                        "emit" => "capture_index",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "value_integer",
                                                                                                                "value" => 1
                                                                                                            ]))
                                                                                                    ])),
                                                                                                    (crate::value!([
                                                                                                        "emit" => "item",
                                                                                                        "children" =>
                                                                                                            (crate::value!([
                                                                                                                "emit" => "value_string",
                                                                                                                "value" => "emit"
                                                                                                            ]))
                                                                                                    ]))
                                                                                                ]))
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "cmp_eq",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    "emit" => "value_string",
                                                                                                    "value" => "alias"
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ])),
                                                                (crate::value!([
                                                                    "emit" => "call",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "ast"
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "callarg",
                                                                                "children" =>
                                                                                    (crate::value!([
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
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Sequences"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Sequence"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_pos",
                                                            "children" =>
                                                                (crate::value!([
                                                                    "emit" => "sequence",
                                                                    "children" =>
                                                                        (crate::value!([
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "|"
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "_"
                                                                            ])),
                                                                            (crate::value!([
                                                                                "emit" => "value_instance",
                                                                                "children" =>
                                                                                    (crate::value!([
                                                                                        (crate::value!([
                                                                                            "emit" => "identifier",
                                                                                            "value" => "Expect"
                                                                                        ])),
                                                                                        (crate::value!([
                                                                                            "emit" => "instarg",
                                                                                            "children" =>
                                                                                                (crate::value!([
                                                                                                    "emit" => "identifier",
                                                                                                    "value" => "Sequence"
                                                                                                ]))
                                                                                        ]))
                                                                                    ]))
                                                                            ]))
                                                                        ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "block"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "Sequence"
                                            ]))
                                        ]))
                                ]))
                        ]))
                    ]))
            ])),
            (crate::value!([
                "emit" => "constant",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "Tokay"
                        ])),
                        (crate::value!([
                            "emit" => "value_parselet",
                            "children" =>
                                (crate::value!([
                                    "emit" => "body",
                                    "children" =>
                                        (crate::value!([
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "T_EOL"
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "begin"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Sequences"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "T_EOL"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "begin"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Keyword"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_token_touch",
                                                                                "value" => "end"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "Sequences"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "T_EOL"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "end"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "T_Identifier"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_token_touch",
                                                            "value" => ":"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "_"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "block",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "sequence",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                (crate::value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Literal"
                                                                                ])),
                                                                                (crate::value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "_"
                                                                                ])),
                                                                                (crate::value!([
                                                                                    "emit" => "value_instance",
                                                                                    "children" =>
                                                                                        (crate::value!([
                                                                                            (crate::value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "Peek"
                                                                                            ])),
                                                                                            (crate::value!([
                                                                                                "emit" => "instarg",
                                                                                                "children" =>
                                                                                                    (crate::value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "T_EOL"
                                                                                                    ]))
                                                                                            ]))
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "sequence",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                (crate::value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "Token"
                                                                                ])),
                                                                                (crate::value!([
                                                                                    "emit" => "identifier",
                                                                                    "value" => "_"
                                                                                ])),
                                                                                (crate::value!([
                                                                                    "emit" => "value_instance",
                                                                                    "children" =>
                                                                                        (crate::value!([
                                                                                            (crate::value!([
                                                                                                "emit" => "identifier",
                                                                                                "value" => "Peek"
                                                                                            ])),
                                                                                            (crate::value!([
                                                                                                "emit" => "instarg",
                                                                                                "children" =>
                                                                                                    (crate::value!([
                                                                                                        "emit" => "identifier",
                                                                                                        "value" => "T_EOL"
                                                                                                    ]))
                                                                                            ]))
                                                                                        ]))
                                                                                ]))
                                                                            ]))
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Sequences"
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "value_instance",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "Expect"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "instarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "identifier",
                                                                                "value" => "T_EOL"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "call",
                                                            "children" =>
                                                                (crate::value!([
                                                                    (crate::value!([
                                                                        "emit" => "identifier",
                                                                        "value" => "ast"
                                                                    ])),
                                                                    (crate::value!([
                                                                        "emit" => "callarg",
                                                                        "children" =>
                                                                            (crate::value!([
                                                                                "emit" => "value_string",
                                                                                "value" => "constant"
                                                                            ]))
                                                                    ]))
                                                                ]))
                                                        ]))
                                                    ]))
                                            ])),
                                            (crate::value!([
                                                "emit" => "sequence",
                                                "children" =>
                                                    (crate::value!([
                                                        (crate::value!([
                                                            "emit" => "identifier",
                                                            "value" => "Sequences"
                                                        ])),
                                                        (crate::value!([
                                                            "emit" => "op_mod_opt",
                                                            "children" =>
                                                                (crate::value!([
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
            (crate::value!([
                "emit" => "sequence",
                "children" =>
                    (crate::value!([
                        (crate::value!([
                            "emit" => "identifier",
                            "value" => "_"
                        ])),
                        (crate::value!([
                            "emit" => "op_mod_kle",
                            "children" =>
                                (crate::value!([
                                    "emit" => "identifier",
                                    "value" => "Tokay"
                                ]))
                        ])),
                        (crate::value!([
                            "emit" => "value_instance",
                            "children" =>
                                (crate::value!([
                                    (crate::value!([
                                        "emit" => "identifier",
                                        "value" => "Expect"
                                    ])),
                                    (crate::value!([
                                        "emit" => "instarg",
                                        "children" =>
                                            (crate::value!([
                                                "emit" => "identifier",
                                                "value" => "EOF"
                                            ]))
                                    ]))
                                ]))
                        ])),
                        (crate::value!([
                            "emit" => "call",
                            "children" =>
                                (crate::value!([
                                    (crate::value!([
                                        "emit" => "identifier",
                                        "value" => "ast"
                                    ])),
                                    (crate::value!([
                                        "emit" => "callarg",
                                        "children" =>
                                            (crate::value!([
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
