# Changelog

This file contains a short note on relevant changes to the Tokay programming language.

## [main]

Current main branch.

### [v0.6.12] Removal of any `unsafe`-stuff again, bugfix release

- Fixed a bug related to `cli`-feature-dependent compilation
- Removed `-d` command-line flags and updated docs

### [v0.6.11] `serde`-support and `_tokay.cbor`-based parser

- feat: `serde`-support
  - `Value` and `RefValue` (limited to atomics: void, null, bool, int, float, str, list and dict)
  - `Parselet`, `Op` and `Program` to serialize and deserialize a VM program
- feat: Use `_tokay.cbor` parser as fast, built-in, binary-encoded Tokay VM program (compiled `tokay.tok`)

### [v0.6.10] `DynBuiltin`

- feat: DynBuiltins to overwrite builtin-function constants (#158)

### [v0.6.9] `tokay::eval` & variable setting

- feat: `UnaryOp` and `BinaryOp` types (#156)
- feat: Set global variables from outside (`-v`-parameter) (#154)
- feat: `upsert`-feature for `dict` and `list` (#155)
- feat: Accept `nargs` for `Builtin::call()` and `RefValue::call_method()`

### [v0.6.8] WASM Update

- `cli`-feature to make `wasm` target possible (#153)
- feat: Replace `Token::BuiltinChar` by `Token::Char`
- fix: Rvalue traversals with calls
- fix: Use charclass==0.3.0

### [v0.6.7] Feature update

Released on Apr 29, 2025

- chore: Move `build`-toolchain into the root
- chore: Update to Rust Edition 2024
- feat: `list_index()`-function
- feat: `min()`, `max()`, `sum()` and `avg()` (#145)
- feat: Add builtin `str_find`
- feat: Adding `list_clone()` function
- feat: Implement explicit `list_extend()`
- feat: Improve `for...in` syntax
- feat: Improved `Token` prelude to match `AsciiPunctuation` first
- feat: Improving `dict` iteration & access
- feat: Resize `list` when setting an index out of bounds
- fix: #127 self-reference endless recursion
- fix: `InlineList` doesn't accept multi-line list definitions
- fix: `Keyword<P>` doesn't recognize '_'
- fix: `list_iadd` with void (#146)
- fix: `loop`-syntax should allow for "loop\n{..."
- fix: `Op::MakeDict()` creates reversed key order
- fix: Complex chained comparison compile into broken code
- fix: Disallow variable shadowing by constants
- fix: Enforce use of `*`-deref for assignments
- fix: Generic `Self`/`self` (#142)
- fix: Improve `float` repr & str representation
- fix: Improve `inplace/(post|pre)` syntax
- fix: object `iadd` call with standalone imutable
- fix+doc: docs for `dict()`, removed `dict_push()`
- perf: Improving VM `Op::MakeDict` and `Op::MakeList`
- refactor: `ImlProgram` compilation and finalization (#147)
- test: Adding test case for dict_clone()
- test: Split `dict.tok` testcases into separate files

### [v0.6.6] Feature update

- Improved and revised `list` and `dict` syntax
  - `,` is now some kind of operator, denoting a list
- Implementation and use of `Keyword<P>`
- `Empty` and `Void` clarification
- Improved logging and debug
- Compiler internals
  - New `Scope` struct
  - More refactoring on `ImlValue`

### [v0.6.5] First implementation of generic parselets

- Syntax changes
- Handle parselet instances
- Handle generic values in intermediate structures
- System-defined parselets in prelude
  - `Repeat<P, min: 1, max: void>`
    - Implement `Pos<P>`, `Opt<P>`, `Kle<P>`
    - Compile `P+` into `Pos<P>`, `P?` into `Opt<P>`, `P*` into `Kle<P>`
  - `List<P, Sep: (',' _), empty: true>`
  - `Peek<P>`, replaced `peek P` by `Peek<P>`
  - `Not<P>`, replaced `not P` by `Not<P>`
  - `Expect<P> msg=void`, replaced `expect P` by `Expect<P>`

### [v0.6.4] Feature update
  - Main parselet operates on multiple inputs (Readers) now
  - Restructuring parts of VM, `Runtime` renamed into `Thread`
  - Several test cases restructured
  - Bugfix for `str_get_item`
  - `Ord` on Value and RefValue, builtin `list_sort()`
  - Implementing `str_split(s, sep=void, n=void)`
  - Fix `begin` and `end` feature of parselets (#109)

### [v0.6.3] Test release

Test release by mistake, see #106 for details.

### [v0.6.2] Internal revision

Internal revision and clarification on compiler's intermediate structures `ImlOp`, `ImlValue` and `ImlProgram` (part of #105)

### [v0.6.1] Feature update

- Comparison chains within expressions (#103)
- Iterators and `for...in`-syntax (#101)
- `Chars<...>` in addition to `Char<...>` syntax

## [v0.6] Milestone

Released on Jan 13, 2023

- General
  - Use of [num-parse](https://crates.io/crates/num-parse) for `Int` and internal string-to-int conversion ("parseInt()"-like behavior) (#65)
  - Updated `clap` command-line parser to v3 (#61)
  - Improving internal `testcase` function and moving all prior `#[test]`-functions into separate Tokay testcases (#86)
- Syntax
  - Operator `//` for integer division implemented (#92)
  - Operator `%` for modulo operation implemented
  - Area syntax `@(...)` for in-place reader extend (#78)
  - Character-class syntax changed from `[a-z]` into `Char<a-z>`, `.` and `Any` substituted by `Char` (#98)
  - Improved syntax for inline blocks and sequences (`|`-operator)
  - Improved list syntax
    - `()` the empty list
    - `(1,)` list with one item (explicit comma required)
  - Implemented `x[...]` item access syntax for rvalue and lvalue (#80)
  - Preliminaries for generic parselets (#10)
  - New built-in signatures (#84)
- Compiler
  - Parser
    - `parser.rs` is now generated from `tokay.tok`; syntax-changes are only done in `tokay.tok` now! (#93)
    - Removed `macros.rs` and macro-based bootstrap parser entirely
  - Internal revision
    - Removed structs `Usage` and `ImlResult`
    - Integrating all `impl Compileable`s into `ImlOp`
    - Code construction now happens in `ImlOp` as well
    - Added required changes to
      - determine whether a part of code consumes input
      - preliminaries to generic parselets (yet unfinished)
  - `prelude.tok` provides some default-parselets defined in Tokay itself
    - `Number` matches either `Float` or `Int`
    - `Token` matches arbitrary tokens
- Virtual Machine
  - Internal refactoring of the essential `Context::collect()`-function (#67)
  - `Frame` is now managed by `Context`
- Values
  - Turned Value::Int to crate [num-bigint](https://crates.io/crates/num-bigint), replaced Value::Addr by the same type (#55)
  - Definition of mutable objects; Imutable objects push a clone of, mutable objects push a ref on the object
  - `dict` now allow for any non-mutable value as key (#96)
- Builtins
  - Added `dict.clone()`, `dict_push()`, `dict.pop()`, `dict.get_item()`, `dict.set_item()`, `list.get_item()`, `list.set_item()`, `str.get_item()`
  - Renamed `dict_update()` into `dict_merge()`
- Examples
  - The self-hosted Tokay parser in `examples/tokay.tok` was now moved into `src/compiler/tokay.tok` and is used to generate `src/compiler/parser.rs` (#93)
  - The JSON parser example in `examples/json.tok` was improved to latest developments

## [v0.5] Milestone

Released on May 17, 2022

- v0.5.1: Improved `build.rs` to write builtin registry only when it changed.
- General
  - Improved Tokay parser newline behavior to support Windows and classic Mac line ending as well
  - Moved tests from `test.rs` into their particular locations, so tests are written next to their implementation
  - Read from stdin when no input stream but a program that is consuming was specified
  - Moved `ccl.rs` into separate [`charclass` crate](https://crates.io/crates/charclass)
  - Equipped Reader struct with better tools for scanning (`Reader::take()`, `Reader::span()`)
- Values
  - New `Object` trait
  - Splitting `enum Value` into separate objects using `Box<dyn Object>`
  - `unary_op()` and `binary_op()` for RefValue, with fast-lanes for some operations
  - Replacement of separate binary- and unary-VM instructions into `Op::BinaryOp` and `Op::UnaryOp`
  - Implemented all primary operations by builtin methods
- Builtins
  - Entire redesign of builtins using proc-macro
  - Builtin function registry generated by a build-script, substituting `inventory`/`linkme`-crates
  - Builtin functions
    - `repr()` to get string with Tokay object representation
    - `type()` to get string of Tokay value type
    - bool methods: `bool()`
    - int methods: `int()`
    - float methods: `float()`, `float.ceil()`, `float.fract()`, `float.trunc()`
    - addr methods: `addr()`
    - dict methods: `dict()`, `dict.len()`, `dict.update()`
    - list methods: `list()`, `list.add()`, `list.iadd()`, `list.len()`, `list.push()`, `list.pop()`
    - str methods: `str()`, `str.add()`, `str.byteslen()` `str.endswith()`, `str.mul()`, `str.join()`, `str.len()`, `str.lower()`, `str.replace()`, `str.startswith()`, `str.substr()`, `str.upper()`
  - Builtin tokens
    - `Float` allowing for parsing floating point numbers into float values
    - `Ident` renamed (from `Identifier`)
    - `Int` renamed (from `Integer`) and accepting parameters like a radix base
    - `Word` skipping words not in the wanted word size, but not rejecting
- Examples
  - Self-hosted Tokay parser in `examples/tokay.tok`
  - Simple JSON parser in `examples/json.tok`


## [v0.4] Milestone

Released on Nov 15, 2021

- Implementation of a separated virtual machine (VM) to reduce overall stack usage (#8)
- Removal of the recursive interpreter due to VM replacement
- Cleaned-up and modularization of an encapsulated compiler
- Turned compile-time building blocks into intermediate language (iml)
- Built-in character classes
- Started loop implementation
- Improved debug facilities


## [v0.3] Milestone

Released on Jul 7, 2021

- Cleaning and finalizing syntax
- Collections with list and dict expression
- Consumables and built-in tokens
- Implemented all remaining operators
- Inline increment/decrement
- R-value handling for subscripts and attributes, with calls to built-ins
- Compile-time expression evaluation
- Whitespace handling
- Escape sequences
- Command-line parameters


## [v0.2] Milestone

Released on Mar 21, 2021

- Parselets and built-ins with parameters
- Improved error reporting
- Interactive REPL


## [v0.1] Milestone

Released on Mar 4, 2021

- Character-classes
- Universal Reader trait
- Macros for grammar expression
- First draft of the Tokay grammar
- Implementing recursive interpreter
- Left-recursive parselets
- Sequence capturing and AST construction
- Distinction between constants and variables
- Symbol resolving and scopes


[main]: https://github.com/tokay-lang/tokay/compare/v0.6...main
[v0.6]: https://github.com/tokay-lang/tokay/compare/v0.5...v0.6
[v0.5]: https://github.com/tokay-lang/tokay/compare/v0.4...v0.5
[v0.4]: https://github.com/tokay-lang/tokay/compare/v0.3...v0.4
[v0.3]: https://github.com/tokay-lang/tokay/compare/v0.2...v0.3
[v0.2]: https://github.com/tokay-lang/tokay/compare/v0.1...v0.2
[v0.1]: https://github.com/tokay-lang/tokay/compare/2d74215f4842d295371112a630d15ab03442cd1e...v0.1
