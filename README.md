<div align="center">
    <img src="https://raw.githubusercontent.com/tokay-lang/tokay-artwork/main/banner-with-darkmode-styling.svg" alt="The Tokay Logo" title="Tokay Programming Language" style="width: 80%"><br>
    <a href="https://github.com/tokay-lang/tokay/actions/workflows/main.yml">
        <img src="https://github.com/tokay-lang/tokay/actions/workflows/main.yml/badge.svg">
    </a>
    <a href="https://crates.io/crates/tokay">
        <img src="https://img.shields.io/crates/v/tokay" alt="Version badge for crates.io" title="crates.io/tokay">
    </a>
    <a href="https://docs.rs/tokay/latest/tokay">
        <img src="https://img.shields.io/docsrs/tokay" alt="Badge of docs.rs" title="docs.rs/tokay">
    </a>
    <a href="https://tokay.dev/">
        <img src="https://img.shields.io/website?down_color=red&down_message=offline&up_color=green&up_message=online&url=https%3A%2F%2Ftokay.dev%2F" alt="Indicator for tokay.dev Website availability" title="tokay.dev">
    </a>
    <a href="https://opensource.org/licenses/MIT">
        <img src="https://img.shields.io/badge/License-MIT-green.svg" alt="MIT License badge" title="MIT License">
    </a>
</div>

# Tokay

Tokay is a programming language, designed for ad-hoc parsing and syntax-driven development.


> [!IMPORTANT]
> Tokay is under development and not considered for production use yet.
> Be part of Tokay's ongoing development, and [contribute](CONTRIBUTING.md)!
>

## About

Tokay is a programming language that can be used to quickly implement solutions for text processing problems. This can involve either simple data extractions, but also the parsing of syntactical structures or parts thereof and the conversion of information into structured parse trees or abstract syntax trees for additional processing.

Therefore, Tokay is not only a practical tool for simple one-liners like matchers or recognizers, but also a language that can be used to implement code analyzers, refactoring tools, interpreters, compilers or transpilers. Actually [Tokay's own language parser](src/compiler/tokay.tok) is implemented in Tokay itself.

Tokay is inspired by [awk](https://en.wikipedia.org/wiki/AWK), has syntactic and semantic flavours from [Python](https://www.python.org/) and [Rust](https://www.rust-lang.org/), but also follows its own philosophy, ideas and design principles. Thus, Tokay isn't directly compareable to other languages or projects, and stands on its own. It's an entirely new programming language in its own niche.

Tokay is still a very young project and gains much potential. Currently, development is going towards generic parselets (a speciality of Tokay) and a [WebAssembly-based development environment](https://github.com/tokay-lang/tokay-wasm).

[Volunteers are welcome!](CONTRIBUTING.md)

## Highlights

- Interpreted, procedural and imperative scripting language
- Concise and easy to learn syntax and object system
- Stream-based input processing
- Automatic parse tree construction and synthesis
- Left-recursive parsing structures ("parselets") supported
- Implements a memoizing packrat parsing algorithm internally
- Robust and fast, as it is written entirely in safe [Rust](https://rust-lang.org)
- Enabling awk-style one-liners in combination with other tools
- Generic parselets and functions (*partly implemented)
- Import system to create modularized programs (*coming soon)
- Embedded interoperability with other programs (*coming soon)

## Installation

By using Rusts dependency manager and build-tool `cargo`, simply install Tokay with

```bash
$ cargo install tokay
```

For Arch Linux-based distros, there is also a [`tokay`](https://aur.archlinux.org/packages/tokay) and [`tokay-git`](https://aur.archlinux.org/packages/tokay-git) package in the [Arch Linux AUR](https://aur.archlinux.org/).

## Examples

Tokay's version of "Hello World" is quite obvious.

```tokay
print("Hello World")
```

> ```
> $ tokay 'print("Hello World")'
> Hello World
> ```

Tokay can also greet any wor(l)ds that are being fed to it. The next program prints "Hello Venus", "Hello Earth" or "Hello" followed by any other name previously parsed by the builtin `Word`-token. Any other input than a word is automatically omitted.

```tokay
print("Hello", Word)
```

> ```
> $ tokay 'print("Hello", Word)' -- "World 1337 Venus Mars 42 Max"
> Hello World
> Hello Venus
> Hello Mars
> Hello Max
> ```

A simple program for counting words and numbers and printing a total afterwards can be implemented like this:

```tokay
Word words += 1
Number numbers += 1
end print(words || 0, "words,", numbers || 0, "numbers")
```

> ```
> $ tokay 'Word words += 1; Number numbers += 1; end print(words || 0, "words,", numbers || 0, "numbers")' -- "this is just the 1st stage of 42.5 or .1 others"
> 9 words, 3 numbers
> ```

By design, Tokay constructs syntax trees from consumed information automatically.

The next program implements a parser and interpreter for simple mathematical expressions, like `1 + 2 + 3` or `7 * (8 + 2) / 5`. The result of each expression is printed afterwards.

Processing direct and indirect left-recursions without ending in infinite loops is one of Tokay's core features.

```tokay
_ : Char< \t>+            # redefine whitespace to just tab and space

Factor : @{
    Int _                 # built-in 64-bit signed integer token
    '(' _ Expr ')' _
}

Term : @{
    Term '*' _ Factor     $1 * $4
    Term '/' _ Factor     $1 / $4
    Factor
}

Expr : @{
    Expr '+' _ Term       $1 + $4
    Expr '-' _ Term       $1 - $4
    Term
}

Expr _ print("= " + $1)   # gives some neat result output
```

> ```
> $ tokay examples/expr_from_readme.tok
> 1 + 2 + 3
> = 6
> 7 * (8 + 2) / 5
> = 14
> 7*(3-9)
> = -42
> ...
> ```

Calculate the fibonacci numbers from parsed integers:

```tokay
fibonacci : @n {
    if n <= 1 n else fibonacci(n - 1) + fibonacci(n - 2)
}

Int print($1, "=>", fibonacci($1))
```

> ```
> $ tokay examples/fibonacci2.tok
> 0
> 0 => 0
> 1
> 1 => 1
> 2
> 2 => 1
> 3
> 3 => 2
> 4
> 4 => 3
> 5
> 5 => 5
> 6
> 6 => 8
> 7
> 7 => 13
> 8
> 8 => 21
> 9
> 9 => 34
> 10
> 10 => 55
> ```

## Documentation

The Tokay homepage [tokay.dev](https://tokay.dev) provides links to a quick start and documentation. The documentation source code is maintained in a [separate repository](https://github.com/tokay-lang/tokay-docs).

## Building

To build Tokay, use Rust's standard build tool `cargo`:

```bash
# for debugging
$ cargo run -- PARAMS... -- INPUTS...

# for debug build
$ cargo build

# for optimized release-build
$ cargo build --release
```

Additionally, parts of Tokay's source code are generated using a `Makefile`-based tooling, which is described below.

### Build features

Tokay can be built with the following features. Features marked with `(*)` are default features.

- `cbor` (*): Serialize and deserialize VM-programs into and from CBOR binaries (includes `serde`)
- `cli` (*): Command-line interface (main.rs)
- `serde`: General `serde` features
- `static_expression_evaluation` (*): Evaluates static expressions like `1 + 2 + 3` directly to static value 6 during compile-time to reduce amount of resulting operations
- `use_cbor_parser` (*): Compiles Tokay with integrated binary-encoded CBOR parser for faster compile times (includes `serde` and `cbor`)

### `make builtins` - update Tokay's builtin registry from the Rust sources

Generate the file `src/_builtins.rs` using `src/_builtins.tok`.

- `make builtins` updates the content of `src/_builtins.rs` using `src/_builtins.tok` from the Rust sources.
- `make show-builtins` dumps what would be generated by `make builtins`
- `make reset-builtins` resets `src/_builtins.rs` from git, if something went wrong.

### `make parser-cbor` / `make parser-ast` - update Tokay's parser from `tokay.tok`

Tokay uses a program written in itself (`src/compiler/tokay.tok`) to generate its own language parser (used by `src/compiler/parser.rs`) incrementally.

There are currently two parser implementations, the CBOR-based pre-compiled Tokay parser is used as the default.

- `make parser-cbor` updates the content of `src/compiler/_tokay.cbor` from `src/compiler/tokay.tok`.
- `make reset-parser-cbor` resets `src/compiler/_tokay.cbor` from git, if something went wrong.

The old AST-based parser that is compiled internally into a Tokay VM program is still be supported.<br>
In the end, both parsers implement exactly the same program.

- `make parser-ast` updates the content of `src/compiler/_tokay.rs` from `src/compiler/tokay.tok`.
- `make show-parser-ast` dumps what would be generated by `make parser-ast`
- `make reset-parser-ast` resets `src/compiler/_tokay.rs` from git, if something went wrong.

### `make prelude` - update Tokay's prelude from `prelude.tok`

Tokay needs the prelude to provide general language features and defaults.

- `make prelude` updates the content of `src/compiler/prelude.rs` from `src/prelude.tok`.
- `make show-prelude` dumps what would be generated by `make prelude`
- `make reset-prelude` resets `src/compiler/prelude.rs` from git, if something went wrong.

## Debugging

For debugging, there are two methods to use.

### Tracing using the `log`-crate

For Rust standard trace, use the [`env_logger` facilities](https://docs.rs/env_logger/latest/env_logger/). Full trace is only compiled into debug executables, the release version only provides warning level and upwards.

```
$ RUST_LOG=tokay=debug tokay
```

### Built-in AST and VM debugger using `TOKAY_DEBUG` and `TOKAY_PARSER_DEBUG`

Set `TOKAY_DEBUG` to a debug level between 1-6. Additionally, `TOKAY_INSPECT` can be set to one or a list of parselet name (-prefixes) which should be inspected in VM step-by-step trace (`TOKAY_DEBUG=6`).

| Level | Mode                              |
| ----- | --------------------------------- |
| 0     | No debug                          |
| 1     | Print constructed AST             |
| 2     | Print final intermediate program  |
| 3     | Print compiled VM program         |
| 4     | Print VM execution trace          |
| 5     | Print VM stack contents           |
| 6     | VM opcode debugger                |

View the parsed AST of a program in debug-level 1:

> ```
> `$ TOKAY_DEBUG=1 cargo run -q -- 'x = 42 print("Hello World " + x)'`
> main [start 1:1, end 1:33]
>  sequence [start 1:1, end 1:33]
>   assign_drop [start 1:1, end 1:8]
>    lvalue [start 1:1, end 1:3]
>     identifier [start 1:1, end 1:2] => "x"
>    value_integer [start 1:5, end 1:7] => 42
>   call [start 1:8, end 1:33]
>    identifier [start 1:8, end 1:13] => "print"
>    callarg [start 1:14, end 1:32]
>     op_binary_add [start 1:14, end 1:32]
>      value_string [start 1:14, end 1:28] => "Hello World "
>      identifier [start 1:31, end 1:32] => "x"
> Hello World 42
> ```

`TOKAY_PARSER_DEBUG` sets the specific debug level for the parser, which is implemented in Tokay itself and is part of the compiler. Only levels > 2 can be recognized here, as the AST of the parser is built into the code.

Here's the VM debugger in action running the simple "Hello World"-program:

> ```
> `$ TOKAY_INSPECT="__main__" cargo run -q -- 'print("Hello World")'`
> __main__      --- Code ---
> __main__       000 Offset(Offset { offset: 6, row: 1, col: 7 })
> __main__      >001 LoadStatic(1)
> __main__       002 Offset(Offset { offset: 0, row: 1, col: 1 })
> __main__       003 CallStaticArg((2, 1))
> __main__      --- Reader ---
> __main__       offset=Offset { offset: 0, row: 1, col: 1 }
> __main__       eof=false
> __main__      --- Globals ---
> __main__      --- Stack ---
> __main__      --- Frames ---
> __main__       000 capture: 0, reader: 0, fuse: None
>
> __main__      ip = 1 state = Ok(Push([59d29e639f88] "Hello World" (10)))
> __main__      --- Code ---
> __main__       000 Offset(Offset { offset: 6, row: 1, col: 7 })
> __main__       001 LoadStatic(1)
> __main__       002 Offset(Offset { offset: 0, row: 1, col: 1 })
> __main__      >003 CallStaticArg((2, 1))
> __main__      --- Reader ---
> __main__       offset=Offset { offset: 0, row: 1, col: 1 }
> __main__       eof=false
> __main__      --- Globals ---
> __main__      --- Stack ---
> __main__       000 [59d29e639f88] "Hello World" (10)
> __main__      --- Frames ---
> __main__       000 capture: 0, reader: 0, fuse: None
>
> Hello World
> __main__      ip = 3 state = Ok(Push([59d29e498fd8] void (10)))
> __main__      exit state = Ok(Push([59d29e498fd8] void (10)))
> ```

## Logo

The Tokay programming language is named after the [Tokay gecko (Gekko gecko)](https://en.wikipedia.org/wiki/Tokay_gecko) from Asia, shouting out "token" in the night.

The Tokay logo and icon was thankfully designed by [Timmytiefkuehl](https://github.com/timmytiefkuehl).<br>
Check out the [tokay-artwork](https://github.com/tokay-lang/tokay-artwork) repository for different versions of the logo as well.

## License

Copyright © 2025 by Jan Max Meyer, Phorward Software Technologies.

Tokay is free software under the MIT license.<br>
Please see the LICENSE file for details.
