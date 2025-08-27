//! Tokay main and REPL
#![cfg(feature = "cli")]
use clap::Parser;
use env_logger;
use rustyline;
use std::fs::{self, File};
#[cfg(feature = "cbor")]
use std::io::Write;
use std::io::{self, BufReader};
use tokay;
use tokay::vm::Thread;
use tokay::{Compiler, Object, Reader, RefValue, Value};

fn print_version() {
    println!("Tokay {}", env!("CARGO_PKG_VERSION"));
}

#[derive(clap::Parser)]
#[clap(
    name = "Tokay",
    author,
    version,
    about,
    help_template = r#"{bin} {version}
Programming language, designed for ad-hoc parsing and syntax-based development.

Copyright © 2025 by Jan Max Meyer, Phorward Software Technologies.
{bin} is free software released under the MIT license.

{all-args}

PROGRAM and INPUT are directly used as input strings in case no file with the
given name exists. Use '-f' to disable this behavior. Specify '-' to use stdin
as input file.

When a PROGRAM is not specified, {bin} turns into an interactive REPL.

Visit https://tokay.dev/ for help and further information."#
)]
struct Opts {
    // vvv--- positional ---vvv
    /// Program to compile and run.
    #[clap(value_parser)]
    program: Option<String>,

    /// Input for program to operate on.
    #[clap(value_parser, last = true)]
    input: Vec<String>,

    #[cfg(feature = "cbor")]
    /// Compile PROGRAM into CBOR binary file OUTPUT.
    #[clap(short, long, action, value_name = "OUTPUT")]
    compile: Option<String>,

    // vvv--- named short/long options (sorted by alphabet) ---vvv
    /// Echo result of executed main parselet
    #[clap(short, long, action)]
    echo: bool,

    /// Accept only files as parameters, no string fallbacks.
    #[clap(short, long, action)]
    files: bool,

    /// Show license agreement and exit.
    #[clap(short, long, action)]
    license: bool,

    /// Run Tokay without verbose outputs
    #[clap(short, long, action)]
    quiet: bool,

    /// Start the given PROGRAM in its own REPL.
    #[clap(short, long, action)]
    repl: bool,

    /// Set variables in the format var=value.
    ///
    /// The value will be tried to be converted into a Tokay value,
    /// and fallsback to str.
    #[clap(short, long, num_args(0..))]
    var: Vec<String>,
}

/// Create Readers from provided filesnames
fn get_readers(opts: &Opts) -> Vec<Reader> {
    // Try getting files to run on program or repl
    let mut readers: Vec<Reader> = Vec::new();

    for filename in &opts.input {
        if filename == "-" && !opts.files {
            readers.push(Reader::new(
                Some(filename.to_string()),
                Box::new(BufReader::new(io::stdin())),
            ));
        } else if let Ok(file) = File::open(filename) {
            readers.push(Reader::new(
                Some(filename.to_string()),
                Box::new(BufReader::new(file)),
            ));
        } else if !opts.files {
            readers.push(Reader::new(
                None,
                Box::new(io::Cursor::new(filename.clone())),
            ));
        } else {
            eprintln!("Can't open INPUT file '{}'", filename);
            std::process::exit(1);
        }
    }

    readers
}

/// Read-Eval-Print-Loop (REPL) for Tokay
fn repl(compiler: &mut Compiler, opts: &Opts) -> rustyline::Result<()> {
    let mut globals: Vec<RefValue> = Vec::new();

    // todo: Implement a completer?
    let mut readline = rustyline::DefaultEditor::new()?;

    // todo: Implement a history in $HOME for production?
    if cfg!(debug_assertions) && std::env::var("TOKAY_HISTORY_LOAD").map_or(true, |var| var == "1")
    {
        readline.load_history(".tokayhist").ok();
    }

    loop {
        let code = match readline.readline(">>> ") {
            Err(rustyline::error::ReadlineError::Interrupted)
            | Err(rustyline::error::ReadlineError::Eof) => break,

            Err(err) => {
                println!("Error {:?}", err);
                break;
            }

            Ok(code) => code,
        };

        // Stop when program is empty.
        if code.trim().is_empty() {
            continue;
        }

        //println!("code = {:?}", code);

        readline.add_history_entry(code.as_str())?;

        match code.as_str() {
            /*
            "#debug" => {
                compiler.debug = 1;
                println!("<<< Debug switched on")
            }
            "#nodebug" => {
                compiler.debug = 0;
                println!("<<< Debug switched off")
            }
            */
            _ => match compiler.compile(Reader::new(None, Box::new(io::Cursor::new(code)))) {
                Ok(None) => {}
                Ok(Some(program)) => {
                    let mut readers = get_readers(&opts);

                    // In case no stream was specified and REPL fires up, read on an empty string.
                    if readers.len() == 0 {
                        readers.push(Reader::new(None, Box::new(io::Cursor::new(String::new()))));
                    }

                    let mut thread = Thread::new(&program, readers.iter_mut().collect());
                    thread.debug = compiler.debug;
                    thread.globals = globals;

                    match thread.run() {
                        Ok(Some(value)) => println!("{}", value.repr()),
                        Err(error) => eprintln!("{}", error),
                        _ => {}
                    }

                    globals = thread.globals;
                }
                Err(errors) => {
                    for error in errors {
                        eprintln!("{}", error);
                    }
                }
            },
        }
    }

    if cfg!(debug_assertions) && std::env::var("TOKAY_HISTORY_SAVE").map_or(true, |var| var == "1")
    {
        readline
            .save_history(".tokayhist")
            .expect("Cannot save REPL history");
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::init();

    // Handle command-line arguments from Opts.
    let opts = Opts::parse();

    // Show license and exit?
    if opts.license {
        print_version();
        print!("{}", include_str!("../LICENSE"));
        std::process::exit(0);
    }

    // Read program, either from stdin, file or direct string.
    let mut program: Option<Reader> = None;

    if let Some(prog) = &opts.program {
        if prog == "-" && !opts.files {
            program = Some(Reader::new(
                Some(prog.to_string()),
                Box::new(BufReader::new(io::stdin())),
            ));
        } else {
            if let Some(meta) = fs::metadata(prog).ok() {
                if !meta.is_dir() {
                    if let Ok(file) = File::open(prog) {
                        program = Some(Reader::new(
                            Some(prog.to_string()),
                            Box::new(BufReader::new(file)),
                        ));
                    }
                }
            }

            if program.is_none() {
                if !opts.files {
                    program = Some(Reader::new(None, Box::new(io::Cursor::new(prog.clone()))))
                } else {
                    eprintln!("Can't open PROGRAM file '{}'", prog);
                    std::process::exit(1);
                }
            }
        }
    }

    // Create a new Tokay compiler
    let mut compiler = Compiler::new();

    /*
    compiler.constant(
        "printH",
        RefValue::from(tokay::value::DynBuiltin {
            name: "printH",
            func: Box::new(|context, args, nargs| {
                println!("Hello World");
                Value::Void.into()
            }),
        }),
    );
    */

    // Load variables from command-line into the compiler
    for var in &opts.var {
        let var: Vec<_> = var.splitn(2, "=").collect();

        if var.len() == 2 {
            /*
            FIXME: This is absolutely inperformant due to the current implementation.
            It builds a new Tokay parser for every value being provided here!

                1. the Tokay parser instance must be globally unique
                2. value/value.tok is a subset of compiler/tokay.tok - modularity needed.
            */
            compiler.global(
                var[0],
                tokay::eval(include_str!("value/value.tok"), var[1], None).unwrap(),
            );
        } else {
            compiler.global(var[0], RefValue::from(Value::Void));
        }
    }

    // When a program is provided, compile and run it
    if let Some(program) = program {
        match compiler.compile(program) {
            Ok(None) => {}
            Ok(Some(program)) => {
                #[cfg(feature = "cbor")]
                if let Some(filename) = &opts.compile {
                    let cbor_program = serde_cbor::to_vec(&program)?;
                    //let json_program = serde_json::to_string(&program).unwrap();

                    let mut file = File::create(filename)?;
                    file.write_all(&cbor_program)?;
                    file.flush()?;

                    return Ok(());
                }

                let mut readers = get_readers(&opts);

                // In case no stream but a program is specified, use stdin as input stream.
                if readers.len() == 0 {
                    // Run program in its own REPL?
                    if opts.repl {
                        let mut readline = rustyline::DefaultEditor::new()?;
                        readline.load_history(".tokayrepl").ok();

                        loop {
                            let code = match readline.readline("<<< ") {
                                Err(rustyline::error::ReadlineError::Interrupted)
                                | Err(rustyline::error::ReadlineError::Eof) => break,
                                Err(err) => {
                                    eprintln!("Error {:?}", err);
                                    break;
                                }

                                Ok(code) => code,
                            };

                            // Stop when program is empty.
                            if code.trim().is_empty() {
                                continue;
                            }

                            readline.add_history_entry(code.as_str())?;

                            match program.run_from_reader(Reader::new(
                                None,
                                Box::new(std::io::Cursor::new(code)),
                            )) {
                                Ok(None) => {}
                                Ok(Some(value)) => println!("{}", value.to_string()),
                                Err(error) => eprintln!("{}", error),
                            }
                        }

                        readline.save_history(".tokayrepl").unwrap();
                        std::process::exit(0);
                    }

                    readers.push(
                        // When program's main is consuming, read from stdin
                        if program.main().is_consuming() {
                            Reader::new(
                                Some("-".to_string()),
                                Box::new(BufReader::new(io::stdin())),
                            )
                        }
                        // otherwise just work on an empty input
                        else {
                            Reader::new(None, Box::new(io::Cursor::new("")))
                        },
                    );
                }

                if opts.repl {
                    eprintln!("REPL-mode not allowed in combination with provided INPUT");
                    std::process::exit(1);
                }

                let mut thread = Thread::new(&program, readers.iter_mut().collect());

                match thread.run() {
                    Ok(None) => {
                        if opts.echo && readers.len() > 1 {
                            print!("\n")
                        }
                    }
                    Ok(Some(value)) => {
                        if opts.echo {
                            println!("{}", value.to_string())
                        }
                    }
                    Err(error) => eprintln!("{}", error),
                }
            }
            Err(errors) => {
                for error in errors {
                    eprintln!("{}", error);
                }
            }
        }
    } else {
        if opts.repl {
            eprintln!("No PROGRAM was specified, can't turn into a REPL for PROGRAM");
            std::process::exit(1);
        }

        #[cfg(feature = "cbor")]
        if opts.compile.is_some() {
            eprintln!("No PROGRAM was specified, can't use `--compile` with a REPL.");
            std::process::exit(1);
        }

        if !opts.quiet {
            print_version();
        }

        repl(&mut compiler, &opts)?
    }

    Ok(())
}
