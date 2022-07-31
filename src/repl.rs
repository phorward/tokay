//! Tokay repeat-eval-print-loop

use std::cell::RefCell;
use std::fs::File;
use std::io::{self, BufReader, Seek};

use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::compiler::Compiler;
use crate::reader::Reader;
use crate::value::{Object, RefValue};
use crate::vm::Runtime;

// Helper enum to allow for different input types
#[derive(Debug)]
pub enum Stream {
    String(String),
    File(File),
    Stdin,
}

impl Stream {
    pub fn get_reader(&mut self) -> Reader {
        match self {
            Stream::String(string) => Reader::new(Box::new(io::Cursor::new(string.clone()))),
            Stream::File(file) => {
                let mut file = file.try_clone().expect("File cannot be cloned?");
                file.seek(std::io::SeekFrom::Start(0))
                    .expect("Unable to seek to file's starting position");
                Reader::new(Box::new(BufReader::new(file)))
            }
            Stream::Stdin => Reader::new(Box::new(BufReader::new(io::stdin()))),
        }
    }
}

// A first simple REPL for Tokay
pub fn repl(streams: Vec<(&str, RefCell<Stream>)>) {
    let mut globals: Vec<RefValue> = Vec::new();

    let mut compiler = Compiler::new(true);

    // todo: Implement a completer?
    let mut readline = Editor::<()>::new();

    // todo: Implement a history in $HOME for production?
    if cfg!(debug_assertions) {
        readline.load_history(".tokayhist").ok();
    }

    loop {
        let code = match readline.readline(">>> ") {
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,

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

        readline.add_history_entry(code.as_str());

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
            _ => {
                if compiler
                    .compile(Reader::new(Box::new(io::Cursor::new(code))))
                    .is_ok()
                {
                    todo!();
                    /*
                    if let Ok(program) = compiler.finalize() {
                        for (name, stream) in &streams {
                            let mut reader = stream.borrow_mut().get_reader();
                            let mut runtime = Runtime::new(&program, &mut reader);
                            runtime.debug = compiler.debug;
                            runtime.load_stack(globals);

                            let ret = program.run(&mut runtime);

                            if streams.len() > 1 {
                                print!("{}: ", name);
                            }

                            match ret {
                                Ok(None) => {
                                    if streams.len() > 1 {
                                        print!("\n")
                                    }
                                }
                                Ok(Some(value)) => println!("{}", value.to_string()),
                                Err(error) => println!("{}", error),
                            }

                            globals = runtime.save_stack();
                        }
                    }
                    */
                }
            }
        }
    }

    if cfg!(debug_assertions) {
        readline.save_history(".tokayhist").unwrap();
    }
}
