//! Utility functions
use crate::value::*;
use std::fs::File;
use std::io::{Read, Write}; // BufRead, BufReader,
use std::process::{Command, Stdio};

/// Checks if an identifier defines a Tokay consumable.
pub(crate) fn identifier_is_consumable(ident: &str) -> bool {
    let ch = ident.chars().next().unwrap();
    ch.is_uppercase() || ch == '_'
}

/** Test-case evaluator.

A testcase is made of the following sections:
```tokay
print("Hello " + Int)   # code
#---
#23                     # input (optional)
#---
#Hello 23               # expected output
```

This is an entire testcase to describe a program, the input fed to it (which is optional)
and the expected output. The advantage of this specification is, that the testcase can directly
be executed with Tokay to reproduce its expected result, as the input and output sections are
specified as comments.

This function currently requires that a tokay debug executable is compiled before test cases are run.

The provided code can either be a testcase or a path to a filename that contains the testcase.

There's also a special REPL test mode: If the first line in the testcase contains `#testmode:repl`,
the code is fed to the Tokay REPL, and expected output is tested against each line specified. In this
mode, it is important to specify multi-line definitions with the alternative `;` delimiter, otherwise
a syntax error will occur (likewise in the normal REPL).
*/
#[allow(dead_code)]
pub(crate) fn testcase(code: &str) {
    //println!("---");

    // Try to open code as file
    let (filename, code) = match File::open(code) {
        // The file is open (no error).
        Ok(mut file) => {
            let mut content = String::new();

            // Read all the file content into a variable (ignoring the result of the operation).
            file.read_to_string(&mut content).unwrap();

            // The file is automatically closed when it goes out of scope.
            (code, content)
        }
        // Error handling.
        Err(_) => ("--", code.to_owned()),
    };

    //println!("code = {:?}", code);
    let repl_mode = code.starts_with("#testmode:repl\n");

    if let Some((code, data)) = code.split_once("#---\n") {
        let mut lines = code.matches("\n").count() + 1;

        //let program = env::args().next().unwrap(); // Doens't work with cargo test
        let program = if cfg!(test) {
            "target/debug/tokay".to_string()
        } else {
            std::env::current_exe()
                .unwrap()
                .to_str()
                .unwrap()
                .to_owned()
        };

        let mut cmd = Command::new(&program);

        //println!("code = {:?}", code);

        if repl_mode {
            cmd.arg("-q");
            cmd.env("TOKAY_HISTORY_SAVE", "0");
            cmd.stdin(Stdio::piped());
        } else {
            cmd.arg("-e");
            cmd.arg(code);
        }

        let mut result = data;
        let tmp;

        // In case there is another separator, split into input and output
        if let Some((input, output)) = data.split_once("#---\n") {
            //println!("input = {:?}", input);
            //println!("output = {:?}", output);

            let input: Vec<&str> = input
                .split("\n")
                .map(|line| line.trim_start())
                .filter(|line| line.starts_with("#"))
                .map(|line| &line[1..])
                .collect();

            tmp = input.join("\n");
            lines += tmp.matches("\n").count() + 1 + 1;

            cmd.arg("--").arg(&tmp);
            result = output;
        }

        // Spawn tokay interpreter process
        let mut process = cmd
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect(&format!(
                "{} failed to run using {}; You need to run `cargo build` first!",
                filename, &program
            ));

        if repl_mode {
            process
                .stdin
                .as_mut()
                .unwrap()
                .write(code.as_bytes())
                .unwrap();
        }

        let output = process.wait_with_output().unwrap();

        let out = String::from_utf8(output.stdout).expect("stdout is not UTF-8");
        let err = String::from_utf8(output.stderr).expect("stderr is not UTF-8");

        //println!("out = {:?}", out);
        //println!("err = {:?}", err);

        // Convert ouput and err to Vec<String>, without the last (empty) line if available
        let mut out: Vec<String> = if out.is_empty() {
            Vec::new()
        } else {
            out.trim_end()
                .split("\n")
                .map(|line| line.to_string())
                .collect()
        };

        let mut err: Vec<String> = if err.is_empty() {
            Vec::new()
        } else {
            err.trim_end()
                .split("\n")
                .map(|line| line.to_string())
                .collect()
        };

        //println!("out = {:?}", out);
        //println!("err = {:?}", err);

        //let mut stdout = BufReader::new(process.stdout.as_mut().unwrap());
        //let mut stderr = BufReader::new(process.stderr.as_mut().unwrap());

        for (row, line) in result.split("\n").into_iter().enumerate() {
            let line = line.trim_start();

            if line.is_empty() {
                continue;
            }

            assert!(
                line.starts_with("#"),
                "{}:{} Result must start with a #-comment, got {:?}",
                filename,
                lines + row + 1,
                line
            );

            if line.starts_with("#ERR:") {
                /*
                let mut err = String::new();
                stderr.read_line(&mut err).unwrap();
                if err.ends_with('\n') {
                    err.pop();
                }
                */
                let exp = &line[5..];
                let line = err
                    .get(0)
                    .expect("Expecting stderr but nothing was emitted");

                if exp != "SKIP" {
                    assert_eq!(
                        line,
                        exp,
                        "{}:{} stderr expects {:?} but got {:?}",
                        filename,
                        lines + row + 1,
                        exp,
                        line
                    );
                }

                err.remove(0);
            } else {
                /*
                let mut out = String::new();
                stdout.read_line(&mut out).unwrap();
                if out.ends_with('\n') {
                    out.pop();
                }
                */
                let exp = &line[1..];
                let line = out
                    .get(0)
                    .expect("Expecting stdout but nothing was emitted");

                if exp != "SKIP" {
                    assert_eq!(
                        line,
                        exp,
                        "{}:{} stdout expects {:?} but got {:?}",
                        filename,
                        lines + row + 1,
                        exp,
                        line
                    );
                }

                out.remove(0);
            }
        }

        assert!(
            out.is_empty(),
            "{} some output not consumed: {:#?}",
            filename,
            out
        );
        assert!(
            err.is_empty(),
            "{} Some errors not consumed: {:#?}",
            filename,
            err
        );

        //process.wait().unwrap();
    } else {
        panic!(
            "{} invalid testcase, at least one '#---' delimiter required",
            filename
        )
    }
}
