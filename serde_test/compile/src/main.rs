use std::fs::File;
use std::io::Write;
use tokay;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut compiler = tokay::Compiler::new();

    let s = include_str!("../../../examples/expr_from_readme.tok");
    //let s = "42";
    let program = compiler.compile_from_str(s).unwrap();

    let cbor_program = serde_cbor::to_vec(&program)?;
    //let json_program = serde_json::to_string(&program).unwrap();

    let mut file = File::create("../program.cbor")?;
    file.write_all(&cbor_program)?;
    file.flush()?;

    println!("OK");

    Ok(())
}
