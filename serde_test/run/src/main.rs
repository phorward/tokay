use std::fs::File;
use std::io::Read;
use tokay;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program: tokay::vm::Program = {
        let mut file = File::open("../program.cbor")?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)?;

        // serde_json::from_slice(&buffer)?
        serde_cbor::from_slice(&buffer)?
    };

    println!("{:?}", program);
    println!("run...{:?}", program.run_from_str("1 + 2 * 3 + 4"));

    Ok(())
}
