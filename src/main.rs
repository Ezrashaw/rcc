// TODO: make our crate just a library without binaries. We could have an example binary (or something of the sort) which would allow the CLI behaviour.

use std::{env, fs, path::Path};

use rcc::compile;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file = fs::read_to_string(&args[1]).unwrap();

    let output_path = Path::new(&args[1]).with_extension("");

    let result = compile(
        file,
        if cfg!(debug_assertions) {
            Path::new("output")
        } else {
            &output_path
        },
        args[1].clone(),
    );

    if let Err(err) = result {
        println!("Compilation FAILED:\n{}", err);
    } else {
        println!("Compiled successfully!");
    }
}
