use std::{
    collections::vec_deque,
    io::{stdin, stdout, Read, Write},
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

use rcc_backend_x86::X86Backend;
use rcc_bytecode::Bytecode;
use rcc_lexer::Lexer;
use rcc_parser::{pretty_printer::PrettyPrinter, Parser};

fn main() {
    let arg = std::env::args().nth(1);
    let (input, path) = match arg.as_deref() {
        Some("-") => {
            let mut input = String::new();
            stdin().read_to_string(&mut input).unwrap();

            (input, None)
        }
        Some(path) => (
            std::fs::read_to_string(path).expect(&format!("failed to read input file! {path}")),
            Some(path),
        ),
        None => {
            panic!("expected input file, either a path or `-`")
        }
    };

    let verbose = std::env::args().nth(2).as_deref() == Some("--verbose");

    let asm = if verbose {
        compile_program_verbose(&input)
    } else {
        compile_program(&input)
    };

    let path = path.map_or("a.out".to_owned(), |p| {
        PathBuf::from(p)
            .with_extension("")
            .to_string_lossy()
            .to_string()
    });

    let mut gcc = Command::new("gcc")
        .args(["-o", &path, "-xassembler", "-"])
        .stdin(Stdio::piped())
        .spawn()
        .expect("failed to run `gcc`");

    gcc.stdin
        .as_mut()
        .unwrap()
        .write_all(asm.as_bytes())
        .expect("failed to pipe to `gcc`");

    gcc.wait().unwrap();
}

fn compile_program_verbose(input: &str) -> String {
    let lexer = Lexer::new(input);

    println!("=== TOKENS ===");
    for tok in lexer.clone() {
        println!("{:?}", tok.kind);
    }
    println!("==============");

    let parser = Parser::new(lexer);
    let ast = parser.parse();

    println!("===== AST =====");
    println!("{ast:#?}");
    println!("===============");

    println!("===== PRETTY =====");
    PrettyPrinter::new(&mut stdout(), &ast).print().unwrap();
    println!("==================");

    let bytecode = Bytecode::from_ast(&ast);
    println!("===== BYTECODE (for only function) ====");
    for instruction in &bytecode.function().1 {
        println!("{instruction:?}");
    }
    println!("===================");

    let x86 = X86Backend::new(bytecode);
    let x86 = x86.gen_x86();
    println!("===== x86 ASM =====");
    print!("{}", x86);
    println!("===================");

    x86
}

fn compile_program(input: &str) -> String {
    let lexer = Lexer::new(input);

    let parser = Parser::new(lexer);
    let ast = parser.parse();

    let bytecode = Bytecode::from_ast(&ast);

    let x86 = X86Backend::new(bytecode);
    let x86 = x86.gen_x86();

    x86
}
