use std::{
    io::{stdin, Read, Write},
    path::PathBuf,
    process::{Command, Stdio},
};

use rcc_asm_arm::ArmBackend;
use rcc_asm_x86::X86Backend;
use rcc_bytecode::Bytecode;
use rcc_lexer::Lexer;
use rcc_parser::Parser;

const OPTIMIZE: bool = false;

fn main() {
    let arg = std::env::args().nth(1);
    let (input, path) = match arg.as_deref() {
        Some("-") => {
            let mut input = String::new();
            stdin().read_to_string(&mut input).unwrap();

            (input, None)
        }
        Some(path) => (
            std::fs::read_to_string(path)
                .unwrap_or_else(|_| panic!("failed to read input file! {path}")),
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
    let mut ast = parser.parse();

    println!("===== AST =====");
    println!("{ast:#?}");
    println!("===============");

    if OPTIMIZE {
        rcc_opt::optimize_ast(&mut ast);
        println!("===== OPTIMIZED =====");
        println!("{ast:#?}");
        println!("=====================");
    }

    let bytecode = Bytecode::from_ast(&ast);
    println!("===== BYTECODE ====");
    for function in &bytecode {
        println!("== BYTECODE FOR `{}` ==", function.fn_name());
        for instruction in function.instructions() {
            println!("{instruction:?}");
        }
        println!("======");
    }

    println!("===================");

    gen_asm(&bytecode, true)
}

fn compile_program(input: &str) -> String {
    let lexer = Lexer::new(input);

    let parser = Parser::new(lexer);
    let mut ast = parser.parse();

    if OPTIMIZE {
        rcc_opt::optimize_ast(&mut ast);
    }

    let bytecode = Bytecode::from_ast(&ast);

    gen_asm(&bytecode, false)
}

fn gen_asm(bytecodes: &[Bytecode], verbose: bool) -> String {
    if verbose {
        print!("====== ");
    }

    let arch = std::env::consts::ARCH;
    let asm = match arch {
        "aarch64" => {
            if verbose {
                print!("ARM ASSEMBLY");
            }

            rcc_backend_asm::generate_assembly(&bytecodes, &mut ArmBackend)
        }
        "x86_64" => {
            if verbose {
                print!("x86 ASSEMBLY");
            }

            rcc_backend_asm::generate_assembly(&bytecodes, &mut X86Backend)
        }
        _ => panic!("unsupported architecture"),
    };

    if verbose {
        println!(" ======");
        print!("{asm}");
        println!("===========");
    }

    asm
}
