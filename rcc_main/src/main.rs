use std::{
    io::{stdin, stdout, Read, Write},
    path::PathBuf,
    process::{Command, Stdio},
};

use rcc_backend_arm::ArmBackend;
use rcc_backend_llvm::LlvmBackend;
use rcc_backend_x86::X86Backend;
use rcc_bytecode::Bytecode;
use rcc_lexer::Lexer;
use rcc_parser::{pretty_printer::PrettyPrinter, Parser};

const OPTIMIZE: bool = false;
const USE_LLVM: bool = false;

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

    if USE_LLVM {
        let mut llc = Command::new("llc")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("failed to run `llc`");

        llc.stdin
            .as_mut()
            .unwrap()
            .write_all(asm.as_bytes())
            .expect("failed to pipe to `llc`");

        let asm = llc.wait_with_output().unwrap();

        let mut clang = Command::new("clang")
            .args(["-o", &path, "-xassembler", "-"])
            .stdin(Stdio::piped())
            .spawn()
            .expect("failed to run `clang`");

        clang
            .stdin
            .as_mut()
            .unwrap()
            .write_all(&asm.stdout)
            .expect("failed to pipe to `clang`");

        clang.wait().unwrap();
    } else {
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

    println!("===== PRETTY =====");
    PrettyPrinter::new(&mut stdout(), &ast).print().unwrap();
    println!("==================");

    if OPTIMIZE {
        rcc_opt::optimize_ast(&mut ast);
        println!("===== OPTIMIZED =====");
        println!("{ast:#?}");
        println!("=====================");

        println!("===== PRETTY-OPT =====");
        PrettyPrinter::new(&mut stdout(), &ast).print().unwrap();
        println!("======================");
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

    let asm = if USE_LLVM {
        if verbose {
            print!("LLVM IR");
        }
        LlvmBackend::gen_llvm(bytecodes.first().unwrap())
    } else {
        let arch = std::env::consts::ARCH;
        match arch {
            "aarch64" => {
                if verbose {
                    print!("ARM ASSEMBLY");
                }

                rcc_backend_traits::generate_assembly(&bytecodes, &mut ArmBackend)
            }
            "x86_64" => {
                if verbose {
                    print!("x86 ASSEMBLY");
                }

                rcc_backend_traits::generate_assembly(&bytecodes, &mut X86Backend)
            }
            _ => panic!("unsupported architecture"),
        }
    };

    if verbose {
        println!(" ======");
        print!("{asm}");
        println!("===========");
    }

    asm
}
