// TODO: create some sort of TokenStream or TokenReader and maybe abstract the lexer into that (this could be done with `Iterator`)

// TODO: implement a tree for variables, instead of the bad hashmap system we currently use.

// TODO: create an IR

// TODO: function validition, we need new step with variable and function validationuse std::{path::Path, process::Command};

// TODO: `lib.rs` is completely incorrect, we shouldn't print to the screen OR touch the filesystem

use std::{fs, path::Path, process::Command};

use generator::Generator;
use lexer::{token::Token, Lexer};
use parser::Parser;

mod ctypes;
mod generator;
mod lexer;
mod parser;
mod peekable;

pub fn compile(c_code: String, output_path: &Path, filename: String) {
    let mut lexer = Lexer::new(c_code.as_bytes(), filename);
    let tokens: Vec<Token> = lexer.collect();

    #[cfg(debug_assertions)]
    {
        println!("=====TOKENS=====");
        for token in &tokens {
            println!("{:?}", token);
        }
    }

    let mut parser = Parser::new(tokens.into_iter());
    let ast = parser.read_program();

    #[cfg(debug_assertions)]
    {
        println!("=====AST=====");
        println!("{:?}", ast);
    }

    let generator = Generator::new(&ast);
    let asm = generator.gen_asm();

    #[cfg(debug_assertions)]
    {
        println!("=====ASM=====");
        println!("{}", asm);
    }

    fs::write("assembly.s", asm).unwrap();

    Command::new("gcc")
        .arg("assembly.s")
        .arg("-m32")
        .arg("-o")
        .arg(output_path)
        .spawn()
        .expect("gcc command failed to run")
        .wait()
        .unwrap();

    fs::remove_file("assembly.s").unwrap();
}
