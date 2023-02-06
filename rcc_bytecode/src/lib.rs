#![feature(let_chains)]

use rcc_parser::ast::{Block, Program};

mod instruction;
mod lowering;
mod utils;

pub use instruction::{Instruction, Register, RegisterOrConst};

/// `rcc` intermediate representation.
///
/// Generated per-function, contains a [`Vec<Instruction>`] which is the
/// lowered AST for that function.
///
/// Use the [`Bytecode::from_function`] method to generate bytecode for a
/// function. Or the [`Bytecode::from_ast`] method to generate bytecode for all
/// functions.
#[derive(Debug)]
pub struct Bytecode<'a> {
    // "public" fields
    fn_name: &'a str,
    instr: Vec<Instruction>,

    // "internal" fields
    label_counter: u32,
    // FIXME: come up with some algorithim or something to remove this vec
    allocated_registers: Vec<Register>,
    loop_start: Vec<u32>,
    loop_end: Vec<u32>,
}

impl<'a> Bytecode<'a> {
    pub fn from_ast(ast: &Program<'a>) -> Vec<Self> {
        let mut functions = Vec::new();

        for function in &ast.functions {
            if let Some(body) = &function.body {
                functions.push(Self::from_function(function.name, body));
            }
        }

        functions
    }

    pub fn from_function(name: &'a str, body: &Block) -> Self {
        let mut bytecode = Self {
            fn_name: name,
            instr: Vec::new(),
            label_counter: 0,
            allocated_registers: Vec::new(),
            loop_start: Vec::new(),
            loop_end: Vec::new(),
        };

        bytecode.append_from_block(body);

        bytecode
    }

    pub fn fn_name(&self) -> &'a str {
        self.fn_name
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instr
    }
}
