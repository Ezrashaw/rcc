#![feature(let_chains)]

use rcc_parser::ast::Function;

mod instruction;
mod lowering;
mod utils;

pub use instruction::{Instruction, ReadLocation, WriteLocation};

/// `rcc` intermediate representation.
///
/// Generated per-function, contains a [`Vec<Instruction>`] which is the
/// lowered AST for that function.
///
/// Use the [`Bytecode::from_function`] method to generate bytecode for a
/// function.
#[derive(Debug)]
pub struct Bytecode<'a> {
    // "public" fields
    fn_name: &'a str,
    instr: Vec<Instruction>,

    // "internal" fields
    label_counter: u32,
    // FIXME: come up with some algorithim or something to remove this vec
    allocated_registers: Vec<WriteLocation>,
}

impl<'a> Bytecode<'a> {
    pub fn from_function(function: &Function<'a>) -> Self {
        let mut bytecode = Self {
            fn_name: function.name,
            instr: Vec::new(),
            label_counter: 0,
            allocated_registers: Vec::new(),
        };

        bytecode.append_from_block(&function.block);

        bytecode
    }

    pub fn fn_name(&self) -> &'a str {
        self.fn_name
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instr
    }
}
