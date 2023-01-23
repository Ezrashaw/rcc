use core::fmt;
use rcc_bytecode::{Bytecode, Instruction};
use std::fmt::Write;

pub struct X86Backend<'a> {
    bytecode: Bytecode<'a>,
}

impl<'a> X86Backend<'a> {
    pub fn new(bytecode: Bytecode<'a>) -> Self {
        Self { bytecode }
    }

    pub fn gen_x86(&self) -> String {
        let mut buf = String::new();

        let function = self.bytecode.function();
        Self::write_function(&mut buf, &(function.0, &function.1)).unwrap();

        buf
    }

    fn write_function(buf: &mut String, function: &(&str, &[Instruction])) -> fmt::Result {
        writeln!(buf, ".globl {}\n{0}:", function.0)?;

        for instr in function.1 {
            write!(buf, "    ")?;
            Self::write_instruction(buf, instr)?;
        }

        Ok(())
    }

    fn write_instruction(buf: &mut String, instruction: &Instruction) -> fmt::Result {
        match instruction {
            Instruction::LoadInt(val) => writeln!(buf, "movl ${val}, %eax"),
            Instruction::Return => writeln!(buf, "ret"),
        }
    }
}
