use core::{fmt, panic};
use rcc_bytecode::{Bytecode, Instruction};
use rcc_structures::BinOp;
use std::fmt::Write;

pub struct X86Backend {
    buf: String,
    indent_lvl: u32,
}

impl X86Backend {
    pub fn gen_x86(bytecode: &Bytecode<'_>) -> String {
        let mut backend = Self {
            buf: String::new(),
            indent_lvl: 0,
        };

        let function = bytecode.function();

        backend.write_function(function.0, function.1).unwrap();

        backend.buf
    }

    fn indent(&self) -> &'static str {
        match self.indent_lvl {
            1 => "    ",
            _ => panic!("too much indent"),
        }
    }
    fn write_function(&mut self, name: &str, bytecode: &[Instruction]) -> fmt::Result {
        writeln!(self.buf, ".globl {}\n{0}:", name)?;

        self.indent_lvl = 1;

        for instr in bytecode {
            self.write_instruction(instr)?;
        }

        Ok(())
    }

    fn write_instruction(&mut self, instruction: &Instruction) -> fmt::Result {
        write!(self.buf, "{0}# {instruction:?}\n{0}", self.indent())?;

        match instruction {
            Instruction::LoadInt(val) => writeln!(self.buf, "movl ${val}, %eax")?,
            Instruction::Return => writeln!(self.buf, "ret")?,

            // unary ops
            Instruction::Negate => writeln!(self.buf, "neg %eax")?,
            Instruction::BitwiseComplement => writeln!(self.buf, "not %eax")?,
            Instruction::LogicalNegate => writeln!(
                self.buf,
                "cmpl   $0, %eax     # set ZF on if exp == 0, set it off otherwise\n{0}\
                movl   $0, %eax     # zero out EAX (doesn't change FLAGS)\n{0}\
                sete   %al          # set AL register (the lower byte of EAX) to 1 iff ZF is on",
                self.indent()
            )?,

            Instruction::Push => writeln!(self.buf, "push %eax # primary register onto stack")?,
            Instruction::Pop => writeln!(
                self.buf,
                "movl %eax, %ecx # move primary to secondary\n{}\
                pop %eax        # stack to primary",
                self.indent()
            )?,

            Instruction::BinaryOp(op) => self.write_binop(op)?,
        }

        writeln!(self.buf)
    }

    fn write_binop(&mut self, op: &BinOp) -> fmt::Result {
        match op {
            BinOp::Add => writeln!(self.buf, "addl %ecx, %eax # into %eax"),
            BinOp::Sub => writeln!(self.buf, "subl %ecx, %eax # into %eax"),
            BinOp::Mul => writeln!(self.buf, "imul %ecx, %eax # into %eax"),
            BinOp::Div => writeln!(
                self.buf,
                "cdq\n{}\
                idiv %ecx",
                self.indent()
            ),
            BinOp::LogicalOr => todo!(),
            BinOp::LogicalAnd => todo!(),
            BinOp::Equals => todo!(),
            BinOp::NotEquals => todo!(),
            BinOp::LessThan => todo!(),
            BinOp::LessThanOrEquals => todo!(),
            BinOp::GreaterThan => todo!(),
            BinOp::GreaterThanOrEquals => todo!(),
        }
    }
}
