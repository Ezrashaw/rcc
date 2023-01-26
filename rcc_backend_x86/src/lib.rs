use core::{fmt, panic};
use rcc_bytecode::{Bytecode, Instruction};
use std::fmt::Write;

pub struct X86Backend<'a> {
    bytecode: Bytecode<'a>,
}

impl<'a> X86Backend<'a> {
    pub fn new(bytecode: Bytecode<'a>) -> Self {
        Self { bytecode }
    }

    fn indent(lvl: u32) -> &'static str {
        match lvl {
            1 => "    ",
            _ => panic!("too much indent"),
        }
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
            Self::write_instruction(buf, instr, 1)?;
        }

        Ok(())
    }

    fn write_instruction(
        buf: &mut String,
        instruction: &Instruction,
        indent_lvl: u32,
    ) -> fmt::Result {
        write!(buf, "{0}# {instruction:?}\n{0}", Self::indent(indent_lvl))?;

        match instruction {
            Instruction::LoadInt(val) => writeln!(buf, "movl ${val}, %eax")?,
            Instruction::Return => writeln!(buf, "ret")?,

            // unary ops
            Instruction::Negate => writeln!(buf, "neg %eax")?,
            Instruction::BitwiseComplement => writeln!(buf, "not %eax")?,
            Instruction::LogicalNegate => writeln!(
                buf,
                "cmpl   $0, %eax     # set ZF on if exp == 0, set it off otherwise\n{0}\
                movl   $0, %eax     # zero out EAX (doesn't change FLAGS)\n{0}\
                sete   %al          # set AL register (the lower byte of EAX) to 1 iff ZF is on",
                Self::indent(indent_lvl)
            )?,

            Instruction::Push => writeln!(buf, "push %eax # primary register onto stack")?,
            Instruction::Pop => writeln!(
                buf,
                "movl %eax, %ecx # move primary to secondary\n{}\
                pop %eax        # stack to primary",
                Self::indent(indent_lvl)
            )?,

            Instruction::Add => writeln!(buf, "addl %ecx, %eax # into %eax")?,
            Instruction::Sub => writeln!(buf, "subl %ecx, %eax # into %eax")?,
            Instruction::Mul => writeln!(buf, "imul %ecx, %eax # into %eax")?,
            Instruction::Div => writeln!(
                buf,
                "cdq\n{}\
                idiv %ecx
                ",
                Self::indent(indent_lvl)
            )?,
        }

        writeln!(buf)
    }
}
