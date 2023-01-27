use core::{fmt, panic};
use rcc_bytecode::{Bytecode, Instruction};
use rcc_structures::{BinOp, UnaryOp};
use register::Register;
use std::fmt::Write;

mod register;

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
            Instruction::LoadInt(val, reg) => writeln!(
                self.buf,
                "movl ${val}, %{}",
                <&u8 as Into<Register>>::into(reg)
            )?,

            Instruction::Return => writeln!(
                self.buf,
                "movl %{}, %eax\n{}\
                ret",
                <&u8 as Into<Register>>::into(&0),
                self.indent(),
            )?,

            Instruction::BinaryOp(op, lhs, rhs) => self.write_binop(op, lhs.into(), rhs.into())?,
            Instruction::UnaryOp(op, reg) => self.write_unary_op(op, reg.into())?,
        }

        writeln!(self.buf)
    }

    fn write_unary_op(&mut self, op: &UnaryOp, reg: Register) -> fmt::Result {
        match op {
            UnaryOp::Negation => writeln!(self.buf, "neg %{reg}"),
            UnaryOp::BitwiseComplement => writeln!(self.buf, "not %{reg}"),
            UnaryOp::LogicalNegation => writeln!(
                self.buf,
                "cmpl   $0, %{reg}     # set ZF on if exp == 0, set it off otherwise\n{0}\
                sete   %{1}          # set {1} register (the lower byte of {reg}) to 1 if ZF is on, note that this clears {reg}",
                self.indent(),
                reg.get_low_8()
            ),
        }
    }

    fn write_binop(&mut self, op: &BinOp, lhs: Register, rhs: Register) -> fmt::Result {
        match op {
            BinOp::Add => writeln!(self.buf, "addl %{rhs}, %{lhs} # into %{lhs}"),
            BinOp::Sub => writeln!(self.buf, "subl %{rhs}, %{lhs} # into %{lhs}"),
            BinOp::Mul => writeln!(self.buf, "imul %{rhs}, %{lhs} # into %{lhs}"),

            // x86 division (`idiv`) requires the dividend to be in EDX:EAX, and
            // the divisor register is passed separately. This means we must put
            // EAX and EDX on the stack while we do the division.
            BinOp::Div => write!(
                self.buf,
                "movl %{lhs}, %eax\n{0}\
                cdq\n{0}\
                idiv %{rhs}\n{0}\
                movl %eax, %{lhs}\n{0}",
                self.indent()
            ),

            op @ (BinOp::Equals
            | BinOp::NotEquals
            | BinOp::LessThan
            | BinOp::LessThanOrEquals
            | BinOp::GreaterThan
            | BinOp::GreaterThanOrEquals) => writeln!(
                self.buf,
                "cmpl %{rhs}, %{lhs}\n{0}\
                    movl $0, %{lhs}\n{0}\
                    set{1} %{2}",
                self.indent(),
                Self::get_relational_instruction(op),
                lhs.get_low_8()
            ),

            BinOp::LogicalOr => todo!(),
            BinOp::LogicalAnd => todo!(),
        }
    }

    fn get_relational_instruction(op: &BinOp) -> &'static str {
        match op {
            BinOp::Equals => "e",
            BinOp::NotEquals => "ne",
            BinOp::LessThan => "l",
            BinOp::LessThanOrEquals => "le",
            BinOp::GreaterThan => "g",
            BinOp::GreaterThanOrEquals => "ge",
            _ => panic!("provided binop was not relational operator!"),
        }
    }
}
