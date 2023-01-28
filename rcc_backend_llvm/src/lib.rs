use std::fmt::{self, Write};

use rcc_bytecode::{Bytecode, Instruction};
use rcc_structures::{BinOp, UnaryOp};

pub struct LlvmBackend {
    buf: String,
    indent_lvl: u32,
    tmp_counter: u32,
}

impl LlvmBackend {
    pub fn gen_llvm(bytecode: &Bytecode<'_>) -> String {
        let mut backend = Self {
            buf: String::new(),
            indent_lvl: 0,
            tmp_counter: 1,
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
        self.indent_lvl = 1;
        // FIXME: this only allocates two registers, right now we can only use two but this will change
        writeln!(
            self.buf,
            "define i32 @{}() nounwind {{\n{1}%reg.1 = alloca i32\n{1}%reg.2 = alloca i32\n",
            name,
            self.indent()
        )?;

        for instr in bytecode {
            self.write_instruction(instr)?;
        }

        writeln!(self.buf, "}}")?;
        self.indent_lvl = 0;

        Ok(())
    }

    fn write_instruction(&mut self, instruction: &Instruction) -> fmt::Result {
        write!(self.buf, "{0}; {instruction:?}\n{0}", self.indent())?;

        match instruction {
            Instruction::LoadInt(val, reg) => {
                writeln!(self.buf, "store i32 {}, ptr %reg.{}", val, reg + 1)?
            }
            Instruction::Return => {
                self.tmp_counter += 1;
                let tmp = self.tmp_counter - 1;
                writeln!(
                    self.buf,
                    "%{tmp} = load i32, ptr %reg.1\n{}ret i32 %{tmp}",
                    self.indent()
                )?
            }
            Instruction::BinaryOp(op, lhs, rhs) => self.write_binop(*op, *lhs, *rhs)?,
            Instruction::UnaryOp(op, reg) => self.write_unop(*op, *reg)?,
            Instruction::ShortCircuit(_, _, _) => todo!(),
            Instruction::BinaryBooleanOp(_, _) => todo!(),
            Instruction::DeclareVariable(_, _) => todo!(),
            Instruction::AssignVariable(_, _) => todo!(),
            Instruction::LoadVariable(_, _) => todo!(),
        }

        writeln!(self.buf)
    }

    fn write_binop(&mut self, op: BinOp, lhs: u8, rhs: u8) -> fmt::Result {
        self.tmp_counter += 3;
        writeln!(
            self.buf,
            "%{} = load i32, ptr %reg.{}",
            self.tmp_counter - 3,
            lhs + 1
        )?;
        writeln!(
            self.buf,
            "{}%{} = load i32, ptr %reg.{}",
            self.indent(),
            self.tmp_counter - 2,
            rhs + 1
        )?;

        match op {
            op @ (BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div) => writeln!(
                self.buf,
                "{}%{} = {4} i32 %{}, %{}",
                self.indent(),
                self.tmp_counter - 1, // new temp
                self.tmp_counter - 3, // lhs
                self.tmp_counter - 2, // rhs
                match op {
                    BinOp::Add => "add",
                    BinOp::Sub => "sub",
                    BinOp::Mul => "mul",
                    BinOp::Div => "div",
                    _ => unreachable!(),
                }
            )?,
            BinOp::LogicalOr => todo!(),
            BinOp::LogicalAnd => todo!(),
            BinOp::Equals => todo!(),
            BinOp::NotEquals => todo!(),
            BinOp::LessThan => todo!(),
            BinOp::LessThanOrEquals => todo!(),
            BinOp::GreaterThan => todo!(),
            BinOp::GreaterThanOrEquals => todo!(),
        }

        writeln!(
            self.buf,
            "{}store i32 %{}, ptr %reg.{}",
            self.indent(),
            self.tmp_counter - 1,
            lhs + 1
        )
    }

    fn write_unop(&mut self, op: rcc_structures::UnaryOp, reg: u8) -> fmt::Result {
        self.tmp_counter += 2;
        writeln!(
            self.buf,
            "%{} = load i32, ptr %reg.{}",
            self.tmp_counter - 2,
            reg + 1
        )?;

        match op {
            UnaryOp::Negation => writeln!(
                self.buf,
                "{}%{} = sub i32 0, %{}",
                self.indent(),
                self.tmp_counter - 1,
                self.tmp_counter - 2
            )?,
            UnaryOp::BitwiseComplement => todo!(),
            UnaryOp::LogicalNegation => todo!(),
        }

        writeln!(
            self.buf,
            "{}store i32 %{}, ptr %reg.{}",
            self.indent(),
            self.tmp_counter - 1,
            reg + 1
        )
    }
}
