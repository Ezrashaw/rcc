mod register;

use rcc_bytecode::{Bytecode, Instruction, ReadLocation, WriteLocation};
use rcc_structures::{BinOp, UnaryOp};
use register::Register;
use std::fmt::{self, Write};

pub struct ArmBackend {
    buf: String,
    indent_lvl: u32,
}

impl ArmBackend {
    pub fn gen_arm(bytecode: &Bytecode<'_>) -> String {
        let mut backend = Self {
            buf: String::new(),
            indent_lvl: 0,
        };

        backend
            .write_function(bytecode.fn_name(), bytecode.instructions())
            .unwrap();

        backend.buf
    }

    fn indent(&self) -> &'static str {
        match self.indent_lvl {
            1 => "    ",
            _ => panic!("too much indent"),
        }
    }

    fn wloc_to_asm(loc: &WriteLocation) -> String {
        format!("{}", Register::from_u8(loc.reg()))
    }

    fn rloc_to_asm(loc: &ReadLocation) -> String {
        match loc {
            ReadLocation::Writable(wloc) => Self::wloc_to_asm(wloc),
            ReadLocation::Constant(val) => format!("#{val}"),
        }
    }

    fn write_function(&mut self, name: &str, bytecode: &[Instruction]) -> fmt::Result {
        if std::env::consts::OS == "macos" {
            writeln!(self.buf, ".globl _{name}\n_{name}:")?;
        } else {
            writeln!(self.buf, ".globl {name}\n{name}:")?;
        }

        self.indent_lvl = 1;

        // write function prologue, sets up a stack frame
        // HACK: you don't need more than 8 variables right?
        writeln!(self.buf, "{}sub sp, sp, #32\n", self.indent())?;

        for instr in bytecode {
            self.write_instruction(instr)?;
        }

        Ok(())
    }

    fn write_instruction(&mut self, instruction: &Instruction) -> fmt::Result {
        write!(self.buf, "{0}# {instruction:?}\n{0}", self.indent())?;

        match instruction {
            Instruction::Move(from, to) => {
                writeln!(
                    self.buf,
                    "mov {}, {}",
                    Self::wloc_to_asm(to),
                    Self::rloc_to_asm(from)
                )?;
            }

            Instruction::Return(loc) => writeln!(
                self.buf,
                "mov w0, {}\n{}\
                add sp, sp, #32\n{1}\
                ret",
                Self::rloc_to_asm(loc),
                self.indent(),
            )?,

            Instruction::BinaryOp(op, lhs, rhs) => self.write_binop(*op, lhs, rhs)?,
            Instruction::UnaryOp(op, loc) => self.write_unary_op(*op, loc)?,

            Instruction::CompareJump(rloc, should_jump, jump_loc) => writeln!(
                self.buf,
                "cmp {}, #0\n{}\
                {} _{jump_loc}",
                Self::wloc_to_asm(rloc),
                self.indent(),
                if *should_jump { "bne" } else { "beq" }
            )?,
            Instruction::BinaryBooleanOp(wloc, jump_loc) => writeln!(
                self.buf,
                "cmp {}, #0\n{}\
                cset {}, ne\n\
            _{jump_loc}:",
                Self::wloc_to_asm(wloc),
                self.indent(),
                Register::from_u8(wloc.reg())
            )?,

            Instruction::AssignVariable(var, rloc) => writeln!(
                self.buf,
                "str {}, [sp, {}]",
                Self::rloc_to_asm(rloc),
                32 - (var + 1) * 4
            )?,
            Instruction::LoadVariable(var, wloc) => writeln!(
                self.buf,
                "ldr {}, [sp, {}]",
                Self::wloc_to_asm(wloc),
                32 - (var + 1) * 4
            )?,
            
            // FIXME: gah, this messes up the formatting
            Instruction::JumpDummy(post_else) => writeln!(self.buf, "_{post_else}:")?,
            Instruction::UnconditionalJump(loc) => writeln!(self.buf, "b _{loc}")?,
        }

        writeln!(self.buf)
    }

    fn write_unary_op(&mut self, op: UnaryOp, wloc: &WriteLocation) -> fmt::Result {
        let wloc_str = Self::wloc_to_asm(wloc);
        match op {
            UnaryOp::Negation => writeln!(self.buf, "neg {wloc_str}, {wloc_str}"),
            UnaryOp::BitwiseComplement => writeln!(self.buf, "mvn {wloc_str}, {wloc_str}"),
            UnaryOp::LogicalNegation => writeln!(
                self.buf,
                "cmp   {wloc_str}, #0\n{0}\
                cset   {1}, eq",
                self.indent(),
                Register::from_u8(wloc.reg())
            ),
        }
    }

    fn write_binop(&mut self, op: BinOp, lhs: &WriteLocation, rhs: &ReadLocation) -> fmt::Result {
        let lhs_str = Self::wloc_to_asm(lhs);
        let rhs_str = Self::rloc_to_asm(rhs);
        match op {
            BinOp::Add => writeln!(self.buf, "add  {lhs_str}, {lhs_str}, {rhs_str}"),
            BinOp::Sub => writeln!(self.buf, "sub  {lhs_str}, {lhs_str}, {rhs_str}"),
            BinOp::Mul => writeln!(self.buf, "mul  {lhs_str}, {lhs_str}, {rhs_str}"),
            BinOp::Div => writeln!(self.buf, "sdiv {lhs_str}, {lhs_str}, {rhs_str}"),

            op @ (BinOp::Equals
            | BinOp::NotEquals
            | BinOp::LessThan
            | BinOp::LessThanOrEquals
            | BinOp::GreaterThan
            | BinOp::GreaterThanOrEquals) => writeln!(
                self.buf,
                "cmp {lhs_str}, {rhs_str}\n{}\
                cset {lhs_str}, {}",
                self.indent(),
                Self::get_relational_reg(op),
            ),

            BinOp::LogicalOr => panic!("`Instruction::BinOp(LogicalOr)` is not allowed, use the `ShortCircuit` and `BinaryBooleanOp` instructions instead."),
            BinOp::LogicalAnd => panic!("`Instruction::BinOp(LogicalAnd)` is not allowed, use the `ShortCircuit` and `BinaryBooleanOp` instructions instead."),
        }
    }

    fn get_relational_reg(op: BinOp) -> &'static str {
        match op {
            BinOp::Equals => "eq",
            BinOp::NotEquals => "ne",
            BinOp::LessThan => "lt",
            BinOp::LessThanOrEquals => "le",
            BinOp::GreaterThan => "gt",
            BinOp::GreaterThanOrEquals => "ge",
            _ => panic!("provided binop was not relational operator!"),
        }
    }
}
