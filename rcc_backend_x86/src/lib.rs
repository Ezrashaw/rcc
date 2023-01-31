use core::{fmt, panic};
use rcc_bytecode::{Bytecode, Instruction, ReadLocation, WriteLocation};
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
        format!("%{}", Register::from_u8(loc.reg()))
    }

    fn rloc_to_asm(loc: &ReadLocation) -> String {
        match loc {
            ReadLocation::Writable(wloc) => Self::wloc_to_asm(wloc),
            ReadLocation::Constant(val) => format!("${val}"),
        }
    }

    fn write_function(&mut self, name: &str, bytecode: &[Instruction]) -> fmt::Result {
        writeln!(self.buf, ".globl {}\n{0}:", name)?;

        self.indent_lvl = 1;

        // write function prologue, sets up a stack frame
        writeln!(
            self.buf,
            "{0}pushq %rbp         # save old value of RBP\n{0}\
            movq %rsp, %rbp    # current top of stack is bottom of new stack frame\n",
            self.indent()
        )?;

        for instr in bytecode {
            self.write_instruction(instr)?;
        }

        Ok(())
    }

    fn write_instruction(&mut self, instruction: &Instruction) -> fmt::Result {
        write!(self.buf, "{0}# {instruction:?}\n{0}", self.indent())?;

        match instruction {
            Instruction::LoadInt(val, reg) => {
                writeln!(self.buf, "movl ${val}, %{}", Register::from_u8(reg.reg()))?
            }

            Instruction::Return(loc) => writeln!(
                self.buf,
                "movl {1}, %eax\n{0}\
                pop %rbp        # restore old RBP; now RSP is where it was before prologue\n{0}\
                ret",
                self.indent(),
                Self::rloc_to_asm(loc),
            )?,

            Instruction::BinaryOp(op, lhs, rhs) => self.write_binop(op, lhs, rhs)?,
            Instruction::UnaryOp(op, loc) => self.write_unary_op(op, loc)?,

            Instruction::ShortCircuit(rloc, should_short, jump_loc) => writeln!(
                self.buf,
                "cmpl $0, {} # check if e1 is true\n{}\
                {} _{jump_loc}  # e1 is 0, we don't need to evaluate clause 2",
                Self::wloc_to_asm(rloc),
                self.indent(),
                if *should_short { "jne" } else { "je" }
            )?,
            Instruction::BinaryBooleanOp(wloc, jump_loc) => writeln!(
                self.buf,
                "cmpl $0, {}  # check if e2 is true\n{}\
                setne %{}      # set AL register (the low byte of EAX) to 1 iff e2 != 0\n\
            _{jump_loc}: # short-circuit jump label",
                Self::wloc_to_asm(wloc),
                self.indent(),
                Register::from_u8(wloc.reg()).get_low_8()
            )?,

            Instruction::AssignVariable(var, rloc) => writeln!(
                self.buf,
                "movl {}, -{}(%rbp)",
                Self::rloc_to_asm(rloc),
                (var + 1) * 4
            )?,
            Instruction::LoadVariable(var, wloc) => writeln!(
                self.buf,
                "movl -{}(%rbp), {}",
                (var + 1) * 4,
                Self::wloc_to_asm(wloc)
            )?,

            Instruction::IfThen(pre_else, rloc) => writeln!(
                self.buf,
                "cmpl $0, {}\n{}je _{pre_else}",
                Self::rloc_to_asm(rloc),
                self.indent()
            )?,
            Instruction::PostIf(post_else, pre_else) => {
                writeln!(self.buf, "jmp _{post_else}\n_{pre_else}:")?
            }
            // FIXME: gah, this messes up the formatting
            Instruction::PostConditionalDummy(post_else) => writeln!(self.buf, "_{post_else}:")?,
        }

        writeln!(self.buf)
    }

    fn write_unary_op(&mut self, op: &UnaryOp, wloc: &WriteLocation) -> fmt::Result {
        let wloc_str = Self::wloc_to_asm(wloc);
        match op {
            UnaryOp::Negation => writeln!(self.buf, "neg {wloc_str}"),
            UnaryOp::BitwiseComplement => writeln!(self.buf, "not {wloc_str}"),
            UnaryOp::LogicalNegation => writeln!(
                self.buf,
                "cmpl   $0, {wloc_str}     # set ZF on if exp == 0, set it off otherwise\n{0}\
                sete   %{1}          # set {1} register (the lower byte of {wloc_str}) to 1 if ZF is on, note that this clears {wloc_str}",
                self.indent(),
                Register::from_u8(wloc.reg()).get_low_8()
            ),
        }
    }

    fn write_binop(&mut self, op: &BinOp, lhs: &WriteLocation, rhs: &ReadLocation) -> fmt::Result {
        let lhs_str = Self::wloc_to_asm(lhs);
        let rhs_str = Self::rloc_to_asm(rhs);
        match op {
            BinOp::Add => writeln!(self.buf, "addl {rhs_str}, {lhs_str} # into {lhs_str}"),
            BinOp::Sub => writeln!(self.buf, "subl {rhs_str}, {lhs_str} # into {lhs_str}"),
            BinOp::Mul => writeln!(self.buf, "imul {rhs_str}, {lhs_str} # into {lhs_str}"),

            // x86 division (`idiv`) requires the dividend to be in EDX:EAX, and
            // the divisor register is passed separately. This means we must put
            // EAX and EDX on the stack while we do the division.
            BinOp::Div => write!(
                self.buf,
                "movl {lhs_str}, %eax\n{0}\
                cdq\n{0}\
                idiv {rhs_str}\n{0}\
                movl %eax, {lhs_str}\n{0}",
                self.indent()
            ),

            op @ (BinOp::Equals
            | BinOp::NotEquals
            | BinOp::LessThan
            | BinOp::LessThanOrEquals
            | BinOp::GreaterThan
            | BinOp::GreaterThanOrEquals) => writeln!(
                self.buf,
                "cmpl {rhs_str}, {lhs_str}\n{0}\
                    movl $0, {lhs_str}\n{0}\
                    set{1} %{2}",
                self.indent(),
                Self::get_relational_instruction(op),
                Register::from_u8(lhs.reg()).get_low_8()
            ),

            BinOp::LogicalOr => panic!("`Instruction::BinOp(LogicalOr)` is not allowed, use the `ShortCircuit` and `BinaryBooleanOp` instructions instead."),
            BinOp::LogicalAnd => panic!("`Instruction::BinOp(LogicalAnd)` is not allowed, use the `ShortCircuit` and `BinaryBooleanOp` instructions instead."),
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
