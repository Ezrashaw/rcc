mod register;

use rcc_backend_traits::{write_asm, write_asm_no_indent, Backend, BackendContext};
use rcc_bytecode::{Instruction, ReadLocation, WriteLocation};
use rcc_structures::{BinOp, UnaryOp};
use register::Register;
use std::fmt::Write;

pub struct ArmBackend;

impl Backend for ArmBackend {
    fn write_function(&mut self, ctx: &mut BackendContext, fn_name: &str) {
        if std::env::consts::OS == "macos" {
            write_asm!(ctx, ".globl _{fn_name}\n_{fn_name}:");
        } else {
            write_asm!(ctx, ".globl {fn_name}\n{fn_name}:");
        }

        // write function prologue, sets up a stack frame
        // HACK: you don't need more than 8 variables right?
        write_asm!(ctx, "sub sp, sp, #32");
    }

    fn write_instruction(&mut self, ctx: &mut BackendContext, instruction: &Instruction) {
        match instruction {
            Instruction::Move(from, to) => {
                write_asm!(ctx, "mov {}, {}", Self::wl(to), Self::rl(from));
            }

            Instruction::Return(loc) => {
                write_asm!(ctx, "mov w0, {}", Self::rl(loc));
                write_asm!(ctx, "add sp, sp, #32");
                write_asm!(ctx, "ret");
            }

            Instruction::CompareJump(rloc, should_jump, jump_loc) => {
                write_asm!(ctx, "cmp {}, #0", Self::wl(rloc));
                write_asm!(
                    ctx,
                    "{} _{jump_loc}",
                    if *should_jump { "bne" } else { "beq" }
                );
            }

            Instruction::NormalizeBoolean(wloc) => {
                write_asm!(ctx, "cmp {}, #0", Self::wl(wloc));
                write_asm!(ctx, "cset {}, ne", Self::wl(wloc));
            }

            Instruction::AssignVariable(var, rloc) => {
                write_asm!(ctx, "str {}, {}", Self::rl(rloc), Self::var(*var));
            }
            Instruction::LoadVariable(var, wloc) => {
                write_asm!(ctx, "ldr {}, {}", Self::wl(wloc), Self::var(*var));
            }

            Instruction::JumpDummy(loc) => write_asm_no_indent!(ctx, "_{loc}:"),
            Instruction::UnconditionalJump(loc) => write_asm!(ctx, "b _{loc}"),

            Instruction::BinaryOp(op, lhs, rhs) => self.write_binary_op(ctx, *op, lhs, rhs),
            Instruction::UnaryOp(op, wloc) => self.write_unary_op(ctx, *op, wloc),
        }
    }
}

impl ArmBackend {
    fn wl(loc: &WriteLocation) -> String {
        format!("{}", Register::from_u8(loc.reg()))
    }

    fn rl(loc: &ReadLocation) -> String {
        match loc {
            ReadLocation::Writable(wloc) => Self::wl(wloc),
            ReadLocation::Constant(val) => format!("#{val}"),
        }
    }

    fn var(id: u32) -> String {
        format!("[sp, {}]", 32 - (id + 1) * 4)
    }

    fn write_unary_op(&mut self, ctx: &mut BackendContext, op: UnaryOp, wloc: &WriteLocation) {
        let wl = Self::wl(wloc);
        match op {
            UnaryOp::Negation => write_asm!(ctx, "neg {wl}, {wl}"),
            UnaryOp::BitwiseComplement => write_asm!(ctx, "mvn {wl}, {wl}"),
            UnaryOp::LogicalNegation => {
                write_asm!(ctx, "cmp {wl}, #0");
                write_asm!(ctx, "cset {wl}, eq");
            }
        }
    }

    fn write_binary_op(
        &mut self,
        ctx: &mut BackendContext,
        op: BinOp,
        lhs: &WriteLocation,
        rhs: &ReadLocation,
    ) {
        let lh = Self::wl(lhs);
        let rh = Self::rl(rhs);
        match op {
            BinOp::Add => write_asm!(ctx, "add  {lh}, {lh}, {rh}"),
            BinOp::Sub => write_asm!(ctx, "sub  {lh}, {lh}, {rh}"),
            BinOp::Mul => write_asm!(ctx, "mul  {lh}, {lh}, {rh}"),
            BinOp::Div => write_asm!(ctx, "sdiv {lh}, {lh}, {rh}"),
            BinOp::Modulo => todo!(),

            op @ (BinOp::Equals
            | BinOp::NotEquals
            | BinOp::LessThan
            | BinOp::LessThanOrEquals
            | BinOp::GreaterThan
            | BinOp::GreaterThanOrEquals) => {
                write_asm!(ctx, "cmp {lh}, {rh}");
                write_asm!(ctx, "cset {lh}, {}", match op {
                    BinOp::Equals => "eq",
                    BinOp::NotEquals => "ne",
                    BinOp::LessThan => "lt",
                    BinOp::LessThanOrEquals => "le",
                    BinOp::GreaterThan => "gt",
                    BinOp::GreaterThanOrEquals => "ge",
                    _ => panic!("provided binop was not relational operator!"),
                });
            },

            BinOp::LeftShift => todo!(),
            BinOp::RightShift => todo!(),

            BinOp::BitwiseOr => todo!(),
            BinOp::ExclusiveOr => todo!(),
            BinOp::BitwiseAnd => todo!(),

            BinOp::LogicalOr => panic!("`Instruction::BinOp(LogicalOr)` is not allowed, use the `ShortCircuit` and `BinaryBooleanOp` instructions instead."),
            BinOp::LogicalAnd => panic!("`Instruction::BinOp(LogicalAnd)` is not allowed, use the `ShortCircuit` and `BinaryBooleanOp` instructions instead."),
        }
    }
}
