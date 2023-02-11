mod register;

use rcc_backend_traits::{write_asm, write_asm_no_indent, Backend, BackendContext};
use rcc_bytecode::{BinOp, Instruction, Register, RegisterOrConst, UnaryOp};
use register::ArmRegister;
use std::fmt::Write;

pub struct ArmBackend;

impl Backend for ArmBackend {
    fn write_function(&mut self, ctx: &mut BackendContext, fn_name: &str) {
        if std::env::consts::OS == "macos" {
            write_asm!(ctx, ".globl _{fn_name}");
            write_asm_no_indent!(ctx, "_{fn_name}:");
        } else {
            write_asm!(ctx, ".globl {fn_name}");
            write_asm_no_indent!(ctx, "{fn_name}:");
        };

        // write function prologue, sets up a stack frame
        // HACK: you don't need more than 8 variables right?
        write_asm!(ctx, "sub sp, sp, #32");
    }

    fn write_instruction(&mut self, ctx: &mut BackendContext, instruction: &Instruction) {
        match instruction {
            Instruction::Move(from, to) => {
                write_asm!(ctx, "mov {}, {}", Self::reg(to), Self::roc(from));
            }

            Instruction::Return(val) => {
                write_asm!(ctx, "mov w0, {}", Self::roc(val));
                write_asm!(ctx, "add sp, sp, #32");
                write_asm!(ctx, "ret");
            }

            Instruction::CompareJump(val, should_jump, jump_loc) => {
                write_asm!(ctx, "cmp {}, #0", Self::roc(val));
                write_asm!(
                    ctx,
                    "{} _{jump_loc}",
                    if *should_jump { "bne" } else { "beq" }
                );
            }

            Instruction::NormalizeBoolean(reg) => {
                write_asm!(ctx, "cmp {}, #0", Self::reg(reg));
                write_asm!(ctx, "cset {}, ne", Self::reg(reg));
            }

            Instruction::AssignVariable(var, val) => {
                write_asm!(ctx, "str {}, {}", Self::roc(val), Self::var(*var));
            }
            Instruction::LoadVariable(var, reg) => {
                write_asm!(ctx, "ldr {}, {}", Self::reg(reg), Self::var(*var));
            }

            Instruction::JumpDummy(loc) => write_asm_no_indent!(ctx, "_{loc}:"),
            Instruction::UnconditionalJump(loc) => write_asm!(ctx, "b _{loc}"),

            Instruction::BinaryOp(op, lhs, rhs) => Self::write_binary_op(ctx, *op, lhs, rhs),
            Instruction::UnaryOp(op, reg) => Self::write_unary_op(ctx, *op, reg),
        }
    }
}

impl ArmBackend {
    /// Gets the ARM string for the given bytecode register.
    fn reg(reg: &Register) -> &'static str {
        ArmRegister::from_u8(reg.register_number()).get_str()
    }

    /// Gets the ARM string for the given [`RegisterOrConst`].
    fn roc(val: &RegisterOrConst) -> String {
        match val {
            RegisterOrConst::Register(reg) => Self::reg(reg).to_string(),
            RegisterOrConst::Constant(val) => format!("#{val}"),
        }
    }

    /// Gets the ARM string for the given variable id.
    fn var(id: u32) -> String {
        format!("[sp, {}]", 32 - (id + 1) * 4)
    }

    fn write_unary_op(ctx: &mut BackendContext, op: UnaryOp, register: &Register) {
        let reg = Self::reg(register);
        match op {
            UnaryOp::Negation => write_asm!(ctx, "neg {reg}"),
            UnaryOp::BitwiseComplement => write_asm!(ctx, "mvn {reg}"),
            UnaryOp::LogicalNegation => {
                write_asm!(ctx, "cmp {reg}, #0");
                write_asm!(ctx, "cset {reg}, eq");
            }
        }
    }

    fn write_binary_op(ctx: &mut BackendContext, op: BinOp, lhs: &Register, rhs: &RegisterOrConst) {
        let lh = Self::reg(lhs);
        let rh = Self::roc(rhs);

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
                write_asm!(
                    ctx,
                    "cset {lh}, {}",
                    match op {
                        BinOp::Equals => "eq",
                        BinOp::NotEquals => "ne",
                        BinOp::LessThan => "lt",
                        BinOp::LessThanOrEquals => "le",
                        BinOp::GreaterThan => "gt",
                        BinOp::GreaterThanOrEquals => "ge",
                        _ => panic!("provided binop was not relational operator!"),
                    }
                );
            }

            BinOp::LeftShift => write_asm!(ctx, "lsl {lh}, {lh}, {rh}"),
            BinOp::RightShift => write_asm!(ctx, "lsr {lh}, {lh}, {rh}"),

            BinOp::BitwiseOr => write_asm!(ctx, "orr {lh}, {lh}, {rh}"),
            BinOp::ExclusiveOr => write_asm!(ctx, "eor {lh}, {lh}, {rh}"),
            BinOp::BitwiseAnd => write_asm!(ctx, "and {lh}, {lh}, {rh}"),
        }
    }
}
