use core::panic;
use rcc_backend_traits::{write_asm, write_asm_no_indent, Backend, BackendContext};
use rcc_bytecode::Instruction;
use rcc_bytecode::Register;
use rcc_bytecode::RegisterOrConst;
use rcc_structures::{BinOp, UnaryOp};
use register::X86Register;
use std::fmt::Write;

mod register;

pub struct X86Backend;

impl Backend for X86Backend {
    fn write_function(&mut self, ctx: &mut BackendContext, fn_name: &str) {
        if std::env::consts::OS == "macos" {
            write_asm!(ctx, ".globl _{fn_name}");
            write_asm_no_indent!(ctx, "_{fn_name}:");
        } else {
            write_asm!(ctx, ".globl {fn_name}");
            write_asm_no_indent!(ctx, "{fn_name}:");
        };

        // write function prologue, sets up a stack frame
        write_asm!(ctx, "pushq %rbp         # save old value of RBP");
        write_asm!(
            ctx,
            "movq %rsp, %rbp    # current top of stack is bottom of new stack frame"
        );
    }

    fn write_instruction(&mut self, ctx: &mut BackendContext, instruction: &Instruction) {
        match instruction {
            Instruction::Move(from, to) => {
                write_asm!(ctx, "movl {}, {}", Self::roc(from), Self::reg(to));
            }

            Instruction::Return(val) => {
                write_asm!(ctx, "movl {}, %eax", Self::roc(val));
                write_asm!(
                    ctx,
                    "pop %rbp # restore old RBP; now RSP is where it was before prologue"
                );
                write_asm!(ctx, "ret");
            }
            Instruction::CompareJump(val, should_jump, jump_loc) => {
                // this is interesting: we have to upgrade because
                // `cmpl $0, $1` isn't allowed. This makes perfect sense and we
                // should optimize it out.
                let val = Self::upgrade_roc(ctx, val);
                write_asm!(ctx, "cmpl $0, {}", val);

                write_asm!(
                    ctx,
                    "j{} _{jump_loc}",
                    if *should_jump { "ne" } else { "e" }
                );
            }

            Instruction::NormalizeBoolean(reg) => {
                write_asm!(ctx, "cmpl $0, {}  # check if e2 is true", Self::reg(reg));
                write_asm!(ctx, "setne {}", Self::reg_low_8(reg));
            }

            Instruction::AssignVariable(var, val) => {
                write_asm!(ctx, "movl {}, {}", Self::roc(val), Self::var(*var));
            }
            Instruction::LoadVariable(var, reg) => {
                write_asm!(ctx, "movl {}, {}", Self::var(*var), Self::reg(reg));
            }

            Instruction::JumpDummy(loc) => write_asm_no_indent!(ctx, "_{loc}:"),
            Instruction::UnconditionalJump(loc) => write_asm!(ctx, "jmp _{loc}"),

            Instruction::BinaryOp(op, lhs, rhs) => Self::write_binary_op(ctx, *op, lhs, rhs),
            Instruction::UnaryOp(op, reg) => Self::write_unary_op(ctx, *op, reg),
        }
    }
}

impl X86Backend {
    /// Gets the x86 string for the given bytecode register.
    fn reg(reg: &Register) -> &'static str {
        X86Register::from_u8(reg.register_number()).get_str()
    }

    /// Gets the x86 string for the low 8 bits (low byte) of the given bytecode
    /// register.
    fn reg_low_8(reg: &Register) -> &'static str {
        X86Register::from_u8(reg.register_number()).get_low_8()
    }

    /// Gets the x86 string for the given [`RegisterOrConst`].
    fn roc(val: &RegisterOrConst) -> String {
        match val {
            RegisterOrConst::Register(reg) => Self::reg(reg).to_string(),
            RegisterOrConst::Constant(val) => format!("${val}"),
        }
    }

    /// Gets the x86 string for the given variable id.
    fn var(id: u32) -> String {
        format!("-{}(%rbp)", (id + 1) * 4)
    }

    /// Ensures that the given [`RegisterOrConst`] is in a register.
    ///
    /// If that is not the case, then the value is put in `%ebx`. The reason
    /// that this upgrade is done here (and not in AST lowering) is because
    /// these upgrades are for inherently x86 reasons and might not be
    /// applicable to other backends.
    fn upgrade_roc(ctx: &mut BackendContext, val: &RegisterOrConst) -> String {
        if matches!(val, RegisterOrConst::Constant(_)) {
            write_asm!(
                ctx,
                "movl {}, %ebx  # upgraded in x86 backend",
                Self::roc(val)
            );

            "%ebx".to_owned()
        } else {
            Self::roc(val)
        }
    }

    fn write_unary_op(ctx: &mut BackendContext, op: UnaryOp, register: &Register) {
        let reg = Self::reg(register);
        match op {
            UnaryOp::Negation => write_asm!(ctx, "neg {reg}"),
            UnaryOp::BitwiseComplement => write_asm!(ctx, "not {reg}"),
            UnaryOp::LogicalNegation => {
                write_asm!(ctx, "cmpl $0, {reg}");
                write_asm!(ctx, "sete {}", Self::reg_low_8(register));
            }
        }
    }

    fn write_binary_op(ctx: &mut BackendContext, op: BinOp, lhs: &Register, rhs: &RegisterOrConst) {
        let lh = Self::reg(lhs);
        let rh = Self::roc(rhs);

        match op {
            BinOp::Add => write_asm!(ctx, "addl {rh}, {lh}"),
            BinOp::Sub => write_asm!(ctx, "subl {rh}, {lh}"),
            BinOp::Mul => write_asm!(ctx, "imul {rh}, {lh}"),
            BinOp::Div => {
                Self::write_division(ctx, lh, rhs);
                write_asm!(ctx, "movl %eax, {lh}");
            }
            BinOp::Modulo => {
                Self::write_division(ctx, lh, rhs);
                // same as division, we can just use EDX for the remainder.
                write_asm!(ctx, "movl %edx, {lh}");
            }

            op @ (BinOp::Equals
            | BinOp::NotEquals
            | BinOp::LessThan
            | BinOp::LessThanOrEquals
            | BinOp::GreaterThan
            | BinOp::GreaterThanOrEquals) => {
                write_asm!(ctx, "cmpl {rh}, {lh}");
                write_asm!(ctx, "movl $0, {lh}");
                write_asm!(
                    ctx,
                    "set{} {}",
                    Self::get_relational_instruction(op),
                    Self::reg_low_8(lhs)
                );
            }

            BinOp::LeftShift => write_asm!(ctx, "shll {}, {}", rh, lh),
            BinOp::RightShift => write_asm!(ctx, "shrl {}, {}", rh, lh),

            BinOp::BitwiseOr => write_asm!(ctx, "orl {}, {}", rh, lh),
            BinOp::BitwiseAnd => write_asm!(ctx, "andl {}, {}", rh, lh),
            BinOp::ExclusiveOr => write_asm!(ctx, "xorl {}, {}", rh, lh),

            BinOp::LogicalOr => panic!("`Instruction::BinOp(LogicalOr)` is not allowed, use the `ShortCircuit` and `BinaryBooleanOp` instructions instead."),
            BinOp::LogicalAnd => panic!("`Instruction::BinOp(LogicalAnd)` is not allowed, use the `ShortCircuit` and `BinaryBooleanOp` instructions instead."),
        }
    }

    fn get_relational_instruction(op: BinOp) -> &'static str {
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

    /// Write a division instruction into the [`BackendContext`].
    ///
    /// This is used by both [`BinOp::Modulo`] and [`BinOp::Div`].
    fn write_division(ctx: &mut BackendContext, lhs: &str, rhs: &RegisterOrConst) {
        // x86 division (`idiv`) requires the dividend to be in EDX:EAX, and
        // the divisor register is passed separately. This is fine as we
        // don't use EAX or EDX as allocatable registers.
        write_asm!(ctx, "movl {lhs}, %eax");
        write_asm!(ctx, "cdq");

        // x86 cannot divide by a constant value.
        // FIXME: we could do some crazy stuff to convert division by a
        //        constant into additions and bitshifts (etc).
        let rh = Self::upgrade_roc(ctx, rhs);
        write_asm!(ctx, "idiv {rh}");
    }
}
