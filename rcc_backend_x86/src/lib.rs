use core::panic;
use rcc_backend_traits::{write_asm, write_asm_no_indent, Backend, BackendContext};
use rcc_bytecode::{Instruction, ReadLocation, WriteLocation};
use rcc_structures::{BinOp, UnaryOp};
use register::Register;
use std::fmt::Write;

mod register;

pub struct X86Backend;

impl Backend for X86Backend {
    fn write_function(&mut self, ctx: &mut BackendContext, fn_name: &str) {
        // FIXME: can we make this generic for all backends?
        if std::env::consts::OS == "macos" {
            write_asm!(ctx, ".globl _{fn_name}\n_{fn_name}:");
        } else {
            write_asm!(ctx, ".globl {fn_name}\n{fn_name}:");
        }

        ctx.increment_indent();

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
                write_asm!(ctx, "movl {}, {}", Self::rl(from), Self::wl(to))
            }

            Instruction::Return(val) => {
                write_asm!(ctx, "movl {}, %eax", Self::rl(val));
                write_asm!(
                    ctx,
                    "pop %rbp # restore old RBP; now RSP is where it was before prologue"
                );
                write_asm!(ctx, "ret");
            }
            Instruction::CompareJump(val, should_jump, jump_loc) => {
                write_asm!(ctx, "cmpl $0, {} # check if expr is true", Self::wl(val));
                write_asm!(
                    ctx,
                    "j{} _{jump_loc}  # if so, jump",
                    if *should_jump { "ne" } else { "e" }
                );
            }

            Instruction::BinaryBooleanOp(wl, jump_loc) => {
                write_asm!(ctx, "cmpl $0, {}  # check if e2 is true", Self::wl(wl));
                write_asm!(ctx, "setne %{}", Register::from_u8(wl.reg()).get_low_8());
                write_asm_no_indent!(ctx, "_{jump_loc}: # short-circuit jump label");
            }

            Instruction::AssignVariable(var, rl) => {
                write_asm!(ctx, "movl {}, {}", Self::rl(rl), Self::var(var));
            }
            Instruction::LoadVariable(var, wl) => {
                write_asm!(ctx, "movl {}, {}", Self::var(var), Self::wl(wl));
            }

            Instruction::IfThen(false_branch, rl) => {
                write_asm!(ctx, "cmpl $0, {}", Self::rl(rl));
                write_asm!(ctx, "je _{false_branch}");
            }

            Instruction::PostConditional(post_else, pre_else) => {
                write_asm!(ctx, "jmp _{post_else}");
                write_asm_no_indent!(ctx, "_{pre_else}:");
            }

            Instruction::JumpDummy(loc) => write_asm_no_indent!(ctx, "_{loc}:"),
            Instruction::UnconditionalJump(loc) => write_asm!(ctx, "jmp _{loc}"),

            Instruction::BinaryOp(op, lhs, rhs) => Self::write_binary_op(ctx, *op, lhs, rhs),
            Instruction::UnaryOp(op, wl) => Self::write_unary_op(ctx, *op, wl),
        }
    }
}

impl X86Backend {
    fn wl(loc: &WriteLocation) -> String {
        format!("%{}", Register::from_u8(loc.reg()))
    }

    fn rl(loc: &ReadLocation) -> String {
        match loc {
            ReadLocation::Writable(wloc) => Self::wl(wloc),
            ReadLocation::Constant(val) => format!("${val}"),
        }
    }

    fn var(id: &u32) -> String {
        format!("-{}(%rbp)", (id + 1) * 4)
    }

    fn write_unary_op(ctx: &mut BackendContext, op: UnaryOp, loc: &WriteLocation) {
        let wl = Self::wl(loc);
        match op {
            UnaryOp::Negation => write_asm!(ctx, "neg {wl}"),
            UnaryOp::BitwiseComplement => write_asm!(ctx, "not {wl}"),
            UnaryOp::LogicalNegation => {
                write_asm!(ctx, "cmpl   $0, {wl}");
                write_asm!(ctx, "sete %{}", Register::from_u8(loc.reg()).get_low_8());
            }
        }
    }

    fn write_binary_op(
        ctx: &mut BackendContext,
        op: BinOp,
        lhs: &WriteLocation,
        rhs: &ReadLocation,
    ) {
        let lh = Self::wl(lhs);
        let rh = Self::rl(rhs);

        match op {
            BinOp::Add => write_asm!(ctx, "addl {rh}, {lh}"),
            BinOp::Sub => write_asm!(ctx, "subl {rh}, {lh}"),
            BinOp::Mul => write_asm!(ctx, "imul {rh}, {lh}"),
            BinOp::Div => {
                // x86 division (`idiv`) requires the dividend to be in EDX:EAX, and
                // the divisor register is passed separately. This is fine as we
                // don't use EAX or EDX as allocatable registers.
                write_asm!(ctx, "movl {lh}, %eax");
                write_asm!(ctx, "cdq");
                write_asm!(ctx, "idiv {rh}");
                write_asm!(ctx, "movl %eax, {lh}");
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
                    "set{} %{}",
                    Self::get_relational_instruction(op),
                    Register::from_u8(lhs.reg()).get_low_8()
                )
            }

            BinOp::LogicalOr => todo!(),
            BinOp::LogicalAnd => todo!(),
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
}
