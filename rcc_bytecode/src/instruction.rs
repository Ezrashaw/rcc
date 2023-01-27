use rcc_parser::ast::{Expression, Function, Statement};
use rcc_structures::{BinOp, UnaryOp};

/// Bytecode instruction used by `rcc` internally.
///
/// It targets a register machine (currently only 32-bit signed registers),
/// with an "infinite" number of registers.
/// (the x86 backend overflows registers to the stack)
#[derive(Debug, Clone)]
pub enum Instruction {
    /// Loads a constant integer into the given register.
    LoadInt(i32, u8),

    /// Returns the value from register 0, `EAX` on x86.
    Return,

    /// Applies the given binary operation to the two registers.
    ///
    /// Note that the first register is the LHS and the second the RHS.
    /// The result is stored in the LHS register.
    BinaryOp(BinOp, u8, u8),

    /// Applies the given unary operation to the specified register.
    UnaryOp(UnaryOp, u8),
}

impl Instruction {
    pub(crate) fn from_function(function: &Function) -> Vec<Self> {
        let mut buf = Vec::new();

        Self::from_statement(&mut buf, &function.statement);

        buf
    }

    fn from_statement(buf: &mut Vec<Self>, statement: &Statement) {
        match statement {
            Statement::Return(val) => {
                Self::from_expression(buf, val, 0);
                buf.push(Instruction::Return);
            }
        }
    }

    fn from_expression(buf: &mut Vec<Self>, expression: &Expression, reg: u8) {
        match expression {
            Expression::Literal { val } => buf.push(Instruction::LoadInt(*val, reg)),
            Expression::UnaryOp { expr, op } => Self::from_unary_op(buf, expr, op, reg),
            Expression::BinOp { lhs, rhs, op, .. } => Self::from_binary_op(buf, lhs, rhs, op, reg),
        }
    }

    fn from_unary_op(buf: &mut Vec<Self>, inner: &Expression, op: &UnaryOp, reg: u8) {
        Self::from_expression(buf, inner, reg);

        buf.push(Instruction::UnaryOp(*op, reg))
    }

    fn from_binary_op(
        buf: &mut Vec<Self>,
        lhs: &Expression,
        rhs: &Expression,
        op: &BinOp,
        reg: u8,
    ) {
        // FIXME: this always works, but we should have a mechanism to alloc/dealloc regs, producing the same result with better simplicity.
        Self::from_expression(buf, lhs, reg);
        Self::from_expression(buf, rhs, reg + 1);

        buf.push(Instruction::BinaryOp(*op, reg, reg + 1))
    }
}
