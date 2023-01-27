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

    /// Returns the value from the first register.
    Return,

    /// Applies the given binary operation to the two registers.
    ///
    /// Note that the first register is the LHS and the second the RHS.
    /// The result is stored in the LHS register.
    ///
    /// IMPORTANT: binary logical boolean operations are not implemented with
    /// this instruction, use [`Instruction::ShortCircuit`] and
    /// [`Instruction::BinaryBooleanOp`] instead.
    BinaryOp(BinOp, u8, u8),

    /// Applies the given unary operation to the specified register.
    UnaryOp(UnaryOp, u8),

    /// Short-circuit a logical boolean binary operation:
    ///
    /// If `bool` is true, then short-circuit when reg is true (logical or).
    /// If `bool` is false, then short-circuit when reg is false (logical and).
    ///
    /// The second `u32` points to the end of the second sub-expression (short-circuit).
    ShortCircuit(u8, bool, u32),

    /// Normalize the second clause of a logical boolean binary operation
    /// (i.e. make all non-zero = 1)
    ///
    /// Also provides location for [`Instruction::ShortCircuit`] to jump to.
    BinaryBooleanOp(u8, u32),
}

impl Instruction {
    pub(crate) fn from_function(function: &Function, label_counter: &mut u32) -> Vec<Self> {
        let mut buf = Vec::new();

        Self::from_statement(&mut buf, &function.statements[0], label_counter);

        buf
    }

    fn from_statement(buf: &mut Vec<Self>, statement: &Statement, label_counter: &mut u32) {
        match statement {
            Statement::Return(val) => {
                Self::from_expression(buf, val, 0, label_counter);
                buf.push(Instruction::Return);
            }
            Statement::Declaration(_, _) => todo!(),
            Statement::Expression(_) => todo!(),
        }
    }

    fn from_expression(
        buf: &mut Vec<Self>,
        expression: &Expression,
        reg: u8,
        label_counter: &mut u32,
    ) {
        match expression {
            Expression::Literal { val } => buf.push(Instruction::LoadInt(*val, reg)),
            Expression::UnaryOp { expr, op } => {
                Self::from_unary_op(buf, expr, op, reg, label_counter)
            }
            Expression::BinOp { lhs, rhs, op, .. } => {
                Self::from_binary_op(buf, lhs, rhs, op, reg, label_counter)
            }
            Expression::Assignment {
                identifier,
                expression,
            } => todo!(),
            Expression::Variable { identifier } => todo!(),
        }
    }

    fn from_unary_op(
        buf: &mut Vec<Self>,
        inner: &Expression,
        op: &UnaryOp,
        reg: u8,
        label_counter: &mut u32,
    ) {
        Self::from_expression(buf, inner, reg, label_counter);

        buf.push(Instruction::UnaryOp(*op, reg));
    }

    fn from_binary_op(
        buf: &mut Vec<Self>,
        lhs: &Expression,
        rhs: &Expression,
        op: &BinOp,
        reg: u8,
        label_counter: &mut u32,
    ) {
        // FIXME: this always works (see rhs), but we should have a mechanism to alloc/dealloc regs, producing the same result with better simplicity.
        Self::from_expression(buf, lhs, reg, label_counter);

        if let BinOp::LogicalAnd | BinOp::LogicalOr = op {
            let label = *label_counter;
            *label_counter += 1;

            buf.push(Instruction::ShortCircuit(
                reg,
                *op == BinOp::LogicalOr,
                label,
            ));

            Self::from_expression(buf, rhs, reg, label_counter);
            buf.push(Instruction::BinaryBooleanOp(reg, label));
        } else {
            Self::from_expression(buf, rhs, reg + 1, label_counter);
            buf.push(Instruction::BinaryOp(*op, reg, reg + 1));
        }
    }
}
