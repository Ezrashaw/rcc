use rcc_parser::ast::{Expression, Function, Statement};
use rcc_structures::{BinOp, UnaryOp};

#[derive(Debug, Clone)]
pub enum Instruction {
    LoadInt(i32),
    Return,

    // unary ops
    Negate,
    BitwiseComplement,
    LogicalNegate,

    /// Push the value being operated on to the stack
    Push,
    /// Pop a value from the stack to the primary register (currently also moves primary to secondary first)
    Pop,

    BinaryOp(BinOp),
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
                Self::from_expression(buf, val);
                buf.push(Instruction::Return);
            }
        }
    }

    fn from_expression(buf: &mut Vec<Self>, expression: &Expression) {
        match expression {
            Expression::Literal { val } => buf.push(Instruction::LoadInt(*val)),
            Expression::UnaryOp { expr, op } => Self::from_unary_op(buf, expr, op),
            Expression::BinOp { lhs, rhs, op, .. } => Self::from_binary_op(buf, lhs, rhs, op),
        }
    }

    fn from_unary_op(buf: &mut Vec<Self>, inner: &Expression, op: &UnaryOp) {
        Self::from_expression(buf, inner);

        buf.push(match op {
            UnaryOp::Negation => Instruction::Negate,
            UnaryOp::BitwiseComplement => Instruction::BitwiseComplement,
            UnaryOp::LogicalNegation => Instruction::LogicalNegate,
        })
    }

    fn from_binary_op(buf: &mut Vec<Self>, lhs: &Expression, rhs: &Expression, op: &BinOp) {
        Self::from_expression(buf, lhs);

        // save lhs, rhs will overwrite the primary register
        buf.push(Instruction::Push);

        Self::from_expression(buf, rhs);

        // load lhs into secondary
        buf.push(Instruction::Pop);

        buf.push(Instruction::BinaryOp(*op))
    }
}
