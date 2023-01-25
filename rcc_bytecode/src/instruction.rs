use rcc_parser::ast::{Expression, Function, Statement, UnaryOp};

#[derive(Debug, Clone)]
pub enum Instruction {
    LoadInt(u32),
    Return,

    // unary ops
    Negate,
    BitwiseComplement,
    LogicalNegate,
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
            Expression::BinOp {
                has_parens,
                lhs,
                rhs,
                op,
            } => todo!(),
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
}
