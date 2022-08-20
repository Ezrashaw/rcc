use crate::ctypes::CInteger;

#[derive(Debug)]
pub struct Expression {
    pub term: Term,
    pub nodes: Vec<(BinOperator, Term)>,
}

#[derive(Debug)]
pub struct Term {
    pub factor: Factor,
    pub nodes: Vec<(BinOperator, Factor)>,
}

#[derive(Debug)]
pub enum Factor {
    Expression(Box<Expression>), // wrapped in parens
    UnaryOp {
        operator: UnaryOperator,
        factor: Box<Factor>,
    },
    Constant(CInteger),
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negation,
    BitwiseComplement,
    LogicalNegation,
}

#[derive(Debug, PartialEq)]
pub enum BinOperator {
    Subtraction,
    Addition,
    Multiplication,
    Division,
}
