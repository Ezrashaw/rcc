use crate::ctypes::CInteger;

#[derive(Debug)]
pub enum Expression {
    Constant(CInteger),
    UnaryOp(UnaryOperator, Box<Expression>),
    BinaryOp(BinOperator, Box<Expression>, Box<Expression>),
    Assign(String, Box<Expression>),
    Variable(String),
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negation,
    BitwiseComplement,
    LogicalNegation,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinOperator {
    Subtraction,
    Addition,
    Multiplication,
    Division,

    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,

    Equal,
    NotEqual,
    LogicalAND,
    LogicalOR,
}
