use crate::ctypes::CInteger;

#[derive(Debug)]
pub enum Expression {
    Constant(CInteger),
    UnaryOp(UnaryOperator, Box<Expression>),
    BinaryOp(BinOperator, Box<Expression>, Box<Expression>),
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

    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,

    Equal,
    NotEqual,
    LogicalAND,
    LogicalOR,
}
