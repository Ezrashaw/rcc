use crate::ctypes::{CInteger, CType};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Illegal,
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    Semicolon,

    Minus,
    BitwiseComplement,
    LogicalNegation,

    Addition,
    Multiplication,
    Division,

    AND,
    OR,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,

    Keyword(Keyword),
    Identifier(String),
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Return,
    DataType(CType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(CInteger),
}
