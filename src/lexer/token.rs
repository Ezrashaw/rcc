use crate::ctypes::{CInteger, CType};

#[derive(Debug, Clone, PartialEq, Eq)]
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

    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,

    Assignment,

    Colon,
    QuestionMark,

    Comma,

    Keyword(Keyword),
    Identifier(String),
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Return,
    DataType(CType),

    If,
    Else,

    For,
    While,
    Do,
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Integer(CInteger),
}
