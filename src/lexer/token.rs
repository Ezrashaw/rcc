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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(CInteger),
}
