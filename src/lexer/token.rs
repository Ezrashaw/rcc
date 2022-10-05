use std::path::PathBuf;

use crate::ctypes::{CInteger, CType};

pub struct Token {
    pub data: TokenData,

    pub file: String,
    pub line: u32,
    pub column: u32,
}

impl Token {
    pub fn new(data: TokenData, file: String, line: u32, column: u32) -> Self {
        Self {
            data,
            file,
            line,
            column,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenData {
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
