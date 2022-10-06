use crate::ctypes::{CInteger, CType};

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,

    pub file: String, // TODO: make this a path
    pub line: u32,
    pub column: u32,
}

impl Token {
    pub fn new(kind: TokenKind, file: String, line: u32, column: u32) -> Self {
        Self {
            kind,
            file,
            line,
            column,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum TokenKind {
    Illegal, // TODO: marked for deletion
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

    Keyword_Return,
    Keyword_DataType(CType),
    Keyword_If,
    Keyword_Else,
    Keyword_For,
    Keyword_While,
    Keyword_Do,
    Keyword_Break,
    Keyword_Continue,

    Literal_Integer(CInteger),

    Identifier(String),
}
