use std::fmt::{self, Display};

pub struct Token {
    kind: TokenKind,
    span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} at {}", self.kind, self.span)
    }
}

pub struct Span {
    pos: usize,
    length: usize,
}

impl Span {
    pub fn new(pos: usize, length: usize) -> Self {
        Self { pos, length }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.pos, self.pos + self.length)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    Semicolon,
    Ident(String),
    IntLiteral(u32),

    EOF,
}
