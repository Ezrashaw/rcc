use std::{
    fmt::{self, Display},
    num::ParseIntError,
};

use super::token::Span;

pub struct LexError {
    kind: LexErrorKind,
    span: Span,
}

impl LexError {
    pub fn new(kind: LexErrorKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &LexErrorKind {
        &self.kind
    }
}

impl Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Lexer error: {:?} at {}", self.kind, self.span)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum LexErrorKind {
    InvalidIntLiteral(ParseIntError),
    InvalidToken,
}

impl From<ParseIntError> for LexErrorKind {
    fn from(value: ParseIntError) -> Self {
        Self::InvalidIntLiteral(value)
    }
}
