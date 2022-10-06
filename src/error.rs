use crate::lexer::token::{Token, TokenKind};

pub struct CompileError {
    kind: CompileErrorKind,
    // positioning?
}

impl CompileError {
    pub fn new(kind: CompileErrorKind) -> Self {
        Self { kind }
    }
}

pub enum CompileErrorKind {
    ExpectedXButFoundY {
        expected: &'static str,
        found: &'static str,
    },
    EOFReached,
}
