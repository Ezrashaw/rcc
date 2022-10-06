use crate::{
    ctypes::CType,
    error::{CompileError, CompileErrorKind},
    lexer::token::{Token, TokenKind},
};

use super::Parser;

macro_rules! unwrap_token {
    ($found:expr, TokenKind::$expected:ident) => {{
        let __eval = $found;
        if let $crate::lexer::token::TokenKind::$expected(__unwrapped) = __eval.kind {
            Ok(__unwrapped)
        } else {
            Err($crate::error::CompileError::new(
                $crate::error::CompileErrorKind::ExpectedXButFoundY {
                    expected: stringify!($expected),
                    found: stringify!(__eval),
                },
            ))
        }
    }};
}

#[macro_export]
macro_rules! expect_token {
    ($self:expr, $expected:pat) => {{
        let __eval = $self.read_token();
        match __eval {
            $expected => Ok(()),
            _ => Err($crate::error::CompileError::new(
                $crate::error::CompileErrorKind::ExpectedXButFoundY {
                    expected: stringify!($expected),
                    found: stringify!(__eval),
                },
            )),
        }
    }};
}

#[macro_export]
macro_rules! expect_token_soft {
    ($self:expr, $expected:pat) => {{
        match $self.peek_token() {
            $expected => {
                $self.read_token();
                true
            }
            _ => false,
        }
    }};
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub(super) fn read_token(&mut self) -> Result<TokenKind, CompileError> {
        Ok(self
            .input
            .next()
            .ok_or(CompileError::new(CompileErrorKind::EOFReached))?
            .kind)
    }

    pub(super) fn peek_token(&mut self) -> Result<&TokenKind, CompileError> {
        self.peek_far_token(1)
    }

    pub(super) fn peek_far_token(&mut self, d: usize) -> Result<&TokenKind, CompileError> {
        Ok(&self
            .input
            .peek_far(d)
            .ok_or(CompileError::new(CompileErrorKind::EOFReached))?
            .kind)
    }

    pub(super) fn read_identifier(&mut self) -> Result<String, CompileError> {
        let id = self
            .input
            .next()
            .ok_or(CompileError::new(CompileErrorKind::EOFReached))?; // TODO: actually use `self.read_token()`

        unwrap_token!(id, TokenKind::Identifier)
    }

    pub(super) fn read_type(&mut self) -> Result<CType, CompileError> {
        let id = self
            .input
            .next()
            .ok_or(CompileError::new(CompileErrorKind::EOFReached))?; // TODO: actually use `self.read_token()`

        unwrap_token!(id, TokenKind::Keyword_DataType)
    }
}
