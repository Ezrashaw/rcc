use crate::{
    ctypes::CType,
    lexer::token::{Token, TokenKind},
};

use super::Parser;

macro_rules! unwrap_token {
    ($found:expr, TokenKind::$expected:ident) => {{
        let __eval = $found;
        if let $crate::lexer::token::TokenKind::$expected(__unwrapped) = __eval.kind {
            // Ok(__unwrapped)
            __unwrapped
        } else {
            // Err(CompileError::new(
            //     CompileErrorKind::ExpectedTokenButFoundToken {
            //         expected: stringify!($expected),
            //         found: __eval,
            //     },
            // ))
            panic!("unexpected token");
        }
    }};
}

#[macro_export]
macro_rules! expect_token {
    ($self:expr, $expected:pat) => {{
        let __eval = $self.read_token();
        match __eval {
            // Ok(())
            $expected => (),

            // Err(CompileError::new(
            //     CompileErrorKind::ExpectedTokenButFoundToken {
            //         expected: stringify!($expected),
            //         found: stringify!(__eval),
            //     },
            // ))
            _ => panic!("unexpected token"),
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
    pub(super) fn read_token(&mut self) -> TokenKind {
        self.input.next().unwrap().kind
    }

    pub(super) fn peek_token(&mut self) -> &TokenKind {
        &self.input.peek().unwrap().kind // TODO: merge with `peek_far_token`?
    }

    pub(super) fn peek_far_token(&mut self, d: usize) -> &TokenKind {
        &self.input.peek_far(d).unwrap().kind
    }

    pub(super) fn read_identifier(&mut self) -> String {
        unwrap_token!(self.input.next().unwrap(), TokenKind::Identifier)
    }

    pub(super) fn read_type(&mut self) -> CType {
        unwrap_token!(self.input.next().unwrap(), TokenKind::Keyword_DataType)
    }
}
