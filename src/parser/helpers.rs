use crate::{
    ctypes::CType,
    lexer::token::{Token, TokenKind},
};

use super::Parser;

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

    pub(super) fn read_ident(&mut self) -> String {
        if let TokenKind::Identifier(ident) = self.read_token() {
            ident
        } else {
            panic!("Expected identifer but found");
        }
    }

    pub(super) fn read_type(&mut self) -> CType {
        if let TokenKind::Keyword_DataType(data_type) = self.read_token() {
            data_type
        } else {
            panic!("Expected type but found");
        }
    }
}
