use rcc_error::SpannedError;
use rcc_lexer::{Token, TokenKind};

use crate::Parser;

impl<'a, I: Iterator<Item = Token<'a>>> Parser<'a, I> {
    /// Peeks the next token from the input stream.
    ///
    /// If no more tokens can be read, then an error is emitted.
    pub(crate) fn peek_kind(&mut self) -> &TokenKind {
        // FIXME: we need to span this
        self.input.peek().map_or_else(
            || SpannedError::without_span("unexpected EOF").emit(),
            |token| &token.kind,
        )
    }

    /// Peeks the nth token from the input stream.
    ///
    /// If the nth token doesn't, then an error is emitted.
    pub(crate) fn peek_nth_kind(&mut self, nth: usize) -> &TokenKind {
        // FIXME: we need to span this
        self.input.peek_nth(nth).map_or_else(
            || SpannedError::without_span("unexpected EOF").emit(),
            |token| &token.kind,
        )
    }

    /// Gets the next token from the input stream.
    ///
    /// If no more tokens can be read, then an error is emitted.
    pub(crate) fn next_kind(&mut self) -> TokenKind<'a> {
        // FIXME: we need to span this
        self.input.next().map_or_else(
            || SpannedError::without_span("unexpected EOF").emit(),
            |token| token.kind,
        )
    }

    /// Drops the next token from the input stream.
    pub(crate) fn drop_next(&mut self) {
        self.input.next();
    }

    pub(crate) fn expect_ident(&mut self) -> &'a str {
        let TokenKind::Ident(ident) = self.next_kind() else {
            SpannedError::without_span("expected identifier").emit()
        };

        ident
    }

    pub(crate) fn expect_variable(&mut self) -> u32 {
        let ident = self.expect_ident();

        self.get_variable_id(ident)
    }

    pub(crate) fn get_variable_id(&mut self, ident: &str) -> u32 {
        // FIXME: make this more functional
        let mut position = self.scopes.iter().flatten().count() as u32 - 1;
        for search in self
            .scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.iter().rev())
        {
            if ident == *search {
                return position;
            }

            position -= 1;
        }

        SpannedError::without_span("undefined variable").emit();
    }

    pub(crate) fn define_variable(&mut self, ident: &'a str) -> u32 {
        let current_scope = self.scopes.last_mut().unwrap();

        if current_scope.contains(&ident) {
            SpannedError::without_span("variable already defined").emit()
        }

        current_scope.push(ident);

        self.scopes.iter().flatten().count() as u32 - 1
    }

    pub(crate) fn expect_token(&mut self, expected_kind: TokenKind) {
        if self.peek_kind() == &expected_kind {
            self.drop_next();
        } else {
            SpannedError::without_span(format!("`{expected_kind:?}`")).emit();
        }
    }

    pub(crate) fn emit_err_from_token(expected: &str, token: Option<Token>) -> ! {
        if let Some(token) = token {
            SpannedError::with_span(
                format!("expected {expected}, found `{:?}`", token.kind),
                token.span,
            )
            .emit()
        } else {
            SpannedError::without_span(format!("expected {expected}, found EOF")).emit()
        }
    }
}

#[macro_export]
macro_rules! maybe_next {
    ($parser:expr, $pattern:pat) => {{
        if matches!($parser.peek_kind(), $pattern) {
            $parser.drop_next();

            true
        } else {
            false
        }
    }};
}

#[macro_export]
macro_rules! maybe_peek {
    ($parser:expr, $pattern:pat) => {{
        matches!($parser.peek_kind(), $pattern)
    }};
}

#[macro_export]
macro_rules! maybe_peek_nth {
    ($parser:expr, $nth:literal, $pattern:pat) => {{
        matches!($parser.peek_nth_kind($nth), $pattern)
    }};
}
