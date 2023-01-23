use rcc_error::SpannedError;
use rcc_span::Span;

use crate::TokenKind;

/// A lexer for the `C` programming language.
///
/// Operates on a string slice, generated tokens may hold references to this input.
#[derive(Clone)]
pub struct Lexer<'a> {
    pub(crate) input: &'a str,
    pub(crate) pos: u32,
}

impl<'a> Lexer<'a> {
    /// Creates a new [`Lexer`] with the specified input.
    #[must_use]
    pub fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    pub(crate) fn next(&mut self) -> Option<char> {
        if self.pos as usize == self.input.len() {
            return None;
        }

        let ch: u8 = self.input.as_bytes()[self.pos as usize];
        self.pos += 1;

        Some(ch.into())
    }

    pub(crate) fn peek(&mut self) -> Option<char> {
        if self.pos as usize == self.input.len() {
            None
        } else {
            Some(self.input.as_bytes()[self.pos as usize].into())
        }
    }

    fn lex_token(&mut self) -> Option<(u32, TokenKind<'a>)> {
        while self.peek()?.is_ascii_whitespace() {
            self.next();
        }

        let start = self.pos;

        let ch = self.peek()?;

        // single character tokens
        if let Some(kind) = Self::match_single_char_tokens(ch) {
            self.next();
            return Some((start, kind));
        }

        // Multiple character tokens
        if let Some(tok) = self.match_multi_char_tokens(ch) {
            return Some((start, tok));
        }

        SpannedError::with_span(
            "unknown start of token",
            Span::new(start, self.pos, self.input),
        )
        .emit();
    }
}

macro_rules! single_char_tokens {
    ( $( $ch:literal => $tok:expr, )+ ) => {
        impl<'a> crate::common::Lexer<'a> {
            fn match_single_char_tokens(ch: char) -> Option<crate::TokenKind<'static>> {
                match ch {
                    $($ch => Some($tok),)+
                    _ => None,
                }
            }
        }
    };
}

/// Lex multi-char (or dynamic) tokens. Each entry must contain three closures:
///
/// - `start`: `fn(char) -> bool`: could a given character start this token?
/// - `continue`: `fn(char) -> bool`: could a given character continue this token?
// FIXME: use `Result<TokenKind, SpannedError>` here instead of `Option<TokenKind>`
/// - `final`: `fn(&str) -> Option<TokenKind>`: With the token built up from the loop, generate a token.
///
/// Tokens are attempted in the order in which they appear in the macro.
macro_rules! multi_char_tokens {
    ( $({ start: $start:expr, continue: $continue:expr, final: $final:expr },)+ ) => {
        impl<'a> crate::common::Lexer<'a> {
            fn match_multi_char_tokens(&mut self, peeked: char) -> Option<TokenKind<'a>> {
                match peeked {
                    $(ch if $start(ch) => {
                        let start = self.pos;
                        // consume the initial character
                        self.next();

                        while let Some(peeked) = self.peek() && $continue(peeked) {
                            self.next();
                        }

                        let span = Span::new(start, self.pos, self.input);
                        let slice = span.subsliced_str();

                        Some($final(slice, span))
                    })+
                    _ => None,
                }
            }
        }
    };
}

pub(crate) use {multi_char_tokens, single_char_tokens};

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    /// Lexes and returns the next token from the input string.
    ///
    /// Returns [`None`] if EOF has been reached.
    fn next(&mut self) -> Option<Self::Item> {
        self.lex_token().map(|(start, tk)| Token {
            kind: tk,
            span: Span::new(start, self.pos, self.input),
        })
    }
}

/// [`TokenKind`] wrapper.
///
/// Complements a [`TokenKind`] with a span.  
#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span<'a>,
}
