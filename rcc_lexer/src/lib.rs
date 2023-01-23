#![feature(let_chains)]

mod common;
pub use common::*;

use rcc_error::SpannedError;
use rcc_span::Span;

// `C` specific stuff

/// Core type for `C` tokens.
///
/// Defines all the tokens that can occur in a valid `C` program. Holds a reference to the input string for identifiers.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind<'a> {
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    Semicolon,

    Keyword(Keyword),

    Literal(u32),
    Ident(&'a str),
}

/// Defines keywords (including builtin types) as defined by `C`.
#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    // types
    Int,

    Return,
}

single_char_tokens! {
    '{' => TokenKind::OpenBrace,
    '}' => TokenKind::CloseBrace,
    '(' => TokenKind::OpenParen,
    ')' => TokenKind::CloseParen,
    ';' => TokenKind::Semicolon,
}

multi_char_tokens! {
    // identifiers/ keywords
    {
        start: |ch: char| ch.is_ascii_alphabetic(),
        continue: |ch: char| ch.is_ascii_alphanumeric() || ch == '_',
        final: |slice, _| {
            match slice {
                "int" => TokenKind::Keyword(Keyword::Int),
                "return" => TokenKind::Keyword(Keyword::Return),

                _ => TokenKind::Ident(slice)
            }
        }
    },

    // integer literals
    {
        start: |ch: char| ch.is_ascii_digit(),
        continue: |ch: char| ch.is_ascii_digit(),
        final: |slice: &str, span: Span| {
            let Ok(lit) = slice.parse() else {
                SpannedError::with_span(format!("Failed to lex integer literal"), span).emit()
            };
            TokenKind::Literal(lit)
        }
    },
}
