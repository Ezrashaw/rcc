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
    // general syntax
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    Semicolon,

    // operators
    Minus,
    Tilde,
    Exclamation,
    Plus,
    Star,
    Slash,
    Equals,

    DoubleAnd,
    DoublePipe,
    DoubleEquals,
    ExclaimEquals,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,

    // complex
    Literal(u32),
    Ident(&'a str),
    Keyword(Keyword),
}

/// Defines keywords (including builtin types) as defined by `C`.
#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    // types
    Int,

    Return,
}

const_tokens! {
    // general syntax
    "{" => TokenKind::OpenBrace,
    "}" => TokenKind::CloseBrace,
    "(" => TokenKind::OpenParen,
    ")" => TokenKind::CloseParen,
    ";" => TokenKind::Semicolon,

    // operators
    "-" => TokenKind::Minus,
    "~" => TokenKind::Tilde,
    "+" => TokenKind::Plus,
    "*" => TokenKind::Star,
    "/" => TokenKind::Slash,

    // double-char operators
    "&&" => TokenKind::DoubleAnd,
    "||" => TokenKind::DoublePipe,
    "==" => TokenKind::DoubleEquals,
    "!=" => TokenKind::ExclaimEquals,
    "<=" => TokenKind::LessThanEquals,
    ">=" => TokenKind::GreaterThanEquals,

    // placed here for lower prio
    "!" => TokenKind::Exclamation,
    "<" => TokenKind::LessThan,
    ">" => TokenKind::GreaterThan,
    "=" => TokenKind::Equals,
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
