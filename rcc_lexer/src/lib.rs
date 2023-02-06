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
    Colon,
    Comma,

    // operators
    Minus,
    Tilde,
    Exclamation,
    Plus,
    Star,
    Slash,
    Equals,
    QuestionMark,
    Percent,
    LeftArrow,
    RightArrow,
    Pipe,
    Caret,
    And,

    DoubleAnd,
    DoublePipe,
    DoubleEquals,
    ExclaimEquals,
    LeftArrowEquals,
    RightArrowEquals,
    DoubleLeftArrow,
    DoubleRightArrow,

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

    // control flow
    Return,
    If,
    Else,
    For,
    While,
    Do,
    Break,
    Continue,
}

const_tokens! {
    // general syntax
    "{" => TokenKind::OpenBrace,
    "}" => TokenKind::CloseBrace,
    "(" => TokenKind::OpenParen,
    ")" => TokenKind::CloseParen,
    ";" => TokenKind::Semicolon,
    ":" => TokenKind::Colon,
    "," => TokenKind::Comma,

    // operators
    "-" => TokenKind::Minus,
    "~" => TokenKind::Tilde,
    "+" => TokenKind::Plus,
    "*" => TokenKind::Star,
    "/" => TokenKind::Slash,
    "?" => TokenKind::QuestionMark,
    "%" => TokenKind::Percent,
    "^" => TokenKind::Caret,

    // double-char operators
    "&&" => TokenKind::DoubleAnd,
    "||" => TokenKind::DoublePipe,
    "==" => TokenKind::DoubleEquals,
    "!=" => TokenKind::ExclaimEquals,
    "<=" => TokenKind::LeftArrowEquals,
    ">=" => TokenKind::RightArrowEquals,
    "<<" => TokenKind::DoubleLeftArrow,
    ">>" => TokenKind::DoubleRightArrow,

    // placed here for lower prio
    "!" => TokenKind::Exclamation,
    "<" => TokenKind::LeftArrow,
    ">" => TokenKind::RightArrow,
    "=" => TokenKind::Equals,
    "&" => TokenKind::And,
    "|" => TokenKind::Pipe,
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
                "if" => TokenKind::Keyword(Keyword::If),
                "else" => TokenKind::Keyword(Keyword::Else),
                "for" => TokenKind::Keyword(Keyword::For),
                "while" => TokenKind::Keyword(Keyword::While),
                "do" => TokenKind::Keyword(Keyword::Do),
                "break" => TokenKind::Keyword(Keyword::Break),
                "continue" => TokenKind::Keyword(Keyword::Continue),

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
                SpannedError::with_span("Failed to lex integer literal".to_string(), span).emit()
            };
            TokenKind::Literal(lit)
        }
    },
}
