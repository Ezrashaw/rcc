use std::{iter::Peekable, num::ParseIntError, path::Iter};

use log::{debug, trace};

use self::{
    error::{LexError, LexErrorKind},
    list::TokenList,
    token::{Span, Token, TokenKind},
};

pub mod error;
pub mod list;
pub mod token;

pub struct Lexer {
    input: Vec<u8>,
    position: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: Vec<u8>) -> Self {
        let first = *input.first().expect("Zero-length input invalid");
        Self {
            input,
            position: 0,
            ch: first,
        }
    }

    fn next_char(&mut self) -> Option<u8> {
        let ch = self.ch;
        self.position += 1;
        if self.position >= self.input.len() {
            None
        } else {
            self.ch = self.input[self.position];
            Some(ch)
        }
    }

    fn prev_char(&mut self) -> u8 {
        let ch = self.ch;
        self.position = self
            .position
            .checked_sub(1)
            .expect("internal error: tried to read beyond start of input");
        self.ch = self.input[self.position];
        ch
    }

    pub fn all_tokens(mut self) -> TokenList {
        let mut vec = Vec::new();

        loop {
            let token = self.next_token();
            if let Err(_) = &token {
                vec.push(token);
                break;
            }
            if unsafe { token.as_ref().unwrap_unchecked() }.kind() == &TokenKind::EOF {
                vec.push(token);
                break;
            }
            vec.push(token);
        }

        TokenList(vec)
    }

    pub fn next_token(&mut self) -> Result<Token, LexError> {
        let token = self.lex_token()?;
        debug!("Lexed token: {}", token);

        Ok(token)
    }

    fn lex_token(&mut self) -> Result<Token, LexError> {
        // strip whitespace
        while self.ch.is_ascii_whitespace() {
            let ch = self.next_char();
            if let Some(ch) = ch {
                ch
            } else {
                return Ok(Token::new(TokenKind::EOF, Span::new(self.position, 0)));
            };
        }

        let ch = self.next_char();
        let ch = if let Some(ch) = ch {
            ch
        } else {
            return Ok(Token::new(TokenKind::EOF, Span::new(self.position, 0)));
        };

        // match single character tokens
        let kind = match ch {
            b'{' => Some(TokenKind::OpenBrace),
            b'}' => Some(TokenKind::CloseBrace),
            b'(' => Some(TokenKind::OpenParen),
            b')' => Some(TokenKind::CloseParen),
            b';' => Some(TokenKind::Semicolon),
            _ => None,
        };

        if let Some(kind) = kind {
            let span = Span::new(self.position - 1, 1);

            return Ok(Token::new(kind, span));
        }

        self.prev_char();
        // match complex tokens
        Ok(match ch {
            c if c.is_ascii_digit() => self.lex_int_literal()?,
            c if c.is_ascii_alphabetic() => self.lex_ident()?,
            _ => Err(LexError::new(
                LexErrorKind::InvalidToken,
                Span::new(self.position, 1),
            ))?,
        })
    }

    fn lex_ident(&mut self) -> Result<Token, LexError> {
        let start = self.position;

        loop {
            if !(self.ch.is_ascii_alphanumeric() || self.ch == b'_') {
                break;
            }
            let ch = self.next_char();
            let ch = if let Some(ch) = ch {
                ch
            } else {
                return Ok(Token::new(TokenKind::EOF, Span::new(self.position, 0)));
            };
        }

        let span = Span::new(start, self.position - start);
        let kind =
            TokenKind::Ident(String::from_utf8(self.input[start..self.position].to_vec()).unwrap());

        Ok(Token::new(kind, span))
    }

    fn lex_int_literal(&mut self) -> Result<Token, LexError> {
        let start = self.position;

        loop {
            if !self.ch.is_ascii_digit() {
                break;
            }
            let ch = self.next_char();
            let ch = if let Some(ch) = ch {
                ch
            } else {
                return Ok(Token::new(TokenKind::EOF, Span::new(self.position, 0)));
            };
        }

        let span = Span::new(start, self.position - start);

        let str = std::str::from_utf8(&self.input[start..self.position])
            .expect("internal error: input wasn't ASCII/ UTF8");
        let num: Result<_, ParseIntError> = str.parse();

        match num {
            Err(err) => Err(LexError::new(err.into(), span)),
            Ok(num) => Ok(Token::new(TokenKind::IntLiteral(num), span)),
        }
    }
}
