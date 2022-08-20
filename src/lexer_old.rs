// credit: https://mohitkarekar.com/posts/pl/lexer/

use crate::ctypes::DataType;

use self::token::{Keyword, Literal, Token};

pub mod token;

pub struct Lexer<'a> {
    input: &'a [u8],
    pub position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [u8]) -> Self {
        Self { input, position: 0 }
    }

    pub fn read_all(&mut self) -> Vec<Token> {
        let mut tokens = vec![];

        loop {
            let token = self.read_token();
            if token == Token::None {
                return tokens;
            }
            tokens.push(token);
        }
    }

    fn read_char(&mut self) -> char {
        let _char = if self.position >= self.input.len() {
            '\0'
        } else {
            self.input[self.position].into()
        };
        self.position += 1;
        _char
    }

    fn peek_char(&mut self) -> char {
        let _char = if self.position >= self.input.len() {
            '\0'
        } else {
            self.input[self.position].into()
        };
        _char
    }

    pub fn read_token(&mut self) -> Token {
        let read_u32_literal = |l: &mut Lexer| -> Option<u32> {
            let position = l.position;
            while l.position < l.input.len() && l.read_char().is_ascii_digit() {}
            l.input[(position - 1)..(l.position - 1)]
                .iter()
                .map(|ch| *ch as char)
                .collect::<String>()
                .parse::<u32>()
                .ok()
        };

        while self.peek_char().is_whitespace() {
            self.read_char();
        }

        let ch = self.peek_char();

        // single character tokens
        let token = match ch {
            '{' => Token::OpenBrace,
            '}' => Token::CloseBrace,
            '(' => Token::OpenParen,
            ')' => Token::CloseParen,
            ';' => Token::Semicolon,
            '-' => Token::Minus,
            '~' => Token::BitwiseComplement,
            '!' => Token::LogicalNegation,
            '+' => Token::Addition,
            '*' => Token::Multiplication,
            '/' => Token::Division,
            _ => Token::None,
        };

        if token != Token::None {
            self.read_char();
            return token;
        }

        match ch {
            'A'..='Z' | 'a'..='z' => {
                let ident = self.read_multi(|ch| ch.is_ascii_alphabetic());

                match ident.as_str() {
                    "return" => Token::Keyword(Keyword::Return),
                    "int" => Token::Keyword(Keyword::DataType(DataType::Integer)),
                    _ => Token::Identifier(ident),
                }
            }
            '0'..='9' => {
                let number = self
                    .read_multi(|ch| ch.is_ascii_digit())
                    .parse::<i32>()
                    .ok();

                if let Some(number) = number {
                    Token::Literal(Literal::Integer(number))
                } else {
                    Token::None
                }
            }
            _ => Token::None,
        }
    }

    fn read_multi(&mut self, f: fn(char) -> bool) -> String {
        let start = self.position;
        while self.position < self.input.len() && f(self.peek_char()) {
            self.position += 1;
        }
        self.input[start..self.position]
            .iter()
            .map(|ch| *ch as char)
            .collect::<String>()
    }
}
