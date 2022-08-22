use self::token::{Keyword, Literal, Token};
use crate::ctypes::CType;

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
            if token == Token::Illegal {
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

    fn peek_char(&self) -> char {
        if self.position >= self.input.len() {
            '\0'
        } else {
            self.input[self.position].into()
        }
    }

    pub fn read_token(&mut self) -> Token {
        // remove all leading whitespace
        while self.peek_char().is_whitespace() {
            self.read_char();
        }

        // read a single char that we will match against
        let ch = self.read_char();

        // matching identifers and keywords
        if ch.is_ascii_alphabetic() {
            let str = self.read_complex(|ch| ch.is_ascii_alphanumeric() || ch == '_');

            // keyword matching
            // TODO: make this system smarter (we could automatically do this, maybe with 'build.rs')
            let keyword = match str.as_str() {
                "return" => Some(Keyword::Return),
                "int" => Some(Keyword::DataType(CType::Integer)),
                "if" => Some(Keyword::If),
                "else" => Some(Keyword::Else),
                "for" => Some(Keyword::For),
                "while" => Some(Keyword::While),
                "do" => Some(Keyword::Do),
                "break" => Some(Keyword::Break),
                "continue" => Some(Keyword::Continue),
                _ => None,
            };

            // return the tokenized keyword
            if let Some(keyword) = keyword {
                return Token::Keyword(keyword);
            }

            // or return an identifer
            return Token::Identifier(str);
        }

        // matching integer literals
        if ch.is_ascii_digit() {
            let literal = self.read_complex(|ch| ch.is_ascii_digit());

            // parse and return literal
            return Token::Literal(Literal::Integer(literal.parse().unwrap())); // TODO: fail on integers that exceed i32::MAX
        }

        // matching multi-char tokens
        // TODO: need to look into making this better; we shouldn't use match
        let multi_char = match () {
            _ if self.read_multi_char("&&") => Token::AND,
            _ if self.read_multi_char("||") => Token::OR,
            _ if self.read_multi_char("==") => Token::Equal,
            _ if self.read_multi_char("!=") => Token::NotEqual,
            _ if self.read_multi_char("<=") => Token::LessThanEqual,
            _ if self.read_multi_char(">=") => Token::GreaterThanEqual,
            _ => Token::Illegal,
        };

        // return mult-char token if it exists
        if multi_char != Token::Illegal {
            return multi_char;
        }

        // matching single character tokens
        // TODO: make this better, maybe with 'build.rs'
        match ch {
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
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,
            '=' => Token::Assignment,
            ':' => Token::Colon,
            '?' => Token::QuestionMark,
            ',' => Token::Comma,
            _ => Token::Illegal,
        }
    }

    // TODO: merge with `read_complex`
    fn read_multi_char(&mut self, token_str: &str) -> bool {
        // iterate the str to match
        for (i, ch) in token_str.chars().enumerate() {
            // calculate position to read (note the -1 to offset the read on line 54)
            let read_pos = self.position + i - 1;

            // check that we have something to read and check it matches
            if read_pos >= self.input.len() || self.input[read_pos] != ch as u8 {
                return false;
            }
        }

        // update the read position
        // we could do this with `self.read_char` and `self.peek_char` but this is simpler and more efficent
        self.position += token_str.len() - 1;

        true
    }

    fn read_complex(&mut self, f: fn(char) -> bool) -> String {
        // record the starting location of our complex token
        // note that we take into account the character we read on line 54
        let start = self.position - 1;

        // read characters while the closure is true and we have stuff to read
        while self.position < self.input.len() && f(self.peek_char()) {
            self.position += 1;
        }

        // reconstruct the complex token
        self.input[start..self.position]
            .iter()
            .map(|ch| *ch as char)
            .collect::<String>()
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.read_token();

        if token == Token::Illegal {
            None
        } else {
            Some(token)
        }
    }
}
