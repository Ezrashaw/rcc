use self::token::{Keyword, Literal, Token};
use crate::ctypes::CType;

pub mod token;

// A Lexer, converts files into C tokens
pub struct Lexer<'a> {
    input: &'a [u8],
    pub position: usize,
}

impl<'a> Lexer<'a> {
    // creates a new `Lexer` from a string reference as byte slice
    pub fn new(input: &'a [u8]) -> Self {
        Self { input, position: 0 }
    }

    // TODO: use the iterator instead
    // reads all of the input and returns the tokens
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

    // pops a character from the input
    fn read_char(&mut self) -> char {
        self.position += 1;
        self.input[self.position - 1].into()
    }

    // peeks a character from the input
    fn peek_char(&self) -> char {
        self.input[self.position].into()
    }

    pub fn read_token(&mut self) -> Token {
        // remove all leading whitespace
        while self.peek_char().is_whitespace() {
            self.read_char();
        }

        // read a single char that we will match against
        let ch = self.read_char();

        // matching identifiers and keywords
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

            // or return an identifier
            return Token::Identifier(str);
        }

        // matching integer literals
        if ch.is_ascii_digit() {
            let literal = self.read_complex(|ch| ch.is_ascii_digit());

            // parse and return literal
            return Token::Literal(Literal::Integer(literal.parse().unwrap())); // TODO: fail on integers that exceed i32::MAX
        }

        // matching multi-char tokens
        let multi_char = match self.read_complex(|ch| ch.is_ascii_punctuation()).as_str() {
            "&&" => Token::And,
            "||" => Token::Or,
            "==" => Token::Equal,
            "!=" => Token::NotEqual,
            "<=" => Token::LessThanEqual,
            ">=" => Token::GreaterThanEqual,
            _ => Token::Illegal,
        };

        // return multi-char token if it exists
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

// simple iterator pattern for Lexer
// NOTE: this code is currently unused
impl Iterator for Lexer<'_> {
    // we are iterating over tokens
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // read a token
        let token = self.read_token();

        // check whether we have reached EOF
        if token == Token::Illegal {
            None
        } else {
            Some(token)
        }
    }
}
