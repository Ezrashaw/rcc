use self::token::{Keyword, Literal, Token, TokenData};
use crate::ctypes::CType;

pub mod token;

// A Lexer, converts files into C tokens
pub struct Lexer<'a> {
    input: &'a [u8],
    position: usize,

    file_name: String,
    line: u32,
    column: u32,
}

impl<'a> Lexer<'a> {
    // creates a new `Lexer` from a string reference as byte slice
    pub fn new(input: &'a [u8], file_name: String) -> Self {
        Self {
            input,
            position: 0,
            line: 0,
            column: 1,
            file_name,
        }
    }

    // TODO: use the iterator instead
    // reads all of the input and returns the tokens
    pub fn read_all(&mut self) -> Vec<TokenData> {
        let mut tokens = vec![];

        loop {
            let token = self.read_token();
            if token == TokenData::Illegal {
                return tokens;
            }
            tokens.push(token);
        }
    }

    // pops a character from the input
    fn read_char(&mut self) -> char {
        let ch = self.peek_char();

        self.position += 1;
        self.column += 1;

        match ch {
            '\n' => {
                // TODO: this is *nix only newlines; support '\r' and '\n'
                self.line += 1;
                self.column = 1;
            }
        }

        ch
    }

    // peeks a character from the input
    fn peek_char(&self) -> char {
        if self.position >= self.input.len() {
            '\0'
        } else {
            self.input[self.position].into()
        }
    }

    fn read_token(&mut self) -> Token {
        // remove all leading whitespace
        while self.peek_char().is_whitespace() {
            self.read_char();
        }

        let line = self.line;
        let column = self.column;

        let kind = self.read_tokenkind();

        Token::new(kind, self.file_name, line, column)
    }

    fn read_tokenkind(&mut self) -> TokenData {
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
                return TokenData::Keyword(keyword);
            }

            // or return an identifier
            return TokenData::Identifier(str);
        }

        // matching integer literals
        if ch.is_ascii_digit() {
            let literal = self.read_complex(|ch| ch.is_ascii_digit());

            // parse and return literal
            return TokenData::Literal(Literal::Integer(literal.parse().unwrap()));
            // TODO: fail on integers that exceed i32::MAX
        }

        let double_char: String = String::from_iter([ch, self.peek_char()]);
        // matching multi-char tokens
        let multi_char = match double_char.as_str() {
            "&&" => TokenData::And,
            "||" => TokenData::Or,
            "==" => TokenData::Equal,
            "!=" => TokenData::NotEqual,
            "<=" => TokenData::LessThanEqual,
            ">=" => TokenData::GreaterThanEqual,
            _ => TokenData::Illegal,
        };

        // return multi-char token if it exists
        if multi_char != TokenData::Illegal {
            self.read_char();
            return multi_char;
        }

        // matching single character tokens
        // TODO: make this better, maybe with 'build.rs'
        match ch {
            '{' => TokenData::OpenBrace,
            '}' => TokenData::CloseBrace,
            '(' => TokenData::OpenParen,
            ')' => TokenData::CloseParen,
            ';' => TokenData::Semicolon,
            '-' => TokenData::Minus,
            '~' => TokenData::BitwiseComplement,
            '!' => TokenData::LogicalNegation,
            '+' => TokenData::Addition,
            '*' => TokenData::Multiplication,
            '/' => TokenData::Division,
            '<' => TokenData::LessThan,
            '>' => TokenData::GreaterThan,
            '=' => TokenData::Assignment,
            ':' => TokenData::Colon,
            '?' => TokenData::QuestionMark,
            ',' => TokenData::Comma,
            _ => TokenData::Illegal,
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
    type Item = TokenData;

    fn next(&mut self) -> Option<Self::Item> {
        // read a token
        let token = self.read_token();

        // check whether we have reached EOF
        if token == TokenData::Illegal {
            None
        } else {
            Some(token)
        }
    }
}
