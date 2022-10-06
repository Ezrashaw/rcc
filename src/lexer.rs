use self::token::{Token, TokenKind};
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

    // pops a character from the input
    fn read_char(&mut self) -> char {
        let ch = self.peek_char();

        self.position += 1;
        self.column += 1;

        if ch == '\n' {
            // TODO: this is *nix only newlines; support '\r' and '\n'
            self.line += 1;
            self.column = 1;
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

        Token::new(kind, self.file_name.clone(), line, column) // TODO: `.clone()` AAAAARRGGHHH!!!!
    }

    fn read_tokenkind(&mut self) -> TokenKind {
        // read a single char that we will match against
        let ch = self.read_char();

        // matching identifiers and keywords
        if ch.is_ascii_alphabetic() {
            let str = self.read_complex(|ch| ch.is_ascii_alphanumeric() || ch == '_');

            // keyword matching
            // TODO: make this system smarter (we could automatically do this, maybe with 'build.rs')
            let keyword = match str.as_str() {
                "return" => Some(TokenKind::Keyword_Return),
                "int" => Some(TokenKind::Keyword_DataType(CType::Integer)),
                "if" => Some(TokenKind::Keyword_If),
                "else" => Some(TokenKind::Keyword_Else),
                "for" => Some(TokenKind::Keyword_For),
                "while" => Some(TokenKind::Keyword_While),
                "do" => Some(TokenKind::Keyword_Do),
                "break" => Some(TokenKind::Keyword_Break),
                "continue" => Some(TokenKind::Keyword_Continue),
                _ => None,
            };

            // return the tokenized keyword
            if let Some(keyword) = keyword {
                return keyword;
            }

            // or return an identifier
            return TokenKind::Identifier(str);
        }

        // matching integer literals
        if ch.is_ascii_digit() {
            let literal = self.read_complex(|ch| ch.is_ascii_digit());

            // parse and return literal
            return TokenKind::Literal_Integer(literal.parse().unwrap());
            // TODO: fail on integers that exceed i32::MAX
        }

        let double_char: String = String::from_iter([ch, self.peek_char()]);
        // matching multi-char tokens
        let multi_char = match double_char.as_str() {
            "&&" => TokenKind::And,
            "||" => TokenKind::Or,
            "==" => TokenKind::Equal,
            "!=" => TokenKind::NotEqual,
            "<=" => TokenKind::LessThanEqual,
            ">=" => TokenKind::GreaterThanEqual,
            _ => TokenKind::Illegal,
        };

        // return multi-char token if it exists
        if multi_char != TokenKind::Illegal {
            self.read_char();
            return multi_char;
        }

        // matching single character tokens
        // TODO: make this better, maybe with 'build.rs'
        match ch {
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            ';' => TokenKind::Semicolon,
            '-' => TokenKind::Minus,
            '~' => TokenKind::BitwiseComplement,
            '!' => TokenKind::LogicalNegation,
            '+' => TokenKind::Addition,
            '*' => TokenKind::Multiplication,
            '/' => TokenKind::Division,
            '<' => TokenKind::LessThan,
            '>' => TokenKind::GreaterThan,
            '=' => TokenKind::Assignment,
            ':' => TokenKind::Colon,
            '?' => TokenKind::QuestionMark,
            ',' => TokenKind::Comma,
            _ => TokenKind::Illegal,
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
impl Iterator for Lexer<'_> {
    // we are iterating over tokens
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // read a token
        let token = self.read_token();

        // check whether we have reached EOF
        if token.kind == TokenKind::Illegal {
            None
        } else {
            Some(token)
        }
    }
}
