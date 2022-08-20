use std::str::ParseBoolError;

use crate::{
    ctypes::CType,
    lexer::token::{Keyword, Literal, Token},
};

use self::{
    ast::{Function, Program, ReturnStatement},
    expression::{BinOperator, Expression, Factor, Term, UnaryOperator},
};

pub mod ast;
pub mod expression;

#[derive(Debug)]
pub struct Parser<'a> {
    input: &'a [Token],
    pub position: usize,
    pub tok: &'a Token,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a [Token]) -> Self {
        let parser = Self {
            input,
            position: 0,
            tok: &Token::Illegal,
        };

        parser
    }

    pub fn read_program(&mut self) -> Program {
        Program(self.read_function())
    }

    fn read_token(&mut self) -> &Token {
        let token = if self.position >= self.input.len() {
            &Token::Illegal
        } else {
            &self.input[self.position]
        };
        self.position += 1;
        self.tok = token;
        token
    }

    fn peek_token(&mut self) -> &Token {
        if self.position >= self.input.len() {
            &Token::Illegal
        } else {
            &self.input[self.position]
        }
    }

    fn read_ident(&mut self) -> String {
        if let Token::Identifier(ident) = self.read_token() {
            ident.clone()
        } else {
            panic!("Expected identifer but found {:?}", self.tok);
        }
    }

    fn read_type(&mut self) -> CType {
        if let Token::Keyword(Keyword::DataType(data_type)) = self.read_token() {
            data_type.clone()
        } else {
            panic!("Expected type but found {:?}", self.tok);
        }
    }

    fn read_args(&mut self) {
        if self.read_token() != &Token::OpenParen {
            panic!("No opening argument paren!")
        }
        if self.read_token() != &Token::CloseParen {
            panic!("No closing argument paren!")
        }
    }

    fn read_function(&mut self) -> Function {
        let return_type = self.read_type();
        let name = self.read_ident();
        self.read_args();
        if self.read_token() != &Token::OpenBrace {
            panic!("No opening block brace!")
        }
        let statements = vec![self.read_statement()];
        if self.read_token() != &Token::CloseBrace {
            panic!("No closing block brace!")
        }
        Function {
            name,
            return_type,
            statements,
        }
    }

    fn read_statement(&mut self) -> ReturnStatement {
        let return_statement = self.read_token();
        if *return_statement != Token::Keyword(Keyword::Return) {
            panic!("Not a return statement!")
        } else {
            let expression = self.read_expression();
            if self.read_token() != &Token::Semicolon {
                panic!("No semicolon! {:?}", self.tok)
            }
            ReturnStatement {
                ret_val: expression,
            }
        }
    }

    fn read_expression(&mut self) -> Expression {
        let mut nodes = vec![];
        let first_term = self.read_term();

        let mut next_token = self.peek_token();
        while next_token == &Token::Addition || next_token == &Token::Minus {
            let op = match self.read_token() {
                &Token::Addition => BinOperator::Addition,
                &Token::Minus => BinOperator::Subtraction,
                _ => panic!("Unknown token in read_expression"),
            };

            let next_term = self.read_term();
            let node = (op, next_term);
            nodes.push(node);

            next_token = self.peek_token();
        }

        Expression {
            term: first_term,
            nodes,
        }
    }

    fn read_term(&mut self) -> Term {
        let mut nodes = vec![];
        let first_factor = self.read_factor();

        let mut next_token = self.peek_token();
        while next_token == &Token::Multiplication || next_token == &Token::Division {
            let op = match self.read_token() {
                Token::Multiplication => BinOperator::Multiplication,
                Token::Division => BinOperator::Division,
                _ => panic!("Unknown token in read_term"),
            };

            let next_factor = self.read_factor();
            let node = (op, next_factor);
            nodes.push(node);

            next_token = self.peek_token();
        }

        Term {
            factor: first_factor,
            nodes,
        }
    }

    fn read_factor(&mut self) -> Factor {
        let token = self.read_token();

        match token {
            &Token::OpenParen => {
                let exp = self.read_expression();
                if self.read_token() != &Token::CloseParen {
                    panic!("Expected close paren!");
                }
                return Factor::Expression(Box::new(exp));
            }
            &Token::Literal(Literal::Integer(int)) => return Factor::Constant(int),
            &Token::BitwiseComplement | &Token::LogicalNegation | &Token::Minus => (),
            _ => panic!("Error in read_factor, unknown token"),
        }

        //unary operator parsing

        let operator = match token {
            &Token::Minus => UnaryOperator::Negation,
            &Token::BitwiseComplement => UnaryOperator::BitwiseComplement,
            &Token::LogicalNegation => UnaryOperator::LogicalNegation,
            _ => panic!("Not a unary operator!"),
        };

        let inner = self.read_factor();

        return Factor::UnaryOp {
            operator,
            factor: Box::new(inner),
        };
    }
}
