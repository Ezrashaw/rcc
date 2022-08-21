use std::str::ParseBoolError;

use crate::{
    ctypes::CType,
    lexer::token::{Keyword, Literal, Token},
};

use self::{
    ast::{Function, Program, ReturnStatement},
    expression::{BinOperator, Expression, UnaryOperator},
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
        let mut exp = self.read_logical_and_exp();

        let mut next_token = self.peek_token();
        while next_token == &Token::OR {
            let op = match self.read_token() {
                &Token::OR => BinOperator::LogicalOR,
                _ => panic!("Unknown token in read_expression"),
            };

            let next_exp = self.read_logical_and_exp();
            exp = Expression::BinaryOp(op, Box::new(exp), Box::new(next_exp));

            next_token = self.peek_token();
        }

        exp
    }

    fn read_logical_and_exp(&mut self) -> Expression {
        let mut exp = self.read_equality_exp();

        let mut next_token = self.peek_token();
        while next_token == &Token::AND {
            let op = match self.read_token() {
                &Token::AND => BinOperator::LogicalAND,
                _ => panic!("Unknown token in read_logical_and_expression"),
            };

            let next_exp = self.read_equality_exp();
            exp = Expression::BinaryOp(op, Box::new(exp), Box::new(next_exp));

            next_token = self.peek_token();
        }

        exp
    }

    fn read_equality_exp(&mut self) -> Expression {
        let mut exp = self.read_relational_exp();

        let mut next_token = self.peek_token();
        while next_token == &Token::Equal || next_token == &Token::NotEqual {
            let op = match self.read_token() {
                &Token::Equal => BinOperator::Equal,
                &Token::NotEqual => BinOperator::NotEqual,
                _ => panic!("Unknown token in read_equality_expression"),
            };

            let next_exp = self.read_relational_exp();
            exp = Expression::BinaryOp(op, Box::new(exp), Box::new(next_exp));

            next_token = self.peek_token();
        }

        exp
    }

    fn read_relational_exp(&mut self) -> Expression {
        let mut exp = self.read_additive_exp();

        let mut next_token = self.peek_token();
        while next_token == &Token::LessThan
            || next_token == &Token::LessThanEqual
            || next_token == &Token::GreaterThan
            || next_token == &Token::GreaterThanEqual
        {
            let op = match self.read_token() {
                &Token::LessThan => BinOperator::LessThan,
                &Token::LessThanEqual => BinOperator::LessThanOrEqual,
                &Token::GreaterThan => BinOperator::GreaterThan,
                &Token::GreaterThanEqual => BinOperator::GreaterThanOrEqual,
                _ => panic!("Unknown token in read_relational_expression"),
            };

            let next_exp = self.read_additive_exp();
            exp = Expression::BinaryOp(op, Box::new(exp), Box::new(next_exp));

            next_token = self.peek_token();
        }

        exp
    }

    fn read_additive_exp(&mut self) -> Expression {
        let mut term = self.read_term();

        let mut next_token = self.peek_token();
        while next_token == &Token::Addition || next_token == &Token::Minus {
            let op = match self.read_token() {
                &Token::Addition => BinOperator::Addition,
                &Token::Minus => BinOperator::Subtraction,
                _ => panic!("Unknown token in read_additive_expression"),
            };

            let next_term = self.read_term();
            term = Expression::BinaryOp(op, Box::new(term), Box::new(next_term));

            next_token = self.peek_token();
        }

        term
    }

    fn read_term(&mut self) -> Expression {
        let mut factor = self.read_factor();

        let mut next_token = self.peek_token();
        while next_token == &Token::Multiplication || next_token == &Token::Division {
            let op = match self.read_token() {
                &Token::Multiplication => BinOperator::Multiplication,
                &Token::Division => BinOperator::Division,
                _ => panic!("Unknown token in read_term"),
            };

            let next_factor = self.read_factor();
            factor = Expression::BinaryOp(op, Box::new(factor), Box::new(next_factor));

            next_token = self.peek_token();
        }

        factor
    }

    fn read_factor(&mut self) -> Expression {
        let token = self.read_token();

        match token {
            &Token::OpenParen => {
                let exp = self.read_expression();
                if self.read_token() != &Token::CloseParen {
                    panic!("Expected close paren!");
                }
                return exp;
            }
            &Token::Literal(Literal::Integer(int)) => return Expression::Constant(int),
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

        return Expression::UnaryOp(operator, Box::new(inner));
    }
}
