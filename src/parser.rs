use crate::{
    error::CompileError,
    expect_token, expect_token_soft,
    lexer::token::{Token, TokenKind},
    peekable::PeekableFar,
};

use self::{
    ast::{BlockItem, Function, Program, Statement},
    expression::{BinOperator, Expression, UnaryOperator},
};

pub mod ast;
pub mod expression;
mod helpers;

pub struct Parser<T: Iterator<Item = Token>> {
    input: PeekableFar<T>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(input: T) -> Self {
        Self {
            input: PeekableFar::new(input),
        }
    }

    pub fn read_program(&mut self) -> Result<Program, CompileError> {
        let mut functions = Vec::new();

        while self.input.peek().is_some() {
            functions.push(self.read_function()?);
        }

        Ok(Program(functions))
    }

    fn read_args(&mut self) -> Result<Vec<String>, CompileError> {
        expect_token!(self, TokenKind::OpenParen)?;

        let mut args = Vec::new();

        if self.peek_token()? != &TokenKind::CloseParen {
            self.read_type()?;
            args.push(self.read_identifier()?);
        }

        while self.peek_token()? != &TokenKind::CloseParen {
            expect_token!(self, TokenKind::Comma)?;

            self.read_type()?;
            args.push(self.read_identifier()?);
        }

        self.read_token()?;
        Ok(args)
    }

    fn read_function(&mut self) -> Result<Function, CompileError> {
        let return_type = self.read_type()?;
        let name = self.read_identifier()?;
        let parameters = self.read_args()?;

        let block = if expect_token_soft!(self, TokenKind::Semicolon) {
            None
        } else {
            expect_token!(self, TokenKind::OpenBrace)?;
            Some(self.read_block()?)
        };

        Ok(Function {
            name,
            return_type,
            parameters,
            block,
        })
    }

    fn read_block(&mut self) -> Result<Vec<BlockItem>, CompileError> {
        let mut block = vec![];

        loop {
            if expect_token_soft!(self, TokenKind::CloseBrace) {
                break;
            }

            block.push(self.read_block_item()?);
        }

        Ok(block)
    }

    fn read_block_item(&mut self) -> Result<BlockItem, CompileError> {
        let item = if expect_token_soft!(self, TokenKind::Keyword_DataType(_)) {
            let name = self.read_identifier()?;
            let assign = self.peek_token()?;
            let decl = if assign == &TokenKind::Assignment {
                // TODO: why isn't this the new pattern?
                self.read_token()?;
                BlockItem::Declaration(name, Some(self.read_expression()?))
            } else {
                BlockItem::Declaration(name, None)
            };

            expect_token!(self, TokenKind::Semicolon)?;

            decl
        } else {
            BlockItem::Statement(self.read_statement()?)
        };

        Ok(item)
    }

    fn read_statement(&mut self) -> Result<Statement, CompileError> {
        let statement = if expect_token_soft!(self, TokenKind::Keyword_Return) {
            Statement::Return(self.read_expression()?)
        } else if expect_token_soft!(self, TokenKind::Keyword_If) {
            expect_token!(self, TokenKind::OpenParen)?;

            let controlling = self.read_expression()?;

            expect_token!(self, TokenKind::CloseParen)?;

            let statement_true = self.read_statement()?;

            if expect_token_soft!(self, TokenKind::Keyword_Else) {
                let statement_false = self.read_statement()?;

                Statement::Conditional(
                    controlling,
                    Box::new(statement_true),
                    Some(Box::new(statement_false)),
                )
            } else {
                Statement::Conditional(controlling, Box::new(statement_true), None)
            }
        } else if expect_token_soft!(self, TokenKind::OpenBrace) {
            Statement::Compound(self.read_block()?)
        } else {
            Statement::Expression(self.read_expression()?)
        };

        if matches!(
            statement,
            Statement::Compound(_) | Statement::Conditional(..)
        ) {
            return Ok(statement);
        } // TODO: simplify this

        expect_token!(self, TokenKind::Semicolon)?;

        Ok(statement)
    }

    fn read_expression(&mut self) -> Result<Expression, CompileError> {
        let token = self.peek_far_token(2)?;

        if let TokenKind::Assignment = token {
            let ident = self.read_identifier()?;
            self.read_token()?; // assignment

            let exp = self.read_expression()?;

            return Ok(Expression::Assign(ident, Box::new(exp)));
        }

        Ok(self.read_conditional_exp()?)
    }

    fn read_conditional_exp(&mut self) -> Result<Expression, CompileError> {
        let exp = self.read_logical_or_exp()?;

        if self.peek_token()? == &TokenKind::QuestionMark {
            self.read_token()?;

            let e1 = self.read_expression()?;
            expect_token!(self, TokenKind::Colon)?;
            let e2 = self.read_conditional_exp()?;

            Ok(Expression::Conditional(
                Box::new(exp),
                Box::new(e1),
                Box::new(e2),
            ))
        } else {
            Ok(exp)
        }
    }

    fn read_logical_or_exp(&mut self) -> Result<Expression, CompileError> {
        let mut exp = self.read_logical_and_exp()?;

        let mut next_token = self.peek_token()?;
        while next_token == &TokenKind::Or {
            let op = match self.read_token()? {
                TokenKind::Or => BinOperator::LogicalOR,
                _ => panic!("Unknown token in read_expression"),
            };

            let next_exp = self.read_logical_and_exp()?;
            exp = Expression::BinaryOp(op, Box::new(exp), Box::new(next_exp));

            next_token = self.peek_token()?;
        }

        Ok(exp)
    }

    fn read_logical_and_exp(&mut self) -> Result<Expression, CompileError> {
        let mut exp = self.read_equality_exp()?;

        let mut next_token = self.peek_token()?;
        while next_token == &TokenKind::And {
            let op = match self.read_token()? {
                TokenKind::And => BinOperator::LogicalAND,
                _ => panic!("Unknown token in read_logical_and_expression"),
            };

            let next_exp = self.read_equality_exp()?;
            exp = Expression::BinaryOp(op, Box::new(exp), Box::new(next_exp));

            next_token = self.peek_token()?;
        }

        Ok(exp)
    }

    fn read_equality_exp(&mut self) -> Result<Expression, CompileError> {
        let mut exp = self.read_relational_exp()?;

        let mut next_token = self.peek_token()?;
        while next_token == &TokenKind::Equal || next_token == &TokenKind::NotEqual {
            let op = match self.read_token()? {
                TokenKind::Equal => BinOperator::Equal,
                TokenKind::NotEqual => BinOperator::NotEqual,
                _ => panic!("Unknown token in read_equality_expression"),
            };

            let next_exp = self.read_relational_exp()?;
            exp = Expression::BinaryOp(op, Box::new(exp), Box::new(next_exp));

            next_token = self.peek_token()?;
        }

        Ok(exp)
    }

    fn read_relational_exp(&mut self) -> Result<Expression, CompileError> {
        let mut exp = self.read_additive_exp()?;

        let mut next_token = self.peek_token()?;
        while next_token == &TokenKind::LessThan
            || next_token == &TokenKind::LessThanEqual
            || next_token == &TokenKind::GreaterThan
            || next_token == &TokenKind::GreaterThanEqual
        {
            let op = match self.read_token()? {
                TokenKind::LessThan => BinOperator::LessThan,
                TokenKind::LessThanEqual => BinOperator::LessThanOrEqual,
                TokenKind::GreaterThan => BinOperator::GreaterThan,
                TokenKind::GreaterThanEqual => BinOperator::GreaterThanOrEqual,
                _ => panic!("Unknown token in read_relational_expression"),
            };

            let next_exp = self.read_additive_exp()?;
            exp = Expression::BinaryOp(op, Box::new(exp), Box::new(next_exp));

            next_token = self.peek_token()?;
        }

        Ok(exp)
    }

    fn read_additive_exp(&mut self) -> Result<Expression, CompileError> {
        let mut term = self.read_term()?;

        let mut next_token = self.peek_token()?;
        while next_token == &TokenKind::Addition || next_token == &TokenKind::Minus {
            let op = match self.read_token()? {
                TokenKind::Addition => BinOperator::Addition,
                TokenKind::Minus => BinOperator::Subtraction,
                _ => panic!("Unknown token in read_additive_expression"),
            };

            let next_term = self.read_term()?;
            term = Expression::BinaryOp(op, Box::new(term), Box::new(next_term));

            next_token = self.peek_token()?;
        }

        Ok(term)
    }

    fn read_term(&mut self) -> Result<Expression, CompileError> {
        let mut factor = self.read_factor()?;

        let mut next_token = self.peek_token()?;
        while next_token == &TokenKind::Multiplication || next_token == &TokenKind::Division {
            let op = match self.read_token()? {
                TokenKind::Multiplication => BinOperator::Multiplication,
                TokenKind::Division => BinOperator::Division,
                _ => panic!("Unknown token in read_term"),
            };

            let next_factor = self.read_factor()?;
            factor = Expression::BinaryOp(op, Box::new(factor), Box::new(next_factor));

            next_token = self.peek_token()?;
        }

        Ok(factor)
    }

    fn read_factor(&mut self) -> Result<Expression, CompileError> {
        let token = self.read_token()?;

        if let TokenKind::Identifier(ref name) = token {
            if expect_token_soft!(self, TokenKind::OpenParen) {
                let mut args = Vec::new();
                if self.peek_token()? != &TokenKind::CloseParen {
                    args.push(self.read_expression()?);
                }
                while self.peek_token()? != &TokenKind::CloseParen {
                    expect_token!(self, TokenKind::Comma)?;

                    args.push(self.read_expression()?);
                }
                self.read_token()?;
                return Ok(Expression::FunCall(name.clone(), args));
            }
        }

        match token {
            TokenKind::OpenParen => {
                let exp = self.read_expression()?;
                expect_token!(self, TokenKind::CloseParen)?;

                return Ok(exp);
            }
            TokenKind::Literal_Integer(int) => return Ok(Expression::Constant(int)),
            TokenKind::Identifier(name) => return Ok(Expression::Variable(name)),
            TokenKind::BitwiseComplement | TokenKind::LogicalNegation | TokenKind::Minus => (),
            _ => panic!("Error in read_factor, unknown token {:?}", token),
        }

        //unary operator parsing
        let operator = match token {
            TokenKind::Minus => UnaryOperator::Negation,
            TokenKind::BitwiseComplement => UnaryOperator::BitwiseComplement,
            TokenKind::LogicalNegation => UnaryOperator::LogicalNegation,
            _ => panic!("Not a unary operator!"),
        };

        let inner = self.read_factor()?;

        Ok(Expression::UnaryOp(operator, Box::new(inner)))
    }
}
