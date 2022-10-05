use crate::{
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

    pub fn read_program(&mut self) -> Program {
        let mut functions = Vec::new();

        while self.input.peek().is_some() {
            functions.push(self.read_function());
        }

        Program(functions)
    }

    fn read_args(&mut self) -> Vec<String> {
        self.expect_token(TokenKind::OpenParen);

        let mut args = Vec::new();

        if self.peek_token() != &TokenKind::CloseParen {
            self.read_type();
            args.push(self.read_identifier());
        }

        while self.peek_token() != &TokenKind::CloseParen {
            self.expect_token(TokenKind::Comma);

            self.read_type();
            args.push(self.read_identifier());
        }

        self.read_token();
        args
    }

    fn read_function(&mut self) -> Function {
        let return_type = self.read_type();
        let name = self.read_identifier();
        let parameters = self.read_args();

        let block = if self.peek_token() == &TokenKind::Semicolon {
            self.read_token();
            None
        } else {
            Some(self.read_block())
        };

        Function {
            name,
            return_type,
            parameters,
            block,
        }
    }

    fn read_block(&mut self) -> Vec<BlockItem> {
        let mut block = vec![];

        self.expect_token(TokenKind::OpenBrace);

        loop {
            if self.peek_token() == &TokenKind::CloseBrace {
                self.read_token();
                break;
            }

            block.push(self.read_block_item());
        }

        block
    }

    fn read_block_item(&mut self) -> BlockItem {
        let item = if let TokenKind::Keyword_DataType(_) = self.peek_token() {
            self.read_token(); // ctype
            let name = self.read_identifier();
            let assign = self.peek_token();
            let decl = if assign == &TokenKind::Assignment {
                self.read_token();
                BlockItem::Declaration(name, Some(self.read_expression()))
            } else {
                BlockItem::Declaration(name, None)
            };

            self.expect_token(TokenKind::Semicolon);

            decl
        } else {
            BlockItem::Statement(self.read_statement())
        };

        item
    }

    fn read_statement(&mut self) -> Statement {
        let token = self.peek_token();

        let statement = if let TokenKind::Keyword_Return = token {
            self.read_token();
            let exp = self.read_expression();

            Statement::Return(exp)
        } else if let TokenKind::Keyword_If = token {
            self.read_token();

            self.expect_token(TokenKind::OpenParen);

            let controlling = self.read_expression();

            self.expect_token(TokenKind::CloseParen);

            let statement_true = self.read_statement();

            if self.peek_token() == &TokenKind::Keyword_Else {
                self.read_token();

                let statement_false = self.read_statement();

                Statement::Conditional(
                    controlling,
                    Box::new(statement_true),
                    Some(Box::new(statement_false)),
                )
            } else {
                Statement::Conditional(controlling, Box::new(statement_true), None)
            }
        } else if let TokenKind::OpenBrace = token {
            Statement::Compound(self.read_block())
        } else {
            Statement::Expression(self.read_expression())
        };

        if matches!(
            statement,
            Statement::Compound(_) | Statement::Conditional(..)
        ) {
            return statement;
        }

        self.expect_token(TokenKind::Semicolon);

        statement
    }

    fn read_expression(&mut self) -> Expression {
        let token = self.peek_far_token(2);

        if let TokenKind::Assignment = token {
            let ident = self.read_token();
            // TODO: `self.read_ident` here
            if let TokenKind::Identifier(name) = ident {
                let name = name; // TODO: we shouldn't clone identifiers like this, or at all!
                self.read_token(); // assignment

                let exp = self.read_expression();

                return Expression::Assign(name, Box::new(exp));
            } else {
                panic!("Expected identifier! {:?}", ident);
            }
        }

        self.read_conditional_exp()
    }

    fn read_conditional_exp(&mut self) -> Expression {
        let exp = self.read_logical_or_exp();

        if self.peek_token() == &TokenKind::QuestionMark {
            self.read_token();

            let e1 = self.read_expression();
            self.expect_token(TokenKind::Colon);
            let e2 = self.read_conditional_exp();

            Expression::Conditional(Box::new(exp), Box::new(e1), Box::new(e2))
        } else {
            exp
        }
    }

    fn read_logical_or_exp(&mut self) -> Expression {
        let mut exp = self.read_logical_and_exp();

        let mut next_token = self.peek_token();
        while next_token == &TokenKind::Or {
            let op = match self.read_token() {
                TokenKind::Or => BinOperator::LogicalOR,
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
        while next_token == &TokenKind::And {
            let op = match self.read_token() {
                TokenKind::And => BinOperator::LogicalAND,
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
        while next_token == &TokenKind::Equal || next_token == &TokenKind::NotEqual {
            let op = match self.read_token() {
                TokenKind::Equal => BinOperator::Equal,
                TokenKind::NotEqual => BinOperator::NotEqual,
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
        while next_token == &TokenKind::LessThan
            || next_token == &TokenKind::LessThanEqual
            || next_token == &TokenKind::GreaterThan
            || next_token == &TokenKind::GreaterThanEqual
        {
            let op = match self.read_token() {
                TokenKind::LessThan => BinOperator::LessThan,
                TokenKind::LessThanEqual => BinOperator::LessThanOrEqual,
                TokenKind::GreaterThan => BinOperator::GreaterThan,
                TokenKind::GreaterThanEqual => BinOperator::GreaterThanOrEqual,
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
        while next_token == &TokenKind::Addition || next_token == &TokenKind::Minus {
            let op = match self.read_token() {
                TokenKind::Addition => BinOperator::Addition,
                TokenKind::Minus => BinOperator::Subtraction,
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
        while next_token == &TokenKind::Multiplication || next_token == &TokenKind::Division {
            let op = match self.read_token() {
                TokenKind::Multiplication => BinOperator::Multiplication,
                TokenKind::Division => BinOperator::Division,
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

        if let TokenKind::Identifier(ref name) = token {
            if self.peek_token() == &TokenKind::OpenParen {
                self.read_token();
                let mut args = Vec::new();
                if self.peek_token() != &TokenKind::CloseParen {
                    args.push(self.read_expression());
                }
                while self.peek_token() != &TokenKind::CloseParen {
                    self.expect_token(TokenKind::Comma);

                    args.push(self.read_expression());
                }
                self.read_token();
                return Expression::FunCall(name.clone(), args);
            }
        }

        match token {
            TokenKind::OpenParen => {
                let exp = self.read_expression();
                self.expect_token(TokenKind::CloseParen);

                return exp;
            }
            TokenKind::Literal_Integer(int) => return Expression::Constant(int),
            TokenKind::Identifier(name) => return Expression::Variable(name),
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

        let inner = self.read_factor();

        Expression::UnaryOp(operator, Box::new(inner))
    }
}
