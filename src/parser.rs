use crate::{
    ctypes::CType,
    lexer::token::{Keyword, Literal, Token, TokenData},
    peekable::PeekableFar,
};

use self::{
    ast::{BlockItem, Function, Program, Statement},
    expression::{BinOperator, Expression, UnaryOperator},
};

pub mod ast;
pub mod expression;

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

    fn read_token(&mut self) -> TokenData {
        self.input.next().unwrap().data
    }

    fn peek_token(&mut self) -> &TokenData {
        &self.input.peek().unwrap().data // TODO: merge with `peek_far_token`?
    }

    fn peek_far_token(&mut self, d: usize) -> &TokenData {
        &self.input.peek_far(d).unwrap().data
    }

    fn read_ident(&mut self) -> String {
        if let TokenData::Identifier(ident) = self.read_token() {
            ident
        } else {
            panic!("Expected identifer but found");
        }
    }

    fn read_type(&mut self) -> CType {
        if let TokenData::Keyword(Keyword::DataType(data_type)) = self.read_token() {
            data_type
        } else {
            panic!("Expected type but found");
        }
    }

    fn read_args(&mut self) -> Vec<String> {
        if self.read_token() != TokenData::OpenParen {
            panic!("No opening argument paren!")
        }
        let mut args = Vec::new();

        if self.peek_token() != &TokenData::CloseParen {
            self.read_type();
            args.push(self.read_ident());
        }

        while self.peek_token() != &TokenData::CloseParen {
            if self.read_token() != TokenData::Comma {
                panic!("expected comma!")
            }
            self.read_type();
            args.push(self.read_ident());
        }

        self.read_token();
        args
    }

    fn read_function(&mut self) -> Function {
        let return_type = self.read_type();
        let name = self.read_ident();
        let parameters = self.read_args();

        let block = if self.peek_token() == &TokenData::Semicolon {
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

        if self.read_token() != TokenData::OpenBrace {
            panic!("No opening block brace!")
        }

        loop {
            if self.peek_token() == &TokenData::CloseBrace {
                self.read_token();
                break;
            }

            block.push(self.read_block_item());
        }

        block
    }

    fn read_block_item(&mut self) -> BlockItem {
        let item = if let TokenData::Keyword(Keyword::DataType(_)) = self.peek_token() {
            self.read_token(); // ctype
            let name = self.read_ident();
            let assign = self.peek_token();
            let decl = if assign == &TokenData::Assignment {
                self.read_token();
                BlockItem::Declaration(name, Some(self.read_expression()))
            } else {
                BlockItem::Declaration(name, None)
            };

            if self.read_token() != TokenData::Semicolon {
                panic!("Missing semicolon!");
            }

            decl
        } else {
            BlockItem::Statement(self.read_statement())
        };

        item
    }

    fn read_statement(&mut self) -> Statement {
        let token = self.peek_token();

        let statement = if let TokenData::Keyword(keyword) = token {
            if keyword == &Keyword::Return {
                self.read_token();
                let exp = self.read_expression();

                Statement::Return(exp)
            } else if keyword == &Keyword::If {
                self.read_token();

                if self.read_token() != TokenData::OpenParen {
                    panic!("Exprected open bracket!")
                }

                let controlling = self.read_expression();

                if self.read_token() != TokenData::CloseParen {
                    panic!("Exprected closing bracket!")
                }

                let statement_true = self.read_statement();

                if self.peek_token() == &TokenData::Keyword(Keyword::Else) {
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
            } else {
                panic!("Unknown keyword in statement!")
            }
        } else if let TokenData::OpenBrace = token {
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

        if self.read_token() != TokenData::Semicolon {
            panic!("Expected semicolon!");
        }

        statement
    }

    fn read_expression(&mut self) -> Expression {
        let token = self.peek_far_token(2);

        if let TokenData::Assignment = token {
            let ident = self.read_token();
            if let TokenData::Identifier(name) = ident {
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

        if self.peek_token() == &TokenData::QuestionMark {
            self.read_token();

            let e1 = self.read_expression();
            if self.read_token() != TokenData::Colon {
                panic!("expected colon in ternary conditional!");
            }
            let e2 = self.read_conditional_exp();

            Expression::Conditional(Box::new(exp), Box::new(e1), Box::new(e2))
        } else {
            exp
        }
    }

    fn read_logical_or_exp(&mut self) -> Expression {
        let mut exp = self.read_logical_and_exp();

        let mut next_token = self.peek_token();
        while next_token == &TokenData::Or {
            let op = match self.read_token() {
                TokenData::Or => BinOperator::LogicalOR,
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
        while next_token == &TokenData::And {
            let op = match self.read_token() {
                TokenData::And => BinOperator::LogicalAND,
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
        while next_token == &TokenData::Equal || next_token == &TokenData::NotEqual {
            let op = match self.read_token() {
                TokenData::Equal => BinOperator::Equal,
                TokenData::NotEqual => BinOperator::NotEqual,
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
        while next_token == &TokenData::LessThan
            || next_token == &TokenData::LessThanEqual
            || next_token == &TokenData::GreaterThan
            || next_token == &TokenData::GreaterThanEqual
        {
            let op = match self.read_token() {
                TokenData::LessThan => BinOperator::LessThan,
                TokenData::LessThanEqual => BinOperator::LessThanOrEqual,
                TokenData::GreaterThan => BinOperator::GreaterThan,
                TokenData::GreaterThanEqual => BinOperator::GreaterThanOrEqual,
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
        while next_token == &TokenData::Addition || next_token == &TokenData::Minus {
            let op = match self.read_token() {
                TokenData::Addition => BinOperator::Addition,
                TokenData::Minus => BinOperator::Subtraction,
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
        while next_token == &TokenData::Multiplication || next_token == &TokenData::Division {
            let op = match self.read_token() {
                TokenData::Multiplication => BinOperator::Multiplication,
                TokenData::Division => BinOperator::Division,
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

        if let TokenData::Identifier(ref name) = token {
            if self.peek_token() == &TokenData::OpenParen {
                self.read_token();
                let mut args = Vec::new();
                if self.peek_token() != &TokenData::CloseParen {
                    args.push(self.read_expression());
                }
                while self.peek_token() != &TokenData::CloseParen {
                    if self.read_token() != TokenData::Comma {
                        panic!("Expected comma!");
                    }
                    args.push(self.read_expression());
                }
                self.read_token();
                return Expression::FunCall(name.clone(), args);
            }
        }

        match token {
            TokenData::OpenParen => {
                let exp = self.read_expression();
                if self.read_token() != TokenData::CloseParen {
                    panic!("Expected close paren!");
                }
                return exp;
            }
            TokenData::Literal(Literal::Integer(int)) => return Expression::Constant(int),
            TokenData::Identifier(name) => return Expression::Variable(name),
            TokenData::BitwiseComplement | TokenData::LogicalNegation | TokenData::Minus => (),
            _ => panic!("Error in read_factor, unknown token {:?}", token),
        }

        //unary operator parsing

        let operator = match token {
            TokenData::Minus => UnaryOperator::Negation,
            TokenData::BitwiseComplement => UnaryOperator::BitwiseComplement,
            TokenData::LogicalNegation => UnaryOperator::LogicalNegation,
            _ => panic!("Not a unary operator!"),
        };

        let inner = self.read_factor();

        Expression::UnaryOp(operator, Box::new(inner))
    }
}
