use crate::{
    ctypes::CType,
    lexer::token::{Keyword, Literal, Token},
};

use self::{
    ast::{BlockItem, Function, Program, Statement},
    expression::{BinOperator, Expression, UnaryOperator},
};

pub mod ast;
pub mod expression;

#[derive(Debug)]
pub struct Parser<'a> {
    input: &'a [Token],
    pub position: usize,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a [Token]) -> Self {
        Self { input, position: 0 }
    }

    pub fn read_program(&mut self) -> Program {
        let mut functions = Vec::new();

        while self.position < self.input.len() {
            functions.push(self.read_function());
        }

        Program(functions)
    }

    fn read_token(&mut self) -> &Token {
        let token = if self.position >= self.input.len() {
            &Token::Illegal
        } else {
            &self.input[self.position]
        };
        self.position += 1;
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
            panic!("Expected identifer but found");
        }
    }

    fn read_type(&mut self) -> CType {
        if let Token::Keyword(Keyword::DataType(data_type)) = self.read_token() {
            data_type.clone()
        } else {
            panic!("Expected type but found");
        }
    }

    fn read_args(&mut self) -> Vec<String> {
        if self.read_token() != &Token::OpenParen {
            panic!("No opening argument paren!")
        }
        let mut args = Vec::new();

        if self.peek_token() != &Token::CloseParen {
            self.read_type();
            args.push(self.read_ident());
        }

        while self.peek_token() != &Token::CloseParen {
            if self.read_token() != &Token::Comma {
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

        let block = if self.peek_token() == &Token::Semicolon {
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

        if self.read_token() != &Token::OpenBrace {
            panic!("No opening block brace!")
        }

        loop {
            if self.peek_token() == &Token::CloseBrace {
                self.read_token();
                break;
            }

            block.push(self.read_block_item());
        }

        block
    }

    fn read_block_item(&mut self) -> BlockItem {
        let item = if let Token::Keyword(Keyword::DataType(_)) = self.peek_token() {
            self.read_token(); // ctype
            let name = self.read_ident();
            let assign = self.peek_token();
            let decl = if assign == &Token::Assignment {
                self.read_token();
                BlockItem::Declaration(name, Some(self.read_expression()))
            } else {
                BlockItem::Declaration(name, None)
            };

            if self.read_token() != &Token::Semicolon {
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

        let statement = if let Token::Keyword(keyword) = token {
            if keyword == &Keyword::Return {
                self.read_token();
                let exp = self.read_expression();

                Statement::Return(exp)
            } else if keyword == &Keyword::If {
                self.read_token();

                if self.read_token() != &Token::OpenParen {
                    panic!("Exprected open bracket!")
                }

                let controlling = self.read_expression();

                if self.read_token() != &Token::CloseParen {
                    panic!("Exprected closing bracket!")
                }

                let statement_true = self.read_statement();

                if self.peek_token() == &Token::Keyword(Keyword::Else) {
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
        } else if let Token::OpenBrace = token {
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

        if self.read_token() != &Token::Semicolon {
            panic!("Expected semicolon!");
        }

        statement
    }

    fn read_expression(&mut self) -> Expression {
        let token = &self.input[self.position + 1];

        if let Token::Assignment = token {
            let ident = self.read_token();
            if let Token::Identifier(name) = ident {
                let name = name.clone();
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

        if self.peek_token() == &Token::QuestionMark {
            self.read_token();

            let e1 = self.read_expression();
            if self.read_token() != &Token::Colon {
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
        while next_token == &Token::Or {
            let op = match self.read_token() {
                &Token::Or => BinOperator::LogicalOR,
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
        while next_token == &Token::And {
            let op = match self.read_token() {
                &Token::And => BinOperator::LogicalAND,
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
            let op = match *self.read_token() {
                Token::Equal => BinOperator::Equal,
                Token::NotEqual => BinOperator::NotEqual,
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
        let token = self.read_token().clone();

        if let Token::Identifier(ref name) = token {
            if self.peek_token() == &Token::OpenParen {
                self.read_token();
                let mut args = Vec::new();
                if self.peek_token() != &Token::CloseParen {
                    args.push(self.read_expression());
                }
                while self.peek_token() != &Token::CloseParen {
                    if self.read_token() != &Token::Comma {
                        panic!("Expected comma!");
                    }
                    args.push(self.read_expression());
                }
                self.read_token();
                return Expression::FunCall(name.clone(), args);
            }
        }

        match token {
            Token::OpenParen => {
                let exp = self.read_expression();
                if self.read_token() != &Token::CloseParen {
                    panic!("Expected close paren!");
                }
                return exp;
            }
            Token::Literal(Literal::Integer(int)) => return Expression::Constant(int),
            Token::Identifier(name) => return Expression::Variable(name),
            Token::BitwiseComplement | Token::LogicalNegation | Token::Minus => (),
            _ => panic!("Error in read_factor, unknown token {:?}", token),
        }

        //unary operator parsing

        let operator = match token {
            Token::Minus => UnaryOperator::Negation,
            Token::BitwiseComplement => UnaryOperator::BitwiseComplement,
            Token::LogicalNegation => UnaryOperator::LogicalNegation,
            _ => panic!("Not a unary operator!"),
        };

        let inner = self.read_factor();

        Expression::UnaryOp(operator, Box::new(inner))
    }
}
