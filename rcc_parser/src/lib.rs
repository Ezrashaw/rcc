#![feature(let_chains)]
#![feature(is_some_and)]
#![feature(if_let_guard)]
#![feature(option_result_contains)]

use self::ast::{Expression, Program, Statement};

pub mod ast;
pub mod pretty_printer;

use ast::{BlockItem, Function};
use peekmore::{PeekMore, PeekMoreIterator};
use rcc_error::SpannedError;
use rcc_lexer::{Keyword, Token, TokenKind};
use rcc_structures::{BinOp, UnaryOp};

/// A parser for the `C` programming language.
///
/// Generates an AST from an [`Iterator`] of tokens. (see [`rcc_lexer::Lexer`])
pub struct Parser<'a, I>
where
    I: Iterator<Item = Token<'a>>,
{
    input: PeekMoreIterator<I>,
}

impl<'a, I: Iterator<Item = Token<'a>>> Parser<'a, I> {
    /// Creates a new [`Parser<I>`] from the given tokens.
    pub fn new(input: I) -> Self {
        Self {
            input: input.peekmore(),
        }
    }

    fn expect_token(&mut self, expected_kind: TokenKind) {
        let tok = self.input.next();

        if !tok.as_ref().is_some_and(|tok| tok.kind == expected_kind) {
            self.emit_err_from_token(&format!("`{expected_kind:?}`"), tok);
        }
    }

    fn emit_err_from_token(&self, expected: &str, token: Option<Token>) -> ! {
        if let Some(token) = token {
            SpannedError::with_span(
                format!("expected {expected}, found `{:?}`", token.kind),
                token.span,
            )
            .emit()
        } else {
            SpannedError::without_span(format!("expected {expected}, found EOF")).emit()
        }
    }

    pub fn parse(mut self) -> Program<'a> {
        Program {
            function: self.parse_function(),
        }
    }

    fn parse_function(&mut self) -> Function<'a> {
        self.expect_token(TokenKind::Keyword(Keyword::Int));

        let tok = self.input.next();
        let Some(TokenKind::Ident(name)) = tok.as_ref().map(|t| &t.kind) else {
            self.emit_err_from_token("<identifier>", tok)
        };

        self.expect_token(TokenKind::OpenParen);
        self.expect_token(TokenKind::CloseParen);
        self.expect_token(TokenKind::OpenBrace);

        let mut block_items = Vec::new();
        let mut locals = Vec::new();
        while self
            .input
            .peek()
            .map_or(false, |tk| tk.kind != TokenKind::CloseBrace)
        {
            block_items.push(self.parse_block_item(&mut locals));
        }

        self.expect_token(TokenKind::CloseBrace);

        // Ensure that we have a return statement somewhere, if not, add one.
        if !block_items
            .iter()
            .any(|stmt| matches!(stmt, BlockItem::Statement(Statement::Return(_))))
        {
            block_items.push(BlockItem::Statement(Statement::Return(
                Expression::Literal { val: 0 },
            )));
        }

        Function {
            name,
            block_items,
            locals,
        }
    }

    fn parse_block_item(&mut self, locals: &mut Vec<&'a str>) -> BlockItem {
        let tok = self.input.peek();

        match tok.map(|t| &t.kind) {
            Some(TokenKind::Keyword(Keyword::Int)) => {
                self.input.next();

                let ident_tok = self.input.next();
                let Some(Token { kind: TokenKind::Ident(ident), .. }) = ident_tok else {
                    self.emit_err_from_token("<identifier>", ident_tok);
                };

                if locals.contains(&ident) {
                    SpannedError::with_span("variable already defined", ident_tok.unwrap().span)
                        .emit()
                }

                locals.push(ident);
                let ident = locals.len() - 1;

                let init = if self
                    .input
                    .peek()
                    .map(|t| t.kind.clone())
                    .contains(&TokenKind::Equals)
                {
                    self.input.next();

                    Some(self.parse_expression(locals))
                } else {
                    None
                };

                self.expect_token(TokenKind::Semicolon);

                BlockItem::Declaration(ident as u32, init)
            }

            _ => BlockItem::Statement(self.parse_statement(locals)),
        }
    }

    fn parse_statement(&mut self, locals: &mut Vec<&'a str>) -> Statement {
        let tok = self.input.peek();

        let stmt = match tok.map(|t| &t.kind) {
            Some(TokenKind::Keyword(Keyword::Return)) => {
                self.input.next();
                let expression = self.parse_expression(locals);

                Statement::Return(expression)
            }

            Some(TokenKind::Keyword(Keyword::If)) => {
                self.input.next();

                self.expect_token(TokenKind::OpenParen);
                let expr = self.parse_expression(locals);
                self.expect_token(TokenKind::CloseParen);

                let if_true = Box::new(self.parse_statement(locals));

                if let Some(Token {
                    kind: TokenKind::Keyword(Keyword::Else),
                    ..
                }) = self.input.peek()
                {
                    self.input.next();

                    return Statement::Conditional(
                        expr,
                        if_true,
                        Some(Box::new(self.parse_statement(locals))),
                    );
                } else {
                    return Statement::Conditional(expr, if_true, None);
                }
            }

            _ => Statement::Expression(self.parse_expression(locals)),
        };

        self.expect_token(TokenKind::Semicolon);
        stmt
    }

    fn parse_expression(&mut self, locals: &mut Vec<&'a str>) -> Expression {
        if let Some(TokenKind::Equals) = self.input.peek_nth(1).map(|t| t.kind.clone()) {
            let ident_tok = self.input.next();
            let Some(Token { kind: TokenKind::Ident(ident), .. }) = ident_tok else {
                self.emit_err_from_token("<variable>", ident_tok);
            };

            let Some(id) = locals.iter().position(|id| *id == ident) else {
                SpannedError::with_span("undefined variable", ident_tok.unwrap().span).emit()
            };

            self.expect_token(TokenKind::Equals);

            let expression = self.parse_expression(locals);

            Expression::Assignment {
                identifier: id as u32,
                expression: Box::new(expression),
            }
        } else {
            *self.parse_binop(locals)
        }
    }

    fn parse_operand(&mut self, locals: &mut Vec<&'a str>) -> Expression {
        let tok = self.input.next();

        match tok.as_ref().map(|t| &t.kind) {
            Some(TokenKind::Literal(val)) => Expression::Literal { val: *val as i32 },

            Some(tok) if let Some(unary) = Self::unary_op_from_tok(tok)
                => Expression::UnaryOp { expr: Box::new(self.parse_operand(locals)), op: unary },

            Some(TokenKind::OpenParen) => {
                let mut expr = self.parse_expression(locals);
                if let Expression::BinOp { ref mut has_parens, .. } = expr {
                    *has_parens = true;
                }

                self.expect_token(TokenKind::CloseParen);
                expr
            },

            Some(TokenKind::Ident(var)) => {
                if let Some(id) = locals.iter().position(|id| id == var) {
                    Expression::Variable { identifier: id as u32 }
                } else {
                    SpannedError::with_span("undefined variable", tok.unwrap().span).emit()
                }
            }

            _ => self.emit_err_from_token("<expresion>", tok),
        }
    }

    fn unary_op_from_tok(kind: &TokenKind) -> Option<UnaryOp> {
        match kind {
            TokenKind::Minus => Some(UnaryOp::Negation),
            TokenKind::Tilde => Some(UnaryOp::BitwiseComplement),
            TokenKind::Exclamation => Some(UnaryOp::LogicalNegation),

            _ => None,
        }
    }
}

macro_rules! impl_binop {
    ({$( $lvl:literal = $match:pat, )+}; {$($token:pat => $op:expr,)*} ) => {
        impl<'a, I: ::std::iter::Iterator<Item = ::rcc_lexer::Token<'a>>> crate::Parser<'a, I> {
            fn map_tok_to_op(tok: ::rcc_lexer::TokenKind) -> Option<::rcc_structures::BinOp> {
                match tok {
                    $($token => Some($op),)*
                    _ => None
                }
            }

            fn parse_binop(&mut self, locals: &mut Vec<&'a str>) -> Box<crate::ast::Expression> {
                self.parse_binop_impl(6, locals)
            }

            fn parse_binop_impl(&mut self, lvl: u8, locals: &mut Vec<&'a str>) -> Box<crate::ast::Expression> {
                if lvl == 0 {
                    return Box::new(self.parse_operand(locals));
                }

                let lhs = self.parse_binop_impl(lvl - 1, locals);

                match lvl {
                    $($lvl => {
                        let mut lhs = lhs;
                        while let Some(op) = self.input.peek().map(|tk| tk.kind.clone()).and_then(Self::map_tok_to_op) && matches!(op, $match) {
                            self.input.next();

                            let rhs = self.parse_binop_impl(lvl - 1, locals);

                            lhs = Box::new(crate::ast::Expression::BinOp { has_parens: false, lhs, rhs, op })
                        }

                        lhs
                    })+
                    _ => unreachable!()
                }
            }
        }
    };
}

impl_binop! {
    {
        6 = BinOp::LogicalOr,
        5 = BinOp::LogicalAnd,
        4 = BinOp::Equals | BinOp::NotEquals,
        3 = BinOp::LessThan | BinOp::LessThanOrEquals | BinOp::GreaterThan | BinOp::GreaterThanOrEquals,
        2 = BinOp::Add | BinOp::Sub,
        1 = BinOp::Mul | BinOp::Div,
    };
    {
        TokenKind::DoublePipe => BinOp::LogicalOr,
        TokenKind::DoubleAnd => BinOp::LogicalAnd,
        TokenKind::DoubleEquals => BinOp::Equals,
        TokenKind::ExclaimEquals => BinOp::NotEquals,
        TokenKind::LessThan => BinOp::LessThan,
        TokenKind::LessThanEquals => BinOp::LessThanOrEquals,
        TokenKind::GreaterThan => BinOp::GreaterThanOrEquals,
        TokenKind::GreaterThanEquals => BinOp::GreaterThanOrEquals,

        TokenKind::Plus => BinOp::Add,
        TokenKind::Minus => BinOp::Sub,
        TokenKind::Star => BinOp::Mul,
        TokenKind::Slash => BinOp::Div,
    }
}
