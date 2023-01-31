#![feature(let_chains)]
#![feature(is_some_and)]
#![feature(if_let_guard)]
#![feature(option_result_contains)]

use std::mem;

use self::ast::{Expression, Program, Statement};

pub mod ast;
pub mod pretty_printer;

use ast::{Block, BlockItem, Function};
use peekmore::{PeekMore, PeekMoreIterator};
use rcc_error::SpannedError;
use rcc_lexer::{Keyword, Token, TokenKind};
use rcc_span::Span;
use rcc_structures::{BinOp, UnaryOp};

/// A parser for the `C` programming language.
///
/// Generates an AST from an [`Iterator`] of tokens. (see [`rcc_lexer::Lexer`])
pub struct Parser<'a, I>
where
    I: Iterator<Item = Token<'a>>,
{
    input: PeekMoreIterator<I>,
    scopes: Vec<Vec<&'a str>>,
}

impl<'a, I: Iterator<Item = Token<'a>>> Parser<'a, I> {
    /// Creates a new [`Parser<I>`] from the given tokens.
    pub fn new(input: I) -> Self {
        Self {
            input: input.peekmore(),
            scopes: Vec::new(),
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

        let mut block = self.parse_block();

        // Ensure that we have a return statement somewhere, if not, add one.
        if !block
            .block_items
            .iter()
            .any(|stmt| matches!(stmt, BlockItem::Statement(Statement::Return(_))))
        {
            block
                .block_items
                .push(BlockItem::Statement(Statement::Return(
                    Expression::Literal { val: 0 },
                )));
        }

        Function { name, block }
    }

    fn parse_block(&mut self) -> Block<'a> {
        self.expect_token(TokenKind::OpenBrace);

        self.scopes.push(Vec::new());

        let mut block_items = Vec::new();
        while self
            .input
            .peek()
            .map_or(false, |tk| tk.kind != TokenKind::CloseBrace)
        {
            block_items.push(self.parse_block_item());
        }

        self.expect_token(TokenKind::CloseBrace);

        Block {
            block_items,
            variables: self.scopes.pop().unwrap(),
            parent: None,
        }
    }

    fn parse_block_item(&mut self) -> BlockItem<'a> {
        let tok = self.input.peek();

        match tok.map(|t| &t.kind) {
            Some(TokenKind::Keyword(Keyword::Int)) => {
                self.input.next();

                let ident_tok = self.input.next();
                let Some(Token { kind: TokenKind::Ident(ident), .. }) = ident_tok else {
                    self.emit_err_from_token("<identifier>", ident_tok);
                };

                let current_scope = self.scopes.last_mut().unwrap();

                if current_scope.contains(&ident) {
                    SpannedError::with_span("variable already defined", ident_tok.unwrap().span)
                        .emit()
                }

                current_scope.push(ident);
                let ident = self.scopes.iter().flatten().count() - 1;

                let init = if self
                    .input
                    .peek()
                    .map(|t| t.kind.clone())
                    .contains(&TokenKind::Equals)
                {
                    self.input.next();

                    Some(self.parse_expression())
                } else {
                    None
                };

                self.expect_token(TokenKind::Semicolon);

                BlockItem::Declaration(ident as u32, init)
            }

            _ => BlockItem::Statement(self.parse_statement()),
        }
    }

    fn parse_statement(&mut self) -> Statement<'a> {
        let tok = self.input.peek();

        let stmt = match tok.map(|t| &t.kind) {
            Some(TokenKind::Keyword(Keyword::Return)) => {
                self.input.next();
                let expression = self.parse_expression();

                Statement::Return(expression)
            }

            Some(TokenKind::Keyword(Keyword::If)) => {
                self.input.next();

                self.expect_token(TokenKind::OpenParen);
                let expr = self.parse_expression();
                self.expect_token(TokenKind::CloseParen);

                let if_true = Box::new(self.parse_statement());

                if let Some(Token {
                    kind: TokenKind::Keyword(Keyword::Else),
                    ..
                }) = self.input.peek()
                {
                    self.input.next();

                    return Statement::Conditional(
                        expr,
                        if_true,
                        Some(Box::new(self.parse_statement())),
                    );
                } else {
                    return Statement::Conditional(expr, if_true, None);
                }
            }

            Some(TokenKind::OpenBrace) => return Statement::Compound(self.parse_block()),

            _ => Statement::Expression(self.parse_expression()),
        };

        self.expect_token(TokenKind::Semicolon);
        stmt
    }

    fn parse_expression(&mut self) -> Expression {
        if let Some(TokenKind::Equals) = self.input.peek_nth(1).map(|t| t.kind.clone()) {
            let ident_tok = self.input.next();
            let Some(Token { kind: TokenKind::Ident(ident), .. }) = ident_tok else {
                self.emit_err_from_token("<variable>", ident_tok);
            };

            let identifier = self.get_variable(ident, ident_tok.unwrap().span);

            self.expect_token(TokenKind::Equals);

            let expression = self.parse_expression();

            Expression::Assignment {
                identifier,
                expression: Box::new(expression),
            }
        } else {
            self.parse_ternary()
        }
    }

    fn parse_ternary(&mut self) -> Expression {
        let expr = self.parse_binop();

        if let Some(Token {
            kind: TokenKind::QuestionMark,
            ..
        }) = self.input.peek()
        {
            self.input.next();

            let if_true = Box::new(self.parse_expression());
            self.expect_token(TokenKind::Colon);
            let if_false = Box::new(self.parse_ternary());

            Expression::TernaryConditional {
                controlling: expr,
                if_true,
                if_false,
            }
        } else {
            *expr
        }
    }

    fn parse_operand(&mut self) -> Expression {
        let tok = self.input.next();

        match tok.as_ref().map(|t| &t.kind) {
            Some(TokenKind::Literal(val)) => Expression::Literal { val: *val as i32 },

            Some(tok) if let Some(unary) = Self::unary_op_from_tok(tok)
                => Expression::UnaryOp { expr: Box::new(self.parse_operand()), op: unary },

            Some(TokenKind::OpenParen) => {
                let mut expr = self.parse_expression();
                if let Expression::BinOp { ref mut has_parens, .. } = expr {
                    *has_parens = true;
                }

                self.expect_token(TokenKind::CloseParen);
                expr
            },

            Some(TokenKind::Ident(var)) => {
                Expression::Variable { identifier: self.get_variable(var, tok.unwrap().span) }
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

    fn get_variable(&mut self, ident: &str, span: Span) -> u32 {
        // FIXME: make this more functional
        let mut position = self.scopes.iter().flatten().count() as u32 - 1;
        for search in self
            .scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.iter().rev())
        {
            if ident == *search {
                return position;
            } else {
                position -= 1;
            }
        }

        SpannedError::with_span("undefined variable", span).emit();
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

            fn parse_binop(&mut self) -> Box<crate::ast::Expression> {
                self.parse_binop_impl(6)
            }

            fn parse_binop_impl(&mut self, lvl: u8) -> Box<crate::ast::Expression> {
                if lvl == 0 {
                    return Box::new(self.parse_operand());
                }

                let lhs = self.parse_binop_impl(lvl - 1);

                match lvl {
                    $($lvl => {
                        let mut lhs = lhs;
                        while let Some(op) = self.input.peek().map(|tk| tk.kind.clone()).and_then(Self::map_tok_to_op) && matches!(op, $match) {
                            self.input.next();

                            let rhs = self.parse_binop_impl(lvl - 1);

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
