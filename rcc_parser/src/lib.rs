#![feature(let_chains)]
#![feature(is_some_and)]

use self::ast::{Expression, Program, Statement};

pub mod ast;
pub mod pretty_printer;

use ast::Function;
use peekmore::{PeekMore, PeekMoreIterator};
use rcc_error::SpannedError;
use rcc_lexer::{Keyword, Token, TokenKind};

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

        let statement = self.parse_statement();

        self.expect_token(TokenKind::CloseBrace);

        Function { name, statement }
    }

    fn parse_statement(&mut self) -> Statement {
        self.expect_token(TokenKind::Keyword(Keyword::Return));
        let expression = self.parse_expression();
        self.expect_token(TokenKind::Semicolon);

        Statement::Return(expression)
    }

    fn parse_expression(&mut self) -> Expression {
        let tok = self.input.next();
        let Some(TokenKind::Literal(val)) = tok.as_ref().map(|t| &t.kind) else {
            self.emit_err_from_token("<int>", tok)
        };

        Expression::Literal { val: *val }
    }

    // fn parse_operand(&mut self) -> Box<Expression> {
    //     let tok = self.input.next().expect("oof, unexpected EOF");
    //     Box::new(match tok.kind {
    //         TokenKind::Literal(val) => Expression::Literal { val },
    //         TokenKind::Ident(name) => Expression::Variable {
    //             id: self
    //                 .variables
    //                 .iter()
    //                 .position(|var| *var == name)
    //                 .expect("undefined variable!") as u32,
    //         },
    //         TokenKind::OpenParen => {
    //             let mut expr = *self.parse_binop();
    //             self.expect_token(TokenKind::CloseParen);

    //             let Expression::BinOp { ref mut has_parens, .. } = expr else {
    //                 unreachable!();
    //             };

    //             *has_parens = true;

    //             expr
    //         }
    //         _ => self.emit_err_from_token("operand", tok),
    //     })
    // }
}

// macro_rules! impl_binop {
//     ({$( $lvl:literal = $match:pat, )+}; {$($token:pat => $op:expr,)*} ) => {
//         impl<'a, I: ::std::iter::Iterator<Item = ::rcc_lexer::Token<'a>>> crate::Parser<'a, I> {
//             fn map_tok_to_op(tok: ::rcc_lexer::TokenKind) -> Option<crate::ast::BinOp> {
//                 match tok {
//                     $($token => Some($op),)*
//                     _ => None
//                 }
//             }

//             fn parse_binop(&mut self) -> Box<crate::ast::Expression> {
//                 self.parse_binop_impl(2)
//             }

//             fn parse_binop_impl(&mut self, lvl: u8) -> Box<crate::ast::Expression> {
//                 if lvl == 0 {
//                     return self.parse_operand();
//                 }

//                 let lhs = self.parse_binop_impl(lvl - 1);

//                 match lvl {
//                     $($lvl => {
//                         let mut lhs = lhs;
//                         while let Some(op) = self.input.peek().map(|tk| tk.kind.clone()).and_then(Self::map_tok_to_op) && matches!(op, $match) {
//                             self.input.next();

//                             let rhs = self.parse_binop_impl(lvl - 1);

//                             lhs = Box::new(crate::ast::Expression::BinOp { has_parens: false, lhs, rhs, op })
//                         }

//                         lhs
//                     })+
//                     _ => unreachable!()
//                 }
//             }
//         }
//     };
// }

// impl_binop! {
//     {
//         2 = BinOp::Add | BinOp::Sub,
//         1 = BinOp::Mul | BinOp::Div,
//     };
//     {
//         TokenKind::Plus => BinOp::Add,
//         TokenKind::Minus => BinOp::Sub,
//         TokenKind::Star => BinOp::Mul,
//         TokenKind::Slash => BinOp::Div,
//     }
// }
