#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(option_result_contains)]

use self::ast::{Expression, Program, Statement};

pub mod ast;

mod block;
mod expr;
mod utils;

use ast::{BlockItem, Function};
use peekmore::{PeekMore, PeekMoreIterator};
use rcc_lexer::{Keyword, Token, TokenKind};
use utils::maybe_next;

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

    /// Parses a C program, in compliance with (a subset of) the C11 standard.
    ///
    /// We use the [N1548 draft](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1548.pdf)
    /// of the C11 standard (semantically equivalent) as the reference.
    ///
    /// More formally: parses the `6.9 translation-unit` grammar as per Annex A
    /// of the N1548 draft of the C11 standard.
    pub fn parse(mut self) -> Program<'a> {
        let mut functions = Vec::new();

        while self.input.peek().is_some() {
            functions.push(self.parse_function());
        }

        Program { functions }
    }

    /// Parses a function.
    ///
    /// The function may have a body or be a foward declaration of the form:
    /// `int function(int arg1, int arg2, ..);`
    ///
    /// Parses the `6.9.1 function-definition` grammar as per Annex A of the
    /// N1548 draft of the C11 standard.
    fn parse_function(&mut self) -> Function<'a> {
        self.expect_token(TokenKind::Keyword(Keyword::Int));

        let name = self.expect_ident();
        let args = self.parse_fn_args();

        let body = (!maybe_next!(self, TokenKind::Semicolon)).then(|| {
            self.scopes.push(args.clone());

            let mut block = self.parse_block(false);

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

            block
        });

        Function { name, args, body }
    }

    fn parse_fn_args(&mut self) -> Vec<&'a str> {
        let mut args = Vec::new();

        self.expect_token(TokenKind::OpenParen);

        while maybe_next!(self, TokenKind::Keyword(Keyword::Int)) {
            let arg_name = self.expect_ident();
            args.push(arg_name);

            if !maybe_next!(self, TokenKind::Comma) {
                break;
            }
        }

        self.expect_token(TokenKind::CloseParen);

        args
    }
}
