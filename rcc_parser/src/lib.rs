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
use rcc_structures::BinOp;

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

    pub fn parse(mut self) -> Program<'a> {
        let mut functions = Vec::new();

        while self.input.peek().is_some() {
            functions.push(self.parse_function());
        }

        Program { functions }
    }

    fn parse_function(&mut self) -> Function<'a> {
        self.expect_token(TokenKind::Keyword(Keyword::Int));

        let tok = self.input.next();
        let Some(TokenKind::Ident(name)) = tok.as_ref().map(|t| &t.kind) else {
            Self::emit_err_from_token("<identifier>", tok)
        };

        let args = self.parse_fn_args();

        // FIXME: this is a bit gross
        let body = if matches!(
            self.input.peek().map(|t| &t.kind),
            Some(TokenKind::Semicolon)
        ) {
            self.input.next();

            None
        } else {
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

            Some(block)
        };

        Function { name, args, body }
    }

    fn parse_fn_args(&mut self) -> Vec<&'a str> {
        let mut args = Vec::new();

        self.expect_token(TokenKind::OpenParen);

        while matches!(
            self.input.peek().map(|tk| &tk.kind),
            Some(TokenKind::Keyword(Keyword::Int))
        ) {
            self.expect_token(TokenKind::Keyword(Keyword::Int));

            let tok = self.input.next();
            let Some(TokenKind::Ident(name)) = tok.as_ref().map(|t| &t.kind) else {
                Self::emit_err_from_token("<identifier>", tok)
            };

            args.push(*name);

            if !matches!(self.input.peek().map(|tk| &tk.kind), Some(TokenKind::Comma)) {
                break;
            }

            self.expect_token(TokenKind::Comma);
        }

        self.expect_token(TokenKind::CloseParen);

        args
    }
}
