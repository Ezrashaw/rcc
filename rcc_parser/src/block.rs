use rcc_lexer::{Keyword, Token, TokenKind};

use crate::{
    ast::{Block, BlockItem, Expression, Statement},
    maybe_next, maybe_peek, Parser,
};

impl<'a, I: Iterator<Item = Token<'a>>> Parser<'a, I> {
    /// Parses a common block, formally known as a compound statement.
    ///
    /// Parses the `6.8.2 compound-statement` grammar as per Annex A of the
    /// N1548 draft of the C11 standard. Note that there may be differences in
    /// scoping rules between function blocks and compound statements, this
    /// function makes no effort to distinguish them.
    pub(crate) fn parse_block(&mut self, new_scope: bool) -> Block<'a> {
        self.expect_token(TokenKind::OpenBrace);

        if new_scope {
            self.scopes.push(Vec::new());
        }

        let mut block_items = Vec::new();
        while !maybe_peek!(self, TokenKind::CloseBrace) {
            block_items.push(self.parse_block_item());
        }

        self.expect_token(TokenKind::CloseBrace);

        Block {
            block_items,
            variables: self.scopes.pop().unwrap(),
            parent: None,
        }
    }

    /// Parses part of a block, either a statement or a declaration.
    ///
    /// Parses the `6.8.2 block-item` grammar as per Annex A of the N1548 draft
    /// of the C11 standard.
    fn parse_block_item(&mut self) -> BlockItem<'a> {
        match self.peek_kind() {
            TokenKind::Keyword(Keyword::Int) => {
                let (variable_id, init) = self.parse_declaration();

                BlockItem::Declaration(variable_id, init)
            }

            _ => BlockItem::Statement(self.parse_statement()),
        }
    }

    /// Parses a variable declaration.
    ///
    /// Parses the `6.7 declaration` grammar as per Annex A of the N1548 draft
    /// of the C11 standard.
    fn parse_declaration(&mut self) -> (u32, Option<Expression<'a>>) {
        self.expect_token(TokenKind::Keyword(Keyword::Int));

        let ident = self.expect_ident();
        let variable_id = self.define_variable(ident);

        let init = maybe_next!(self, TokenKind::Equals).then(|| self.parse_expression());

        self.expect_token(TokenKind::Semicolon);
        (variable_id, init)
    }

    /// Parses a statement.
    ///
    /// Doesn't include variable declarations, does include compound statements.
    ///
    /// Parses the `6.8 statement` grammar as per Annex A of the N1548 draft of
    /// the C11 standard.
    fn parse_statement(&mut self) -> Statement<'a> {
        let stmt = match self.peek_kind() {
            TokenKind::Keyword(Keyword::Return) => {
                self.drop_next();

                Statement::Return(self.parse_expression())
            }

            TokenKind::Keyword(Keyword::If) => {
                self.drop_next();

                let controlling = self.parse_paren_expression();

                let true_branch = self.parse_statement();
                let false_branch = maybe_next!(self, TokenKind::Keyword(Keyword::Else))
                    .then(|| Box::new(self.parse_statement()));

                return Statement::Conditional(controlling, Box::new(true_branch), false_branch);
            }

            TokenKind::OpenBrace => return Statement::Compound(self.parse_block(true)),

            TokenKind::Keyword(Keyword::While) => {
                self.drop_next();

                let controlling = self.parse_paren_expression();
                let body = self.parse_statement();

                return Statement::While(controlling, Box::new(body));
            }

            TokenKind::Keyword(Keyword::Do) => {
                self.drop_next();

                let body = self.parse_statement();

                self.expect_token(TokenKind::Keyword(Keyword::While));

                let controlling = self.parse_paren_expression();

                // note that this is different from a normal `while` loop due
                // to the trailing semicolon
                Statement::Do(controlling, Box::new(body))
            }

            TokenKind::Keyword(Keyword::For) => {
                // FIXME: this is too verbose
                self.drop_next();

                self.expect_token(TokenKind::OpenParen);

                if maybe_peek!(self, TokenKind::Keyword(Keyword::Int)) {
                    // this is the declaration variant of the for loop
                    self.scopes.push(Vec::new());

                    let declaration = self.parse_declaration();

                    let condition = self.parse_optional_expression();
                    self.expect_token(TokenKind::Semicolon);

                    let post_expression = self.parse_optional_expression();
                    self.expect_token(TokenKind::CloseParen);

                    let body = self.parse_statement();

                    self.scopes.pop();

                    return Statement::ForDecl(
                        declaration.0,
                        declaration.1,
                        condition,
                        post_expression,
                        Box::new(body),
                    );
                }

                let initial_expression = self.parse_optional_expression();
                self.expect_token(TokenKind::Semicolon);

                let condition = self.parse_optional_expression();
                self.expect_token(TokenKind::Semicolon);

                let post_expression = self.parse_optional_expression();
                self.expect_token(TokenKind::CloseParen);

                let body = self.parse_statement();

                return Statement::For(
                    initial_expression,
                    condition,
                    post_expression,
                    Box::new(body),
                );
            }

            TokenKind::Keyword(Keyword::Break) => {
                self.drop_next();
                Statement::Break
            }
            TokenKind::Keyword(Keyword::Continue) => {
                self.drop_next();
                Statement::Continue
            }

            _ => Statement::Expression(self.parse_optional_expression()),
        };

        self.expect_token(TokenKind::Semicolon);
        stmt
    }
}
