#![feature(let_chains)]
#![feature(is_some_and)]
#![feature(if_let_guard)]
#![feature(option_result_contains)]

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
            Self::emit_err_from_token(&format!("`{expected_kind:?}`"), tok);
        }
    }

    fn emit_err_from_token(expected: &str, token: Option<Token>) -> ! {
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

    fn parse_block(&mut self, new_scope: bool) -> Block<'a> {
        self.expect_token(TokenKind::OpenBrace);

        if new_scope {
            self.scopes.push(Vec::new());
        }

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
                let (ident, init) = self.parse_declaration();

                BlockItem::Declaration(ident, init)
            }

            _ => BlockItem::Statement(self.parse_statement()),
        }
    }

    fn parse_declaration(&mut self) -> (u32, Option<Expression<'a>>) {
        self.input.next();

        let ident_tok = self.input.next();
        let Some(Token { kind: TokenKind::Ident(ident), .. }) = ident_tok else {
            Self::emit_err_from_token("<identifier>", ident_tok);
        };

        let current_scope = self.scopes.last_mut().unwrap();

        if current_scope.contains(&ident) {
            SpannedError::with_span("variable already defined", ident_tok.unwrap().span).emit()
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

        (ident as u32, init)
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
                }

                return Statement::Conditional(expr, if_true, None);
            }

            Some(TokenKind::OpenBrace) => return Statement::Compound(self.parse_block(true)),

            Some(TokenKind::Keyword(Keyword::While)) => {
                self.input.next();

                self.expect_token(TokenKind::OpenParen);
                let controlling = self.parse_expression();
                self.expect_token(TokenKind::CloseParen);

                let body = self.parse_statement();
                return Statement::While(controlling, Box::new(body));
            }

            Some(TokenKind::Keyword(Keyword::Do)) => {
                self.input.next();

                let body = self.parse_statement();

                self.expect_token(TokenKind::Keyword(Keyword::While));

                self.expect_token(TokenKind::OpenParen);
                let controlling = self.parse_expression();
                self.expect_token(TokenKind::CloseParen);

                // note that this is different from a normal `while` loop due
                // to the trailing semicolon
                Statement::Do(controlling, Box::new(body))
            }

            Some(TokenKind::Keyword(Keyword::For)) => {
                self.input.next();

                self.expect_token(TokenKind::OpenParen);

                if let Some(TokenKind::Keyword(Keyword::Int)) = self.input.peek().map(|tk| &tk.kind)
                {
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

            Some(TokenKind::Keyword(Keyword::Break)) => {
                self.input.next();
                Statement::Break
            }
            Some(TokenKind::Keyword(Keyword::Continue)) => {
                self.input.next();
                Statement::Continue
            }

            _ => Statement::Expression(self.parse_optional_expression()),
        };

        self.expect_token(TokenKind::Semicolon);
        stmt
    }

    fn parse_optional_expression(&mut self) -> Option<Expression<'a>> {
        if self.input.peek().map_or(true, |tok| {
            matches!(tok.kind, TokenKind::Semicolon | TokenKind::CloseParen)
        }) {
            None
        } else {
            Some(self.parse_expression())
        }
    }

    fn parse_expression(&mut self) -> Expression<'a> {
        if let Some(TokenKind::Equals) = self.input.peek_nth(1).map(|t| t.kind.clone()) {
            let ident_tok = self.input.next();
            let Some(Token { kind: TokenKind::Ident(ident), .. }) = ident_tok else {
                Self::emit_err_from_token("<variable>", ident_tok);
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

    fn parse_ternary(&mut self) -> Expression<'a> {
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

    fn parse_operand(&mut self) -> Expression<'a> {
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

            Some(TokenKind::Ident(ident)) => {
                if let Some(TokenKind::OpenParen) = self.input.peek().as_ref().map(|t| &t.kind) {
                    self.input.next();

                    let mut args = Vec::new();

                    while !matches!(self.input.peek().map(|t| &t.kind), Some(TokenKind::CloseParen)) {
                        args.push(self.parse_expression());

                        if !matches!(self.input.peek().map(|t| &t.kind), Some(TokenKind::Comma)) {
                            break;
                        }

                        self.expect_token(TokenKind::Comma);
                    }

                    self.expect_token(TokenKind::CloseParen);

                    Expression::FunctionCall { identifier: ident, args }
                } else {
                    Expression::Variable { identifier: self.get_variable(ident, tok.unwrap().span) }
                }
            }

            _ => Self::emit_err_from_token("<expresion>", tok),
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
            }

            position -= 1;
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

            fn parse_binop(&mut self) -> Box<crate::ast::Expression<'a>> {
                self.parse_binop_impl(10)
            }

            fn parse_binop_impl(&mut self, lvl: u8) -> Box<crate::ast::Expression<'a>> {
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
        10 = BinOp::LogicalOr,
        9 = BinOp::LogicalAnd,
        8 = BinOp::BitwiseOr,
        7 = BinOp::ExclusiveOr,
        6 = BinOp::BitwiseAnd,
        5 = BinOp::Equals | BinOp::NotEquals,
        4 = BinOp::LessThan | BinOp::LessThanOrEquals | BinOp::GreaterThan | BinOp::GreaterThanOrEquals,
        3 = BinOp::LeftShift | BinOp::RightShift,
        2 = BinOp::Add | BinOp::Sub,
        1 = BinOp::Mul | BinOp::Div | BinOp::Modulo,
    };
    {
        TokenKind::DoublePipe => BinOp::LogicalOr,
        TokenKind::DoubleAnd => BinOp::LogicalAnd,
        TokenKind::DoubleEquals => BinOp::Equals,
        TokenKind::ExclaimEquals => BinOp::NotEquals,
        TokenKind::LeftArrowEquals => BinOp::LessThanOrEquals,
        TokenKind::RightArrowEquals => BinOp::GreaterThanOrEquals,

        TokenKind::Plus => BinOp::Add,
        TokenKind::Minus => BinOp::Sub,
        TokenKind::Star => BinOp::Mul,
        TokenKind::Slash => BinOp::Div,
        TokenKind::Percent => BinOp::Modulo,

        TokenKind::RightArrow => BinOp::GreaterThan,
        TokenKind::LeftArrow => BinOp::LessThan,

        TokenKind::DoubleLeftArrow => BinOp::LeftShift,
        TokenKind::DoubleRightArrow => BinOp::RightShift,

        TokenKind::Pipe => BinOp::BitwiseOr,
        TokenKind::And => BinOp::BitwiseAnd,
        TokenKind::Caret => BinOp::ExclusiveOr,
    }
}
