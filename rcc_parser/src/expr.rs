use std::ops::BitXor;

use crate::{ast::Expression, maybe_next, maybe_peek, maybe_peek_nth, BinOp, Parser};
use rcc_error::SpannedError;
use rcc_lexer::{Token, TokenKind};
use rcc_structures::UnaryOp;

impl<'a, I: Iterator<Item = Token<'a>>> Parser<'a, I> {
    /// Parses an expression wrapped in parentheses.
    /// 
    /// There is no formal grammar rule in the C11 standard which corresponds
    /// to this function, however it is used often (in `if` statements and
    /// loops, etc).
    pub(crate) fn parse_paren_expression(&mut self) -> Expression<'a> {
        self.expect_token(TokenKind::OpenParen);
        let expr = self.parse_expression();
        self.expect_token(TokenKind::CloseParen);

        expr
    }

    /// Parses an expression or nothing at all.
    ///
    /// Parses the `6.5.1 expression(opt)` grammar as per Annex A of the N1548 draft
    /// of the C11 standard.
    pub(crate) fn parse_optional_expression(&mut self) -> Option<Expression<'a>> {
        // Should this list be expanded?
        maybe_peek!(self, TokenKind::Semicolon | TokenKind::CloseParen)
            .bitxor(true) // invert the above expression
            .then(|| self.parse_expression())
    }

    /// Parses a common C expression.
    ///
    /// A cornerstone of the parser. See the feature list for details.
    ///
    /// Parses the `6.5.1 expression` grammar as per Annex A of the N1548 draft
    /// of the C11 standard.
    pub(crate) fn parse_expression(&mut self) -> Expression<'a> {
        // assignment expressions
        // note that we can't just check for an identifier, it could be a
        // normal variable.
        if maybe_peek_nth!(self, 1, TokenKind::Equals) {
            let variable = self.expect_variable();

            self.expect_token(TokenKind::Equals);

            let expression = self.parse_expression();

            Expression::Assignment {
                variable,
                expression: Box::new(expression),
            }
        } else {
            self.parse_ternary_conditional()
        }
    }

    /// Parses a C ternary conditional expression.
    ///
    /// Ternary expressions are of the form
    /// `conditional ? true_val : false_val`. Note that unlike most C
    /// expressions, ternary conditionals are right-associative.
    ///
    /// Parses the `6.5.1 conditional-expression` grammar as per Annex A of the
    /// N1548 draft of the C11 standard.
    fn parse_ternary_conditional(&mut self) -> Expression<'a> {
        let expr = self.parse_binop();

        if maybe_next!(self, TokenKind::QuestionMark) {
            let if_true = Box::new(self.parse_expression());
            self.expect_token(TokenKind::Colon);
            let if_false = Box::new(self.parse_ternary_conditional());

            Expression::TernaryConditional {
                controlling: Box::new(expr),
                if_true,
                if_false,
            }
        } else {
            expr
        }
    }

    /// Parses a operand, otherwise known as a factor.
    ///
    /// Parses the `6.5.3 unary-expression` grammar as per Annex A of the N1548
    /// draft of the C11 standard. Note that this function also parses
    /// `6.5.1 primary-expression` at the same level, currently this difference
    /// is irrelevant.
    fn parse_operand(&mut self) -> Expression<'a> {
        let tok = self.next_kind();

        match tok {
            // integer literals
            TokenKind::Literal(val) => Expression::Literal { val: val.try_into().unwrap() },

            // FIXME: is this best?
            // Unary operations
            kind if let Some(unary) = match kind {
                TokenKind::Minus => Some(UnaryOp::Negation),
                TokenKind::Tilde => Some(UnaryOp::BitwiseComplement),
                TokenKind::Exclamation => Some(UnaryOp::LogicalNegation),
    
                _ => None,
            } => Expression::UnaryOp { expr: Box::new(self.parse_operand()), op: unary },

            // Expression wrapped in parens: `(5 + 5)`
            TokenKind::OpenParen => {
                let expr = self.parse_expression();
                self.expect_token(TokenKind::CloseParen);
                expr
            },

            TokenKind::Ident(ident) => {
                // Function calls
                if maybe_next!(self, TokenKind::OpenParen) {
                    let args = self.parse_function_call_args();

                    self.expect_token(TokenKind::CloseParen);

                    Expression::FunctionCall { identifier: ident, args }
                } else {
                    // Variable reference
                    Expression::Variable { identifier: self.get_variable_id(ident) }
                }
            }

            _ => SpannedError::without_span("expected <factor>").emit(),
        }
    }

    /// Parses the arguments for a function *call*.
    /// 
    /// Parses the `6.5.2 argument-expression-list` grammar as per Annex A of
    /// the N1548 draft of the C11 standard.
    fn parse_function_call_args(&mut self) -> Vec<Expression<'a>> {
        let mut args = Vec::new();

        while !matches!(self.input.peek().map(|t| &t.kind), Some(TokenKind::CloseParen)) {
            args.push(self.parse_expression());

            if !matches!(self.input.peek().map(|t| &t.kind), Some(TokenKind::Comma)) {
                break;
            }

            self.expect_token(TokenKind::Comma);
        }

        args
    }
}

macro_rules! impl_binop {
    (enum binary_op_levels {$( $lvl:literal = $match:pat, )+}; match tokens_to_binary_ops {$($token:pat => $op:expr,)*} ) => {
        impl<'a, I: ::std::iter::Iterator<Item = ::rcc_lexer::Token<'a>>> crate::Parser<'a, I> {
            fn map_tok_to_op(tok: &::rcc_lexer::TokenKind) -> Option<::rcc_structures::BinOp> {
                match tok {
                    $($token => Some($op),)*
                    _ => None
                }
            }

            /// Parses a binary operation.
            /// 
            /// Parses the `6.5.14 logical-OR-expression` grammar as per
            /// Annex A of the N1548 draft of the C11 standard.
            fn parse_binop(&mut self) -> crate::ast::Expression<'a> {
                self.parse_binop_impl(10)
            }

            fn parse_binop_impl(&mut self, lvl: u8) -> crate::ast::Expression<'a> {
                if lvl == 0 {
                    return self.parse_operand();
                }

                let mut lhs = self.parse_binop_impl(lvl - 1);

                match lvl {
                    $($lvl => {
                        while let Some(op) = Self::map_tok_to_op(self.peek_kind()) && matches!(op, $match) {
                            self.drop_next();

                            let rhs = self.parse_binop_impl(lvl - 1);

                            lhs = crate::ast::Expression::BinOp { lhs: Box::new(lhs), rhs: Box::new(rhs), op }
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
    enum binary_op_levels {
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
    match tokens_to_binary_ops {
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
