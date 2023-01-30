use rcc_parser::ast::{BlockItem, Expression, Program, Statement};
use rcc_structures::{BinOp, UnaryOp};

pub struct ConstantFolder<'a> {
    ast: Program<'a>,
}

impl<'a> ConstantFolder<'a> {
    pub fn new(ast: Program<'a>) -> Self {
        Self { ast }
    }

    pub fn optimize(mut self) -> Program<'a> {
        for item in &mut self.ast.function.block.block_items {
            match item {
                BlockItem::Statement(Statement::Return(ref mut expr)) => {
                    Self::constify_expr_or_inner(expr)
                }
                BlockItem::Statement(Statement::Expression(ref mut expr)) => {
                    Self::constify_expr_or_inner(expr)
                }

                BlockItem::Declaration(_, Some(ref mut expr)) => Self::constify_expr_or_inner(expr),

                _ => (),
            }
        }

        // remove useless statements
        self.ast.function.block.block_items.retain(|stmt| {
            !matches!(
                stmt,
                BlockItem::Statement(Statement::Expression(Expression::Literal { .. }))
            )
        });

        self.ast
    }

    /// Implementation:
    /// 1. take the expression and decide whether it is constant.
    ///    a. If it is, then overwrite the expression with the value.
    ///    b. If it isn't, then some sub-expressions might be constant, recurse on them.
    fn constify_expr_or_inner(expr: &mut Expression) {
        if let Some(val) = Self::get_const_value(expr) {
            *expr = Expression::Literal { val }
        } else {
            match expr {
                Expression::BinOp { lhs, rhs, .. } => {
                    Self::constify_expr_or_inner(lhs);
                    Self::constify_expr_or_inner(rhs);
                }
                Expression::UnaryOp { expr, .. } => Self::constify_expr_or_inner(expr),

                Expression::Assignment { expression, .. } => {
                    Self::constify_expr_or_inner(expression)
                }

                Expression::TernaryConditional {
                    controlling,
                    if_true,
                    if_false,
                } => {
                    Self::constify_expr_or_inner(controlling);
                    Self::constify_expr_or_inner(if_true);
                    Self::constify_expr_or_inner(if_false);
                }

                _ => (),
            }
        }
    }

    fn get_const_value(expr: &Expression) -> Option<i32> {
        match expr {
            Expression::BinOp { lhs, rhs, op, .. } => {
                let lhs = Self::get_const_value(lhs)?;
                let rhs = Self::get_const_value(rhs)?;

                match op {
                    BinOp::Add => Some(lhs + rhs),
                    BinOp::Sub => Some(lhs - rhs),
                    BinOp::Mul => Some(lhs * rhs),
                    BinOp::Div => Some(lhs / rhs),
                    BinOp::LogicalOr => Some(((lhs != 0) || (rhs != 0)) as i32),
                    BinOp::LogicalAnd => Some(((lhs != 0) && (rhs != 0)) as i32),
                    BinOp::Equals => Some((lhs == rhs) as i32),
                    BinOp::NotEquals => Some((lhs != rhs) as i32),
                    BinOp::LessThan => Some((lhs < rhs) as i32),
                    BinOp::LessThanOrEquals => Some((lhs <= rhs) as i32),
                    BinOp::GreaterThan => Some((lhs > rhs) as i32),
                    BinOp::GreaterThanOrEquals => Some((lhs >= rhs) as i32),
                }
            }
            Expression::UnaryOp { expr, op } => match op {
                UnaryOp::Negation => Some(-Self::get_const_value(expr)?),
                UnaryOp::BitwiseComplement => Some(!Self::get_const_value(expr)?),
                UnaryOp::LogicalNegation => Some(if Self::get_const_value(expr)? == 0 {
                    1
                } else {
                    0
                }),
            },
            Expression::Literal { val } => Some(*val as i32),
            Expression::Assignment { .. } => None,
            Expression::Variable { .. } => None,
            Expression::TernaryConditional {
                controlling,
                if_true,
                if_false,
            } => {
                let controlling = Self::get_const_value(controlling)? != 0;

                if controlling {
                    Some(Self::get_const_value(if_true)?)
                } else {
                    Some(Self::get_const_value(if_false)?)
                }
            }
        }
    }
}
