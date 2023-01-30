use rcc_parser::ast::Expression;
use rcc_structures::{BinOp, UnaryOp};

use crate::OptimizationPass;

pub struct ConstantFolder;

impl OptimizationPass for ConstantFolder {
    fn opt_expression(&mut self, expr: &mut Expression) {
        if let Some(val) = Self::get_const_value(expr) {
            *expr = Expression::Literal { val }
        }
    }
}

impl ConstantFolder {
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
