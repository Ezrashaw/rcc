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
                    BinOp::Modulo => Some(lhs % rhs),
                    BinOp::LogicalOr => Some(i32::from((lhs != 0) || (rhs != 0))),
                    BinOp::LogicalAnd => Some(i32::from((lhs != 0) && (rhs != 0))),
                    BinOp::Equals => Some(i32::from(lhs == rhs)),
                    BinOp::NotEquals => Some(i32::from(lhs != rhs)),
                    BinOp::LessThan => Some(i32::from(lhs < rhs)),
                    BinOp::LessThanOrEquals => Some(i32::from(lhs <= rhs)),
                    BinOp::GreaterThan => Some(i32::from(lhs > rhs)),
                    BinOp::GreaterThanOrEquals => Some(i32::from(lhs >= rhs)),
                    BinOp::LeftShift => Some(i32::from(lhs << rhs)),
                    BinOp::RightShift => Some(i32::from(lhs >> rhs)),
                    BinOp::BitwiseOr => Some(i32::from(lhs | rhs)),
                    BinOp::ExclusiveOr => Some(i32::from(lhs ^ rhs)),
                    BinOp::BitwiseAnd => Some(i32::from(lhs & rhs)),
                }
            }
            Expression::UnaryOp { expr, op } => match op {
                UnaryOp::Negation => Some(-Self::get_const_value(expr)?),
                UnaryOp::BitwiseComplement => Some(!Self::get_const_value(expr)?),
                UnaryOp::LogicalNegation => Some(i32::from(Self::get_const_value(expr)? == 0)),
            },
            Expression::Literal { val } => Some(*val),
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
            Expression::Assignment { .. } | Expression::Variable { .. } => None,
            Expression::FunctionCall { .. } => todo!(),
        }
    }
}
