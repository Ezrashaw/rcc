use std::fmt;

use crate::ctypes::CType;

use super::expression::{BinOperator, Expression, UnaryOperator};

pub struct Program(pub Function);

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub return_type: CType,
    pub block: Vec<BlockItem>,
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    Conditional(Expression, Box<Statement>, Option<Box<Statement>>), // controlling condition, then true, then false
}

#[derive(Debug)]
pub enum BlockItem {
    Declaration(String, Option<Expression>), // var name, var
    Statement(Statement),
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fun = &self.0;
        write!(f, "fn {}() -> {:?}\n\t", fun.name, fun.return_type)?;
        for block_item in &fun.block {
            match block_item {
                BlockItem::Declaration(name, value) => {
                    write!(f, "INT {}", name)?;
                    if value.is_some() {
                        write!(f, " = ")?;
                        Self::write_exp(f, value.as_ref().unwrap())?;
                    }
                }
                BlockItem::Statement(statement) => Self::write_statement(f, statement)?,
            }
        }

        Ok(())
    }
}

impl Program {
    fn write_statement(f: &mut fmt::Formatter<'_>, statement: &Statement) -> fmt::Result {
        match statement {
            Statement::Return(exp) => {
                write!(f, "RETURN ")?;
                Self::write_exp(f, exp)?;
            }
            Statement::Expression(exp) => Self::write_exp(f, exp)?,
            Statement::Conditional(_, _, _) => todo!(),
        }

        write!(f, "\n\t")
    }

    fn write_exp(f: &mut fmt::Formatter<'_>, exp: &Expression) -> fmt::Result {
        match exp {
            Expression::BinaryOp(op, exp1, exp2) => {
                Self::write_exp(f, exp1)?;
                Self::write_binop(f, op)?;
                Self::write_exp(f, exp2)?;
            }
            Expression::UnaryOp(op, exp) => {
                Self::write_unop(f, op)?;
                Self::write_exp(f, exp)?;
            }
            Expression::Constant(int) => write!(f, "{int}")?,
            Expression::Assign(name, exp) => {
                write!(f, "{} = ", name)?;
                Self::write_exp(f, exp)?;
            }
            Expression::Variable(name) => write!(f, "{}", name)?,
            Expression::Conditional(_, _, _) => todo!(),
        }

        Ok(())
    }

    fn write_binop(f: &mut fmt::Formatter<'_>, binop: &BinOperator) -> fmt::Result {
        write!(
            f,
            " {} ",
            match binop {
                &BinOperator::Addition => "+",
                &BinOperator::Subtraction => "-",
                &BinOperator::Multiplication => "*",
                &BinOperator::Division => "/",
                BinOperator::LessThan => "<",
                BinOperator::GreaterThan => ">",
                BinOperator::LessThanOrEqual => "<=",
                BinOperator::GreaterThanOrEqual => ">=",
                BinOperator::Equal => "==",
                BinOperator::NotEqual => "!=",
                BinOperator::LogicalAND => "&&",
                BinOperator::LogicalOR => "||",
            }
        )
    }

    fn write_unop(f: &mut fmt::Formatter<'_>, unop: &UnaryOperator) -> fmt::Result {
        write!(
            f,
            "{}",
            match unop {
                UnaryOperator::Negation => '-',
                UnaryOperator::BitwiseComplement => '~',
                UnaryOperator::LogicalNegation => '!',
            }
        )
    }
}
