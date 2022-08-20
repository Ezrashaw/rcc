use std::fmt;

use crate::ctypes::{CInteger, CType};

use super::expression::{BinOperator, Expression, UnaryOperator};

pub struct Program(pub Function);

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub return_type: CType,
    pub statements: Vec<ReturnStatement>,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub ret_val: Expression,
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fun = &self.0;
        writeln!(f, "fn {}() -> {:?}", fun.name, fun.return_type)?;
        write!(f, "    return ")?;
        let exp = &fun.statements[0].ret_val;
        Self::write_exp(f, exp)?;

        Ok(())
    }
}

impl Program {
    fn write_exp(f: &mut fmt::Formatter<'_>, exp: &Expression) -> fmt::Result {
        match exp {
            Expression::BinaryOp(op, exp1, exp2) => {
                Self::write_exp(f, &exp1)?;
                Self::write_binop(f, &op)?;
                Self::write_exp(f, &exp2)?;
            }
            Expression::UnaryOp(op, exp) => {
                Self::write_unop(f, &op)?;
                Self::write_exp(f, &exp)?;
            }
            Expression::Constant(int) => write!(f, "{int}")?,
        }

        Ok(())
    }

    fn write_binop(f: &mut fmt::Formatter<'_>, binop: &BinOperator) -> fmt::Result {
        write!(
            f,
            " {} ",
            match binop {
                &BinOperator::Addition => '+',
                &BinOperator::Subtraction => '-',
                &BinOperator::Multiplication => '*',
                &BinOperator::Division => '/',
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
