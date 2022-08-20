use std::fmt;

use crate::ctypes::{CInteger, CType};

use super::expression::{BinOperator, Expression, Factor, Term, UnaryOperator};

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
        Self::write_term(f, &exp.term)?;

        for term in &exp.nodes {
            Self::write_binop(f, &term.0)?;
            Self::write_term(f, &term.1)?;
        }

        Ok(())
    }

    fn write_term(f: &mut fmt::Formatter<'_>, term: &Term) -> fmt::Result {
        write!(f, "(")?;
        Self::write_factor(f, &term.factor)?;

        for factor in &term.nodes {
            Self::write_binop(f, &factor.0)?;
            Self::write_factor(f, &factor.1)?;
        }

        write!(f, ")")?;
        Ok(())
    }

    fn write_factor(f: &mut fmt::Formatter<'_>, factor: &Factor) -> fmt::Result {
        match factor {
            Factor::Expression(exp) => {
                Self::write_exp(f, exp)?;
            }
            Factor::Constant(int) => write!(f, "{int}")?,
            Factor::UnaryOp { operator, factor } => {
                Self::write_unop(f, operator)?;
                Self::write_factor(f, factor)?;
            }
        };

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
