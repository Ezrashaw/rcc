use rcc_parser::ast::{Expression, Function, Statement};

#[derive(Debug, Clone)]
pub enum Instruction {
    LoadInt(u32),
    Return,
}

impl Instruction {
    pub(crate) fn from_function(function: &Function) -> Vec<Self> {
        let mut buf = Vec::new();

        Self::from_statement(&mut buf, &function.statement);

        buf
    }

    fn from_statement(buf: &mut Vec<Self>, statement: &Statement) {
        match statement {
            Statement::Return(val) => {
                Self::from_expression(buf, val);
                buf.push(Instruction::Return);
            }
        }
    }

    fn from_expression(buf: &mut Vec<Self>, expression: &Expression) {
        match expression {
            Expression::Literal { val } => buf.push(Instruction::LoadInt(*val)),
        }
    }
}
