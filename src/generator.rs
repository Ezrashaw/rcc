use crate::parser::{
    ast::{Program, Statement},
    expression::{BinOperator, Expression, UnaryOperator},
};

pub struct Generator<'a> {
    input: &'a Program,
    output: String,
    label_id: u32,
}

impl<'a> Generator<'a> {
    pub fn new(input: &'a Program) -> Self {
        Self {
            input,
            output: String::new(),
            label_id: 0,
        }
    }

    pub fn gen_asm(mut self) -> String {
        self.write_fn_def(&self.input.0.name);
        for statement in &self.input.0.statements {
            self.write_statement(statement);
        }

        self.output
    }

    fn write_fn_def(&mut self, name: &str) {
        self.output.push_str(&format!(
            ".globl {name}\n\
            {name}:\n"
        ));
    }

    fn write_statement(&mut self, statement: &Statement) {
        if let Statement::Return(exp) = statement {
            self.write_expression(exp);

            self.output.push_str(&format!("ret\n"));
        } else {
            todo!()
        }
    }

    fn write_expression(&mut self, exp: &Expression) {
        match exp {
            Expression::Constant(int) => self.output.push_str(&format!("movl ${}, %eax\n", int)),
            Expression::UnaryOp(op, exp) => {
                self.write_expression(exp);
                match op {
                    UnaryOperator::Negation => self.output.push_str(&format!("neg %eax\n")),
                    UnaryOperator::BitwiseComplement => {
                        self.output.push_str(&format!("not %eax\n"))
                    }
                    UnaryOperator::LogicalNegation => self.output.push_str(&format!(
                        "cmpl  $0, %eax\n\
                        movl   $0, %eax\n\
                        sete   %al\n"
                    )),
                }
            }
            Expression::BinaryOp(op, exp1, exp2) => {
                self.write_expression(&exp1);
                if op == &BinOperator::LogicalOR || op == &BinOperator::LogicalAND {
                    self.write_logical_exp(op, exp2);
                    return;
                }
                self.output.push_str(&format!("push %eax\n"));
                self.write_expression(&exp2);
                self.write_binop(op);
            }
            Expression::Assign(_, _) => todo!(),
            Expression::Variable(_) => todo!(),
        }
    }

    fn write_binop(&mut self, op: &BinOperator) {
        if op == &BinOperator::Subtraction || op == &BinOperator::Division {
            self.output.push_str(&format!(
                "movl %eax, %ecx\n\
                pop %eax\n{}",
                match op {
                    &BinOperator::Subtraction => "subl %ecx, %eax\n",
                    &BinOperator::Division =>
                        "cdq\n\
                        idivl %ecx\n",
                    _ => panic!("Maths is wrong!"),
                }
            ));
        } else if op == &BinOperator::Addition || op == &BinOperator::Multiplication {
            self.output.push_str(&format!(
                "pop %ecx\n\
            {} %ecx, %eax\n",
                match op {
                    BinOperator::Addition => "addl",
                    BinOperator::Multiplication => "imul",
                    _ => panic!("Unknown binary operation!"),
                }
            ));
        } else if op == &BinOperator::Equal
            || op == &BinOperator::NotEqual
            || op == &BinOperator::LessThan
            || op == &BinOperator::GreaterThan
            || op == &BinOperator::GreaterThanOrEqual
            || op == &BinOperator::LessThanOrEqual
        {
            self.output.push_str(&format!(
                "pop %ecx\n\
                cmpl %eax, %ecx\n\
                movl $0, %eax\n\
                {} %al\n",
                match op {
                    BinOperator::NotEqual => "setne",
                    BinOperator::Equal => "sete",
                    BinOperator::GreaterThan => "setg",
                    BinOperator::GreaterThanOrEqual => "setge",
                    BinOperator::LessThan => "setl",
                    BinOperator::LessThanOrEqual => "setle",
                    _ => panic!("Unknown binary operation!"),
                }
            ));
        }
    }

    fn write_logical_exp(&mut self, op: &BinOperator, exp: &Expression) {
        let id_clause2 = self.label_id;
        let id_end = id_clause2 + 1;
        self.label_id += 2;
        if op == &BinOperator::LogicalOR {
            self.output.push_str(&format!(
                "cmpl $0, %eax\n\
                je _{id_clause2}\n\
                movl $1, %eax\n\
                jmp _{id_end}\n\
                _{id_clause2}:\n"
            ));

            self.write_expression(exp);
            self.output.push_str(&format!(
                "cmpl $0, %eax\n\
                movl $0, %eax\n\
                setne %al\n\
                _{id_end}:"
            ));
        } else if op == &BinOperator::LogicalAND {
            self.output.push_str(&format!(
                "cmpl $0, %eax\n\
                jne _{id_clause2}\n\
                jmp _{id_end}\n\
                _{id_clause2}:\n"
            ));

            self.write_expression(exp);
            self.output.push_str(&format!(
                "cmpl $0, %eax\n\
                movl $0, %eax\n\
                setne %al\n\
                _{id_end}:\n"
            ));
        }
    }
}
