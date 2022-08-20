use crate::parser::{
    ast::{Program, ReturnStatement},
    expression::{BinOperator, Expression, UnaryOperator},
};

pub struct Generator<'a> {
    input: &'a Program,
    output: String,
}

impl<'a> Generator<'a> {
    pub fn new(input: &'a Program) -> Self {
        Self {
            input,
            output: String::new(),
        }
    }

    pub fn gen_asm(mut self) -> String {
        self.write_fn_def(&self.input.0.name);
        self.write_return_statement(&self.input.0.statements[0]);

        self.output
    }

    fn write_fn_def(&mut self, name: &str) {
        self.output.push_str(&format!(
            ".globl {name}\n\
            {name}:\n"
        ));
    }

    fn write_return_statement(&mut self, statement: &ReturnStatement) {
        self.write_expression(&statement.ret_val);

        self.output.push_str(&format!("ret\n"));
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
                self.output.push_str(&format!("push %eax\n"));
                self.write_expression(&exp2);
                self.write_binop(op);
            }
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
        } else {
            self.output.push_str(&format!(
                "pop %ecx\n\
            {} %ecx, %eax\n",
                match op {
                    BinOperator::Addition => "addl",
                    BinOperator::Multiplication => "imul",
                    _ => panic!("Unknown binary operation!"),
                }
            ));
        }
    }
}
