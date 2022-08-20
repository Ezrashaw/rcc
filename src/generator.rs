use std::fmt::format;

use crate::parser::{
    ast::{Program, ReturnStatement},
    expression::{BinOperator, Expression, Factor, Term, UnaryOperator},
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

    fn write_expression(&mut self, expression: &Expression) {
        self.write_term(&expression.term);

        for i in &expression.nodes {
            self.write_binop_term(&i.1, &i.0);
        }
    }

    fn write_term(&mut self, term: &Term) {
        self.write_factor(&term.factor);

        for i in &term.nodes {
            self.write_binop_factor(&i.1, &i.0);
        }
    }

    fn write_factor(&mut self, factor: &Factor) {
        match &factor {
            Factor::Constant(val) => {
                self.output.push_str(&format!("movl ${}, %eax\n", val));
            }
            Factor::UnaryOp { operator, factor } => match operator {
                UnaryOperator::Negation => {
                    self.write_factor(&factor);
                    self.output.push_str(&format!("neg %eax\n"));
                }
                UnaryOperator::BitwiseComplement => {
                    self.write_factor(&factor);
                    self.output.push_str(&format!("not %eax\n"));
                }
                UnaryOperator::LogicalNegation => {
                    self.write_factor(&factor);
                    self.output.push_str(&format!(
                        "cmpl  $0, %eax\n\
                        movl   $0, %eax\n\
                        sete   %al\n"
                    ));
                }
            },
            Factor::Expression(exp) => self.write_expression(&exp),
        }
    }

    fn write_binop_term(&mut self, term: &Term, op: &BinOperator) {
        self.output.push_str(&format!("push %eax\n"));
        self.write_term(&term);
        self.write_binop(&op);
    }

    fn write_binop_factor(&mut self, factor: &Factor, op: &BinOperator) {
        self.output.push_str(&format!("push %eax\n"));
        self.write_factor(&factor);
        self.write_binop(&op);
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
