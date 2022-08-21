use std::{collections::HashMap, fmt::Write};

use crate::parser::{
    ast::{Program, Statement},
    expression::{BinOperator, Expression, UnaryOperator},
};

pub struct Generator<'a> {
    input: &'a Program,
    output: String,
    label_id: u32,
    variables: HashMap<&'a String, u32>, // name and offset
    stack_index: u32,
}

impl<'a> Generator<'a> {
    pub fn new(input: &'a Program) -> Self {
        Self {
            input,
            output: String::new(),
            label_id: 0,
            variables: HashMap::new(),
            stack_index: 0,
        }
    }

    pub fn gen_asm(mut self) -> String {
        self.write_fn_pre(&self.input.0.name);
        for statement in &self.input.0.statements {
            self.write_statement(statement);
        }

        self.output.push_str("movl $0, %eax\n");
        self.write_fn_pro();

        self.output
    }

    fn write_fn_pre(&mut self, name: &str) {
        self.output.push_str(&format!(
            ".globl {name}\n\
            {name}:\n\
            push %ebp\n\
            movl %esp, %ebp\n"
        ));
    }

    fn write_fn_pro(&mut self) {
        self.output.push_str(&format!(
            "movl %ebp, %esp\n\
            pop %ebp\n\
            ret\n"
        ))
    }

    fn write_statement(&mut self, statement: &'a Statement) {
        if let Statement::Return(exp) = statement {
            self.write_expression(exp);

            self.write_fn_pro();
        } else if let Statement::Declare(name, exp) = statement {
            if self.variables.contains_key(name) {
                panic!("Tried to declare variable twice!");
            }
            if exp.is_some() {
                self.write_expression(&exp.as_ref().unwrap());
            } else {
                self.output.push_str("movl $0, %eax\n");
            }
            self.output.push_str("pushl %eax\n");
            self.stack_index += 4;
            self.variables.insert(&name, self.stack_index);
        } else if let Statement::Expression(exp) = statement {
            self.write_expression(exp);
        }
    }

    fn write_expression(&mut self, exp: &Expression) {
        match exp {
            Expression::Constant(int) => self.output.push_str(&format!("movl ${}, %eax\n", int)),
            Expression::UnaryOp(op, exp) => {
                self.write_expression(exp);
                match op {
                    UnaryOperator::Negation => self.output.push_str("neg %eax\n"),
                    UnaryOperator::BitwiseComplement => self.output.push_str("not %eax\n"),
                    UnaryOperator::LogicalNegation => self.output.push_str(
                        "cmpl $0, %eax\n\
                        movl $0, %eax\n\
                        sete %al\n",
                    ),
                }
            }
            Expression::BinaryOp(op, exp1, exp2) => {
                self.write_expression(exp1);
                if op == &BinOperator::LogicalOR || op == &BinOperator::LogicalAND {
                    self.write_logical_exp(op, exp2);
                    return;
                }
                self.output.push_str("push %eax\n");
                self.write_expression(exp2);
                self.write_binop(op);
            }
            Expression::Assign(name, exp) => {
                self.write_expression(exp);
                if !self.variables.contains_key(name) {
                    panic!("undefined variable!");
                }
                let offset = self.variables.get(name).unwrap();
                self.output
                    .push_str(&format!("movl %eax, -{}(%ebp)\n", offset));
            }
            Expression::Variable(name) => {
                if !self.variables.contains_key(name) {
                    panic!("undefined variable!");
                }
                let offset = self.variables.get(name).unwrap();
                self.output
                    .push_str(&format!("movl -{}(%ebp), %eax\n", offset));
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
