// TODO: make `ret` simply jump to the end of the function (allowing us to collapse stack frame logic)
// this should allow minimal code overhead for returns

// example (psuedo-code C/asm):
// fn main() -> int {
//    DO SOMETHING that sets EAX
//    return/ jump end
//    clear EAX
//end:
//    stack frame stuff
//    ret

// TODO: fix the lifetimes!

use std::collections::HashMap;

use crate::parser::{
    ast::{BlockItem, Function, Program, Statement},
    expression::{BinOperator, Expression, UnaryOperator},
};

pub struct Generator<'a> {
    input: &'a Program,
    output: String,
    label_id: u32,
    stack_index: usize,
}

impl<'a> Generator<'a> {
    pub fn new(input: &'a Program) -> Self {
        Self {
            input,
            output: String::new(),
            label_id: 0,
            stack_index: 4,
        }
    }

    pub fn gen_asm(mut self) -> String {
        for function in &self.input.0 {
            self.write_fn_def(function);
        }
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
        self.output.push_str(
            "movl %ebp, %esp\n\
            pop %ebp\n\
            ret\n",
        )
    }

    fn write_fn_def(&mut self, function: &'a Function) {
        if let Some(block) = &function.block {
            self.write_fn_pre(&function.name);

            self.write_block(&block, &mut HashMap::new());

            self.output.push_str("movl $0, %eax\n");
            self.write_fn_pro();
        }
    }

    fn write_block(&mut self, block: &'a Vec<BlockItem>, vars: &mut HashMap<&'a String, usize>) {
        let mut current_scope = HashMap::new();

        for item in block {
            self.write_block_item(item, vars, &mut current_scope);
        }

        let bytes_to_dealloc = 4 * current_scope.len();
        self.output
            .push_str(&format!("addl ${}, %esp\n", bytes_to_dealloc));
        self.stack_index -= bytes_to_dealloc;
    }

    fn write_block_item(
        &mut self,
        item: &'a BlockItem,
        vars: &mut HashMap<&'a String, usize>,
        current_scope: &mut HashMap<&'a String, usize>,
    ) {
        if let BlockItem::Statement(statement) = item {
            self.write_statement(statement, vars);
        } else if let BlockItem::Declaration(name, exp) = item {
            if current_scope.contains_key(&name) {
                panic!("Tried to declare variable twice!");
            }
            if exp.is_some() {
                self.write_expression(exp.as_ref().unwrap(), vars);
            } else {
                self.output.push_str("movl $0, %eax\n");
            }
            self.output.push_str("pushl %eax\n");
            vars.insert(name, self.stack_index);
            current_scope.insert(name, self.stack_index);
            self.stack_index += 4;
        }
    }

    fn write_statement(&mut self, statement: &'a Statement, vars: &mut HashMap<&'a String, usize>) {
        if let Statement::Return(exp) = statement {
            self.write_expression(exp, vars);

            self.write_fn_pro();
        } else if let Statement::Expression(exp) = statement {
            self.write_expression(exp, vars);
        } else if let Statement::Conditional(cntrl, state_true, state_false) = statement {
            if let Some(state_false) = state_false {
                let state_false = Some(state_false.as_ref()); // wtf is this, we rewrap the Option????
                self.write_conditional(cntrl, state_true, state_false, vars);
            } else {
                self.write_conditional(cntrl, state_true, None, vars);
            }
        } else if let Statement::Compound(block) = statement {
            self.write_block(block, &mut vars.clone());
        }
    }

    fn write_expression(&mut self, exp: &'a Expression, vars: &mut HashMap<&'a String, usize>) {
        match exp {
            Expression::Constant(int) => self.output.push_str(&format!("movl ${}, %eax\n", int)),
            Expression::UnaryOp(op, exp) => {
                self.write_expression(exp, vars);
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
                self.write_expression(exp1, vars);
                if op == &BinOperator::LogicalOR || op == &BinOperator::LogicalAND {
                    self.write_logical_exp(op, exp2, vars);
                    return;
                }
                self.output.push_str("push %eax\n");
                self.write_expression(exp2, vars);
                self.write_binop(op);
            }
            Expression::Assign(name, exp) => {
                self.write_expression(exp, vars);
                if !vars.contains_key(name) {
                    panic!("undefined variable!");
                }
                let offset = vars.get(name).unwrap();
                self.output
                    .push_str(&format!("movl %eax, -{}(%ebp)\n", offset));
            }
            Expression::Variable(name) => {
                if !vars.contains_key(name) {
                    panic!("undefined variable!");
                }
                let offset = vars.get(name).unwrap();
                self.output
                    .push_str(&format!("movl -{}(%ebp), %eax\n", offset));
            }
            Expression::Conditional(exp, e1, e2) => {
                self.write_ternary_conditional(exp, e1, e2, vars)
            }
            Expression::FunCall(..) => todo!(),
        }
    }

    fn write_ternary_conditional(
        &mut self,
        cntrl: &'a Expression,
        e1: &'a Expression,
        e2: &'a Expression,
        vars: &mut HashMap<&'a String, usize>,
    ) {
        let start_id = self.label_id;
        self.label_id += 2;

        self.write_expression(cntrl, vars);
        self.output.push_str(&format!(
            "cmpl $0, %eax\n\
            je _{start_id}\n"
        ));
        self.write_expression(e1, vars);
        self.output.push_str(&format!(
            "jmp _{}\n\
            _{start_id}:\n",
            start_id + 1
        ));
        self.write_expression(e2, vars);
        self.output.push_str(&format!("_{}:\n", start_id + 1));
    }

    fn write_conditional(
        &mut self,
        cntrl: &'a Expression,
        state_true: &'a Statement,
        state_false: Option<&'a Statement>,
        vars: &mut HashMap<&'a String, usize>,
    ) {
        let start_id = self.label_id;
        self.label_id += if state_false.is_some() { 2 } else { 1 };

        self.write_expression(cntrl, vars);
        if state_false.is_some() {
            self.output.push_str(&format!(
                "cmpl $0, %eax\n\
                je _{start_id}\n"
            ));

            self.write_statement(state_true, vars);
            self.output.push_str(&format!(
                "jmp _{}\n\
                _{start_id}:\n",
                start_id + 1
            ));
            self.write_statement(state_false.unwrap(), vars);

            self.output.push_str(&format!("_{}:\n", start_id + 1));
        } else {
            self.output.push_str(&format!(
                "cmpl $0, %eax\n\
                je _{start_id}\n"
            ));

            self.write_statement(state_true, vars);

            self.output.push_str(&format!("_{}:\n", start_id));
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

    fn write_logical_exp(
        &mut self,
        op: &'a BinOperator,
        exp: &'a Expression,
        vars: &mut HashMap<&'a String, usize>,
    ) {
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

            self.write_expression(exp, vars);
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

            self.write_expression(exp, vars);
            self.output.push_str(&format!(
                "cmpl $0, %eax\n\
                movl $0, %eax\n\
                setne %al\n\
                _{id_end}:\n"
            ));
        }
    }
}
