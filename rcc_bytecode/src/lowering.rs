use rcc_parser::ast::{Block, BlockItem, Expression, Statement};
use rcc_structures::BinOp;

use crate::{Bytecode, Instruction, ReadLocation};

/// Defines `append_from_*` methods, used internally to lower from AST.
impl Bytecode<'_> {
    pub(crate) fn append_from_block(&mut self, block: &Block) {
        for item in &block.block_items {
            self.append_from_block_item(item);
        }

        // ensure that we don't leak registers.
        // FIXME: Rust's memory model could probably help with this.
        if !self.allocated_registers.is_empty() {
            println!("~~~~ BYTECODE DUMP ~~~~");
            for instruction in &self.instr {
                println!("{instruction:?}");
            }
            println!("~~~~~~~~~~~~~~~~~~~~~~~");
            panic!("internal *bug*: we are leaking registers")
        }
    }

    fn append_from_block_item(&mut self, item: &BlockItem) {
        match item {
            BlockItem::Declaration(id, val) => self.append_from_declaration(id, val),
            BlockItem::Statement(stmt) => self.append_from_statement(stmt),
        }
    }

    fn append_from_declaration(&mut self, id: &u32, init: &Option<Expression>) {
        let reg = if let Some(init) = init {
            self.append_from_expression(init)
        } else {
            // If a variable is declared but not initialized, then we can just
            // initialize to 0. The standard does not specify a value for this
            // situation. Ahh, the wonders of C.
            ReadLocation::Constant(0)
        };

        // aarch64 limitations
        let reg = self.upgrade_readable(reg).downgrade();

        self.append_instruction(Instruction::AssignVariable(*id, reg.clone()));
        self.dealloc_reg(reg);
    }

    fn append_from_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Return(expr) => {
                let reg = self.append_from_expression(expr);
                self.append_instruction(Instruction::Return(reg.clone()));

                self.dealloc_reg(reg);
            }
            Statement::Expression(expr) => {
                if let Some(expr) = expr {
                    let reg = self.append_from_expression(expr);
                    self.dealloc_reg(reg);
                }
            }
            Statement::Conditional(expr, true_branch, false_branch) => {
                self.append_from_conditional(expr, true_branch, false_branch.as_deref());
            }
            Statement::Compound(block) => self.append_from_block(block),
            Statement::While(controlling, body) => {
                let evaluate_label = self.label_counter;
                let post_label = self.label_counter + 1;
                self.label_counter += 2;

                self.loop_start.push(evaluate_label);
                self.loop_end.push(post_label);

                self.append_instruction(Instruction::JumpDummy(evaluate_label));

                let controlling_loc = self.append_from_expression(controlling);
                let controlling_loc = self.upgrade_readable(controlling_loc);

                self.append_instruction(Instruction::CompareJump(
                    controlling_loc.clone(),
                    false,
                    post_label,
                ));

                self.dealloc_reg(controlling_loc.downgrade());

                self.append_from_statement(body);
                self.append_instruction(Instruction::UnconditionalJump(evaluate_label));
                self.append_instruction(Instruction::JumpDummy(post_label));

                self.loop_start.pop();
                self.loop_end.pop();
            }
            Statement::Do(controlling, body) => {
                let pre_body = self.label_counter;
                // used solely for `break`.
                let post_loop = self.label_counter + 1;
                self.label_counter += 2;

                self.loop_start.push(pre_body);
                self.loop_end.push(post_loop);

                self.append_instruction(Instruction::JumpDummy(pre_body));

                self.append_from_statement(body);

                let controlling_loc = self.append_from_expression(controlling);
                let controlling_loc = self.upgrade_readable(controlling_loc);

                self.append_instruction(Instruction::CompareJump(
                    controlling_loc.clone(),
                    true,
                    pre_body,
                ));

                self.dealloc_reg(controlling_loc.downgrade());

                self.loop_start.pop();
                self.loop_end.pop();
            }
            Statement::For(init, condition, post, body) => {
                if let Some(init) = init {
                    let reg = self.append_from_expression(init);
                    self.dealloc_reg(reg);
                }

                self.append_from_for_loop(condition.as_ref(), post.as_ref(), body);
            }
            Statement::ForDecl(var_id, var_init, condition, post, body) => {
                self.append_from_declaration(var_id, var_init);

                self.append_from_for_loop(condition.as_ref(), post.as_ref(), body);
            }

            Statement::Break => {
                if let Some(post_loop) = self.loop_end.last() {
                    self.append_instruction(Instruction::UnconditionalJump(*post_loop));
                } else {
                    // FIXME: use the already established error-reporting methods.
                    panic!("`break` occured outside of loop!");
                }
            }
            Statement::Continue => {
                if let Some(pre_loop) = self.loop_start.last() {
                    self.append_instruction(Instruction::UnconditionalJump(*pre_loop));
                } else {
                    // FIXME: use the already established error-reporting methods.
                    panic!("`continue` occured outside of loop!");
                }
            }
        }
    }

    fn append_from_for_loop(
        &mut self,
        condition: Option<&Expression>,
        post: Option<&Expression>,
        body: &Statement,
    ) {
        // C requires that this expression must be a non-zero constant value if
        // the AST doesn't contain a expression.
        let condition = condition.unwrap_or(&Expression::Literal { val: 1 });

        let pre_condition = self.label_counter;
        let loop_continue = self.label_counter + 1;
        let post_loop = self.label_counter + 2;
        self.label_counter += 3;

        self.loop_start.push(loop_continue);
        self.loop_end.push(post_loop);

        self.append_instruction(Instruction::JumpDummy(pre_condition));
        let reg = self.append_from_expression(condition);
        let reg = self.upgrade_readable(reg);

        self.append_instruction(Instruction::CompareJump(reg.clone(), false, post_loop));
        self.dealloc_reg(reg.downgrade());

        self.append_from_statement(body);

        self.append_instruction(Instruction::JumpDummy(loop_continue));
        if let Some(post) = post {
            let reg = self.append_from_expression(post);
            self.dealloc_reg(reg);
        }

        self.append_instruction(Instruction::UnconditionalJump(pre_condition));
        self.append_instruction(Instruction::JumpDummy(post_loop));

        self.loop_start.pop();
        self.loop_end.pop();
    }

    /// Generates bytecode for the provided expression and appends it to the
    /// [`Bytecode`].
    ///
    /// This function will allocate and return a location for the
    /// result of the expression to be stored in. This does not preclude this
    /// function from allocating more intermediate registers.
    fn append_from_expression(&mut self, expr: &Expression) -> ReadLocation {
        match expr {
            Expression::BinOp { lhs, rhs, op, .. } => {
                let lhs = self.append_from_expression(lhs);
                let lhs = self.upgrade_readable(lhs);

                if let BinOp::LogicalAnd | BinOp::LogicalOr = op {
                    let label = self.label_counter;
                    self.label_counter += 1;

                    self.append_instruction(Instruction::CompareJump(
                        lhs.clone(),
                        *op == BinOp::LogicalOr,
                        label,
                    ));

                    self.dealloc_reg(ReadLocation::Writable(lhs));

                    // FIXME: hack: this assumes that we'll get the same register as before (certainly this isn't right).
                    let rhs = self.append_from_expression(rhs);
                    let rhs = self.upgrade_readable(rhs);
                    self.append_instruction(Instruction::BinaryBooleanOp(rhs.clone(), label));

                    return rhs.downgrade();
                }

                let rhs = self.append_from_expression(rhs);

                // aarch64 requires that the RHS for these binary operaors are in registers.
                let rhs = if let BinOp::Mul
                | BinOp::GreaterThan
                | BinOp::GreaterThanOrEquals
                | BinOp::LessThan
                | BinOp::LessThanOrEquals
                | BinOp::NotEquals
                | BinOp::Equals = op
                {
                    self.upgrade_readable(rhs).downgrade()
                } else {
                    rhs
                };

                // FIXME: hack because `idiv` on x86 cannot take immediate (constant) rhs values.
                let rhs = if let BinOp::Div = op {
                    self.upgrade_readable(rhs).downgrade()
                } else {
                    rhs
                };

                self.append_instruction(Instruction::BinaryOp(*op, lhs.clone(), rhs.clone()));

                self.dealloc_reg(rhs);
                lhs.downgrade()
            }
            Expression::UnaryOp { expr, op } => {
                let rloc = self.append_from_expression(expr);
                let wloc = self.upgrade_readable(rloc);

                self.append_instruction(Instruction::UnaryOp(*op, wloc.clone()));

                wloc.downgrade()
            }
            Expression::Literal { val } => ReadLocation::Constant(*val),
            Expression::Assignment {
                identifier,
                expression,
            } => {
                let reg = self.append_from_expression(expression);
                // aarch64 limitations
                let reg = self.upgrade_readable(reg).downgrade();

                self.append_instruction(Instruction::AssignVariable(*identifier, reg.clone()));

                reg
            }
            Expression::Variable { identifier } => {
                let reg = self.alloc_reg();
                self.append_instruction(Instruction::LoadVariable(*identifier, reg.clone()));

                reg.downgrade()
            }
            Expression::TernaryConditional {
                controlling,
                if_true,
                if_false,
            } => {
                // FIXME: copied from below, merge please
                let controlling = self.append_from_expression(controlling);

                // FIXME: you know the deal, x86 constraints. GAHH, it's also below.
                let controlling = self.upgrade_readable(controlling);

                // the first instructions to be encountered from here will read the reg
                // and redirect control flow; it is no longer needed.
                self.dealloc_reg(controlling.clone().downgrade());

                self.label_counter += 2;
                let post_else = self.label_counter - 1;
                let pre_else = self.label_counter - 2;

                self.append_instruction(Instruction::CompareJump(controlling, false, pre_else));

                let true_reg = self.append_from_expression(if_true);
                let true_reg = self.upgrade_readable(true_reg).downgrade();
                self.append_instruction(Instruction::UnconditionalJump(post_else));
                self.append_instruction(Instruction::JumpDummy(pre_else));

                // FIXME: this all assumes that the same reg is allocated, completely flawed.
                self.dealloc_reg(true_reg);

                let false_reg = self.append_from_expression(if_false);
                let false_reg = self.upgrade_readable(false_reg).downgrade();
                self.append_instruction(Instruction::JumpDummy(post_else));

                false_reg
            }
        }
    }

    fn append_from_conditional(
        &mut self,
        expr: &Expression,
        true_branch: &Statement,
        false_branch: Option<&Statement>,
    ) {
        let controlling = self.append_from_expression(expr);

        // FIXME: you know the deal, x86 constraints
        let controlling = self.upgrade_readable(controlling);

        // the first instructions to be encountered from here will read the reg
        // and redirect control flow; it is no longer needed.
        self.dealloc_reg(controlling.clone().downgrade());

        if let Some(false_branch) = false_branch {
            self.label_counter += 2;
            let post_else = self.label_counter - 1;
            let pre_else = self.label_counter - 2;

            self.append_instruction(Instruction::CompareJump(controlling, false, pre_else));

            self.append_from_statement(true_branch);
            self.append_instruction(Instruction::UnconditionalJump(post_else));
            self.append_instruction(Instruction::JumpDummy(pre_else));

            self.append_from_statement(false_branch);
            self.append_instruction(Instruction::JumpDummy(post_else));
        } else {
            self.label_counter += 1;
            let post_conditional = self.label_counter - 1;

            self.append_instruction(Instruction::CompareJump(
                controlling,
                false,
                post_conditional,
            ));

            self.append_from_statement(true_branch);
            self.append_instruction(Instruction::JumpDummy(post_conditional));
        }
    }
}
