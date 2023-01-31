use rcc_parser::ast::{Block, BlockItem, Expression, Statement};
use rcc_structures::BinOp;

use crate::{Bytecode, Instruction, ReadLocation};

/// Defines `append_from_*` methods, used internally to lower from AST.
impl Bytecode<'_> {
    pub(crate) fn append_from_block(&mut self, block: &Block) {
        for item in &block.block_items {
            self.append_from_block_item(item);
        }
    }

    fn append_from_block_item(&mut self, item: &BlockItem) {
        match item {
            BlockItem::Declaration(id, val) => {
                let reg = if let Some(init) = val {
                    self.append_from_expression(init)
                } else {
                    // If a variable is declared but not initialized, then we can just
                    // initialize to 0. The standard does not specify a value for this
                    // situation. Ahh, the wonders of C.
                    ReadLocation::Constant(0)
                };

                self.append_instruction(Instruction::AssignVariable(*id, reg));
            }
            BlockItem::Statement(stmt) => self.append_from_statement(stmt),
        }
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
        }
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

                if let BinOp::LogicalAnd | BinOp::LogicalOr = op {
                    let label = self.label_counter;
                    self.label_counter += 1;

                    let lhs = self.upgrade_readable(lhs);

                    self.append_instruction(Instruction::ShortCircuit(
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

                // FIXME: hack because `idiv` on x86 cannot take immediate (constant) rhs values.
                let rhs = if let BinOp::Div = op {
                    self.upgrade_readable(rhs).downgrade()
                } else {
                    rhs
                };

                let lhs = self.upgrade_readable(lhs);

                self.append_instruction(Instruction::BinaryOp(*op, lhs.clone(), rhs));

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
                let controlling = self.upgrade_readable(controlling).downgrade();

                // the first instructions to be encountered from here will read the reg
                // and redirect control flow; it is no longer needed.
                self.dealloc_reg(controlling.clone());

                self.label_counter += 2;
                let post_else = self.label_counter - 1;
                let pre_else = self.label_counter - 2;

                self.append_instruction(Instruction::IfThen(pre_else, controlling));

                let true_reg = self.append_from_expression(if_true);
                let true_reg = self.upgrade_readable(true_reg).downgrade();
                self.append_instruction(Instruction::PostIf(post_else, pre_else));

                // FIXME: this all assumes that the same reg is allocated, completely flawed.
                self.dealloc_reg(true_reg);

                let false_reg = self.append_from_expression(if_false);
                let false_reg = self.upgrade_readable(false_reg).downgrade();
                self.append_instruction(Instruction::PostConditionalDummy(post_else));

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
        let controlling = self.upgrade_readable(controlling).downgrade();

        // the first instructions to be encountered from here will read the reg
        // and redirect control flow; it is no longer needed.
        self.dealloc_reg(controlling.clone());

        if let Some(false_branch) = false_branch {
            self.label_counter += 2;
            let post_else = self.label_counter - 1;
            let pre_else = self.label_counter - 2;

            self.append_instruction(Instruction::IfThen(pre_else, controlling));

            self.append_from_statement(true_branch);
            self.append_instruction(Instruction::PostIf(post_else, pre_else));

            self.append_from_statement(false_branch);
            self.append_instruction(Instruction::PostConditionalDummy(post_else));
        } else {
            self.label_counter += 1;
            let post_conditional = self.label_counter - 1;

            self.append_instruction(Instruction::IfThen(post_conditional, controlling));

            self.append_from_statement(true_branch);
            self.append_instruction(Instruction::PostConditionalDummy(post_conditional));
        }
    }
}
