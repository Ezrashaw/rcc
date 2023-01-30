use rcc_parser::ast::{BlockItem, Expression, Function, Statement};
use rcc_structures::{BinOp, UnaryOp};

/// Bytecode instruction used by `rcc` internally.
///
/// It targets a register machine (currently only 32-bit signed registers),
/// with an "infinite" number of registers.
/// (the x86 backend overflows registers to the stack)
#[derive(Debug, Clone)]
pub enum Instruction {
    /// Loads a constant integer into the given register.
    LoadInt(i32, u8),

    /// Returns the value from the first register.
    Return,

    /// Applies the given binary operation to the two registers.
    ///
    /// Note that the first register is the LHS and the second the RHS.
    /// The result is stored in the LHS register.
    ///
    /// IMPORTANT: binary logical boolean operations are not implemented with
    /// this instruction, use [`Instruction::ShortCircuit`] and
    /// [`Instruction::BinaryBooleanOp`] instead.
    BinaryOp(BinOp, u8, u8),

    /// Applies the given unary operation to the specified register.
    UnaryOp(UnaryOp, u8),

    /// Short-circuit a logical boolean binary operation:
    ///
    /// If `bool` is true, then short-circuit when reg is true (logical or).
    /// If `bool` is false, then short-circuit when reg is false (logical and).
    ///
    /// The second `u32` points to the end of the second sub-expression (short-circuit).
    ShortCircuit(u8, bool, u32),

    /// Normalize the second clause of a logical boolean binary operation
    /// (i.e. make all non-zero = 1)
    ///
    /// Also provides location for [`Instruction::ShortCircuit`] to jump to.
    BinaryBooleanOp(u8, u32),

    /// Declares a new local variable on the stack.
    ///
    /// For x86, simply emits `pushl %{reg}`.
    // FIXME: how can we optimize `LoadInt` + `DefineVariable`? we should remove the register indirection.
    DeclareVariable(u32, u8),

    /// Modifies a local variable (declared with [`Instruction::DeclareVariable`]).
    ///
    /// Assigns the variable with the given identifier, for x86 these are
    /// multiplied and converted to `EBP` offsets.
    AssignVariable(u32, u8),

    /// Loads the local variable into the given register.
    ///
    /// Uses the same logic as [`Instruction::AssignVariable`] for finding
    /// stack offsets.
    LoadVariable(u32, u8),

    /// Checks the given register and either jumps to the false branch or falls
    /// through to the true branch.
    IfThen(u32, u8),

    /// An instruction which comes after a if statement, it jumps over the else
    /// statement (jumps to the [`Instruction::PostConditionalDummy`]).
    ///
    /// Also specifies the start of the false branch, jumped to by
    /// [`Instruction::IfThen`].
    PostIf(u32, u32),

    /// An instruction which specifies where [`Instruction::PostIf`] or
    /// [`Instruction::IfThen`] should jump to, it appears after a conditional
    /// statement.
    // FIXME: remove this, we could fix this with instruction addressing
    //        instead of the ad-hoc allocation currently.
    PostConditionalDummy(u32),
}

impl Instruction {
    pub(crate) fn from_function(function: &Function, label_counter: &mut u32) -> Vec<Self> {
        let mut buf = Vec::new();

        for item in &function.block_items {
            Self::from_block_item(&mut buf, item, label_counter);
        }

        buf
    }

    fn from_block_item(buf: &mut Vec<Self>, item: &BlockItem, label_counter: &mut u32) {
        match item {
            BlockItem::Declaration(id, val) => {
                Self::from_declaration(buf, *id, val.as_ref(), label_counter)
            }
            BlockItem::Statement(stmt) => Self::from_statement(buf, stmt, label_counter),
        }
    }

    fn from_statement(buf: &mut Vec<Self>, statement: &Statement, label_counter: &mut u32) {
        match statement {
            Statement::Return(val) => {
                Self::from_expression(buf, val, 0, label_counter);
                buf.push(Instruction::Return);
            }

            Statement::Expression(expr) => Self::from_expression(buf, expr, 0, label_counter),
            Statement::Conditional(expr, true_branch, false_branch) => Self::from_conditional(
                buf,
                expr,
                true_branch,
                false_branch.as_deref(),
                label_counter,
            ),
        }
    }

    fn from_expression(
        buf: &mut Vec<Self>,
        expression: &Expression,
        reg: u8,
        label_counter: &mut u32,
    ) {
        match expression {
            Expression::Literal { val } => buf.push(Instruction::LoadInt(*val, reg)),
            Expression::UnaryOp { expr, op } => {
                Self::from_unary_op(buf, expr, op, reg, label_counter)
            }
            Expression::BinOp { lhs, rhs, op, .. } => {
                Self::from_binary_op(buf, lhs, rhs, op, reg, label_counter)
            }
            Expression::Assignment {
                identifier,
                expression,
            } => {
                Self::from_expression(buf, expression, reg, label_counter);

                buf.push(Instruction::AssignVariable(*identifier, reg))
            }
            Expression::Variable { identifier } => {
                buf.push(Instruction::LoadVariable(*identifier, reg))
            }
            Expression::TernaryConditional { .. } => todo!(),
        }
    }

    fn from_unary_op(
        buf: &mut Vec<Self>,
        inner: &Expression,
        op: &UnaryOp,
        reg: u8,
        label_counter: &mut u32,
    ) {
        Self::from_expression(buf, inner, reg, label_counter);

        buf.push(Instruction::UnaryOp(*op, reg));
    }

    fn from_binary_op(
        buf: &mut Vec<Self>,
        lhs: &Expression,
        rhs: &Expression,
        op: &BinOp,
        reg: u8,
        label_counter: &mut u32,
    ) {
        // FIXME: this always works (see rhs), but we should have a mechanism to alloc/dealloc regs, producing the same result with better simplicity.
        Self::from_expression(buf, lhs, reg, label_counter);

        if let BinOp::LogicalAnd | BinOp::LogicalOr = op {
            let label = *label_counter;
            *label_counter += 1;

            buf.push(Instruction::ShortCircuit(
                reg,
                *op == BinOp::LogicalOr,
                label,
            ));

            Self::from_expression(buf, rhs, reg, label_counter);
            buf.push(Instruction::BinaryBooleanOp(reg, label));
        } else {
            Self::from_expression(buf, rhs, reg + 1, label_counter);
            buf.push(Instruction::BinaryOp(*op, reg, reg + 1));
        }
    }

    fn from_declaration(
        buf: &mut Vec<Self>,
        id: u32,
        val: Option<&Expression>,
        label_counter: &mut u32,
    ) {
        const REGISTER: u8 = 0;

        if let Some(init) = val {
            Self::from_expression(buf, init, REGISTER, label_counter)
        } else {
            // If a variable is declared but not initialized, then we can just
            // initialize to 0. The standard does not specify a value for this
            // situation. Ahh, the wonders of C.
            buf.push(Instruction::LoadInt(0, REGISTER))
        }

        buf.push(Instruction::DeclareVariable(id, REGISTER))
    }

    fn from_conditional(
        buf: &mut Vec<Self>,
        expr: &Expression,
        true_branch: &Statement,
        false_branch: Option<&Statement>,
        label_counter: &mut u32,
    ) {
        Self::from_expression(buf, expr, 0, label_counter);

        if let Some(false_branch) = false_branch {
            *label_counter += 2;
            let post_else = *label_counter - 1;
            let pre_else = *label_counter - 2;

            buf.push(Instruction::IfThen(pre_else, 0));

            Self::from_statement(buf, true_branch, label_counter);
            buf.push(Instruction::PostIf(post_else, pre_else));

            Self::from_statement(buf, false_branch, label_counter);
            buf.push(Instruction::PostConditionalDummy(post_else));
        } else {
            *label_counter += 1;
            let post_conditional = *label_counter - 1;

            buf.push(Instruction::IfThen(post_conditional, 0));

            Self::from_statement(buf, true_branch, label_counter);
            buf.push(Instruction::PostConditionalDummy(post_conditional));
        }
    }
}
