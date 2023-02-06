mod constant_folder;
mod variable_inliner;

pub use constant_folder::ConstantFolder;
use rcc_parser::ast::{Block, BlockItem, Expression, Function, Program, Statement};


/// Defines the interface for creating an optimization pass.
///
/// Note that `rcc` optimization recurses breadth-first.
pub(crate) trait OptimizationPass {
    fn opt_function(&mut self, _: &mut Function) {}
    fn opt_block(&mut self, _: &mut Block) {}
    fn opt_block_item(&mut self, _: &mut BlockItem) {}
    fn opt_statement(&mut self, _: &mut Statement) {}
    fn opt_expression(&mut self, _: &mut Expression) {}
}

pub fn optimize_ast(_ast: &mut Program) {
    // optimize_ast_with_pass(ast, &mut ConstantFolder);

    // // remove useless statements
    // ast.function.block.block_items.retain(|stmt| {
    //     !matches!(
    //         stmt,
    //         BlockItem::Statement(Statement::Expression(
    //             Some(Expression::Literal { .. }) | None
    //         ))
    //     )
    // });

    // let mut inliner = VariableInliner::new();
    // optimize_ast_with_pass(ast, &mut inliner);

    // dbg!(&ast);

    // // remove useless statements
    // ast.functions.bod.block_items.retain(|stmt| {
    //     !matches!(
    //         stmt,
    //         BlockItem::Statement(Statement::Expression(
    //             Some(Expression::Literal { .. }) | None
    //         ))
    //     )
    // });

    // optimize_ast_with_pass(ast, &mut inliner);

    // // remove useless statements
    // ast.function.block.block_items.retain(|stmt| {
    //     !matches!(
    //         stmt,
    //         BlockItem::Statement(Statement::Expression(
    //             Some(Expression::Literal { .. }) | None
    //         ))
    //     )
    // });

    // optimize_ast_with_pass(ast, &mut ConstantFolder);

    // // remove useless statements
    // ast.function.block.block_items.retain(|stmt| {
    //     !matches!(
    //         stmt,
    //         BlockItem::Statement(Statement::Expression(
    //             Some(Expression::Literal { .. }) | None
    //         ))
    //     )
    // });
}

fn optimize_ast_with_pass(ast: &mut Program, pass: &mut impl OptimizationPass) {
    for function in &mut ast.functions {
        optimize_function_with_pass(function, pass);
    }
}

fn optimize_function_with_pass(function: &mut Function, pass: &mut impl OptimizationPass) {
    pass.opt_function(function);

    if let Some(block) = &mut function.body {
        optimize_block_with_pass(block, pass);
    }
}

fn optimize_block_with_pass(block: &mut Block, pass: &mut impl OptimizationPass) {
    pass.opt_block(block);

    for block_item in &mut block.block_items {
        optimize_block_item_with_pass(block_item, pass);
    }
}

fn optimize_block_item_with_pass(item: &mut BlockItem, pass: &mut impl OptimizationPass) {
    pass.opt_block_item(item);
    match item {
        BlockItem::Statement(stmt) => optimize_statement_with_pass(stmt, pass),
        BlockItem::Declaration(..) => (),
    }
}

fn optimize_statement_with_pass(stmt: &mut Statement, pass: &mut impl OptimizationPass) {
    pass.opt_statement(stmt);
    match stmt {
        Statement::Return(expr) => optimize_expression_with_pass(expr, pass),
        Statement::Expression(expr) => {
            if let Some(expr) = expr {
                optimize_expression_with_pass(expr, pass);
            }
        }
        Statement::Conditional(controlling, true_branch, false_branch) => {
            optimize_expression_with_pass(controlling, pass);
            optimize_statement_with_pass(true_branch, pass);
            if let Some(false_branch) = false_branch {
                optimize_statement_with_pass(false_branch, pass);
            }
        }
        Statement::Compound(block) => optimize_block_with_pass(block, pass),

        Statement::While(controlling, body) | Statement::Do(controlling, body) => {
            optimize_expression_with_pass(controlling, pass);
            optimize_statement_with_pass(body, pass);
        }

        Statement::Break | Statement::Continue => (),
        Statement::For(_, _, _, _) => todo!(),
        Statement::ForDecl(_, _, _, _, _) => todo!(),
    }
}

fn optimize_expression_with_pass(expression: &mut Expression, pass: &mut impl OptimizationPass) {
    pass.opt_expression(expression);

    match expression {
        Expression::BinOp { lhs, rhs, .. } => {
            optimize_expression_with_pass(lhs, pass);
            optimize_expression_with_pass(rhs, pass);
        }
        Expression::UnaryOp { expr, .. } => optimize_expression_with_pass(expr, pass),
        Expression::Assignment { expression, .. } => {
            optimize_expression_with_pass(expression, pass);
        }
        Expression::TernaryConditional {
            controlling,
            if_true,
            if_false,
        } => {
            optimize_expression_with_pass(controlling, pass);
            optimize_expression_with_pass(if_true, pass);
            optimize_expression_with_pass(if_false, pass);
        }

        _ => (),
    }
}
