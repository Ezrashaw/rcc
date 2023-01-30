use rcc_structures::{BinOp, UnaryOp};

#[derive(Debug)]
pub struct Program<'a> {
    pub function: Function<'a>,
}
#[derive(Debug)]
pub struct Function<'a> {
    pub name: &'a str,
    pub block_items: Vec<BlockItem>,
    pub locals: Vec<&'a str>,
}

#[derive(Debug)]
pub enum BlockItem {
    /// Variable definition.
    ///
    /// Indexes into [`Function`]s vector of local identifiers.
    /// Contains an optional initializer.
    Declaration(u32, Option<Expression>),

    Statement(Statement),
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),

    /// Standalone expression.
    ///
    /// Created from code like `2 + 2;`, used for variable assignments.
    Expression(Expression),

    /// A common `if` statement.
    ///
    /// Contains a controlling expression, a `true` branch and an optional
    /// `false` branch.
    Conditional(Expression, Box<Statement>, Option<Box<Statement>>),
}

#[derive(Debug)]
pub enum Expression {
    BinOp {
        has_parens: bool,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        op: BinOp,
    },
    UnaryOp {
        expr: Box<Expression>,
        op: UnaryOp,
    },
    Literal {
        // Note that literals in source code cannot be negative, but constant folding might produce a negative value.
        val: i32,
    },
    Assignment {
        identifier: u32,
        expression: Box<Expression>,
    },
    Variable {
        identifier: u32,
    },
    TernaryConditional {
        controlling: Box<Expression>,
        if_true: Box<Expression>,
        if_false: Box<Expression>,
    },
}
