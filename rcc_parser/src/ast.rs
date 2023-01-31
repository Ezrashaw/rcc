use rcc_structures::{BinOp, UnaryOp};

#[derive(Debug)]
pub struct Program<'a> {
    pub function: Function<'a>,
}
#[derive(Debug)]
pub struct Function<'a> {
    pub name: &'a str,
    pub block: Block<'a>,
}

#[derive(Debug)]
// FIXME: scope should be seperated for global variables
pub struct Block<'a> {
    pub block_items: Vec<BlockItem<'a>>,
    pub variables: Vec<&'a str>,
    pub parent: Option<&'a Block<'a>>,
}

#[derive(Debug)]
pub enum BlockItem<'a> {
    /// Variable definition.
    ///
    /// Indexes into [`Function`]s vector of local identifiers.
    /// Contains an optional initializer.
    Declaration(u32, Option<Expression>),

    Statement(Statement<'a>),
}

#[derive(Debug)]
pub enum Statement<'a> {
    Return(Expression),

    /// Standalone expression.
    ///
    /// Created from code like `2 + 2;`, primarily for variable assignments.
    /// "Null" expressions are also allowed, as in ";;;;;;;;".
    Expression(Option<Expression>),

    /// A common `if` statement.
    ///
    /// Contains a controlling expression, a `true` branch and an optional
    /// `false` branch.
    Conditional(Expression, Box<Statement<'a>>, Option<Box<Statement<'a>>>),

    /// A compound statement, otherwise known as a "block".
    Compound(Block<'a>),
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
