use rcc_structures::{BinOp, UnaryOp};

#[derive(Debug)]
pub struct Program<'a> {
    pub functions: Vec<Function<'a>>,
}
#[derive(Debug)]
pub struct Function<'a> {
    pub name: &'a str,
    pub args: Vec<&'a str>,
    pub body: Option<Block<'a>>,
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
    Declaration(u32, Option<Expression<'a>>),

    Statement(Statement<'a>),
}

#[derive(Debug)]
pub enum Statement<'a> {
    Return(Expression<'a>),

    /// Standalone expression.
    ///
    /// Created from code like `2 + 2;`, primarily for variable assignments.
    /// "Null" expressions are also allowed, as in ";;;;;;;;".
    Expression(Option<Expression<'a>>),

    /// A common `if` statement.
    ///
    /// Contains a controlling expression, a `true` branch and an optional
    /// `false` branch.
    Conditional(
        Expression<'a>,
        Box<Statement<'a>>,
        Option<Box<Statement<'a>>>,
    ),

    /// A compound statement, otherwise known as a "block".
    Compound(Block<'a>),

    /// A "while" loop.
    ///
    /// Contains the expression to evaluate per-iteration and the body
    /// to execute.
    While(Expression<'a>, Box<Statement<'a>>),

    /// A "do" loop.
    ///
    /// Extremely similar to a "while" loop, except that the expression is
    /// evaluated after each iteration.
    ///
    /// Contains the expression to evaluate per-iteration and the body
    /// to execute.
    Do(Expression<'a>, Box<Statement<'a>>),

    /// A `for` loop that does not define a loop variable.
    ///
    /// In order, contains a:
    /// - Initial expression
    /// - Condition
    /// - Post-expression
    /// - Body
    For(
        Option<Expression<'a>>,
        Option<Expression<'a>>,
        Option<Expression<'a>>,
        Box<Statement<'a>>,
    ),

    /// A `for` loop that defines a loop variable.
    ///
    /// In order, contains a:
    /// - Variable identifier (for the declaration)
    /// - Optional initializer (for the declaration)
    /// - Condition
    /// - Post-expression
    /// - Body
    // FIXME: I don't think tuple variants should span multiple lines. See `Statement::For` as well.
    ForDecl(
        u32,
        Option<Expression<'a>>,
        Option<Expression<'a>>,
        Option<Expression<'a>>,
        Box<Statement<'a>>,
    ),

    /// A `for` loop that defines a loop variable.

    /// Statement that "break"s out of a loop.
    ///
    /// Note that it is *valid* ast for a [`Statement::Break`] to occur
    /// outside of a loop, this is validated later.
    Break,

    /// Statement that "continue"s to the next iteration of a loop.
    ///
    /// Note that it is *valid* ast for a [`Statement::Continue`] to occur
    /// outside of a loop, this is validated later.
    Continue,
}

#[derive(Debug)]
pub enum Expression<'a> {
    BinOp {
        lhs: Box<Expression<'a>>,
        rhs: Box<Expression<'a>>,
        op: BinOp,
    },
    UnaryOp {
        expr: Box<Expression<'a>>,
        op: UnaryOp,
    },
    Literal {
        // Note that literals in source code cannot be negative, but constant folding might produce a negative value.
        val: i32,
    },
    Assignment {
        variable: u32,
        expression: Box<Expression<'a>>,
    },
    Variable {
        identifier: u32,
    },
    TernaryConditional {
        controlling: Box<Expression<'a>>,
        if_true: Box<Expression<'a>>,
        if_false: Box<Expression<'a>>,
    },
    FunctionCall {
        identifier: &'a str,
        args: Vec<Expression<'a>>,
    },
}
