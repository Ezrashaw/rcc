use rcc_structures::{BinOp, UnaryOp};

#[derive(Debug)]
pub struct Program<'a> {
    pub function: Function<'a>,
    // variables: Vec<&'a str>,
}
#[derive(Debug)]
pub struct Function<'a> {
    pub name: &'a str,
    pub statement: Statement,
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

// #[derive(Debug)]
// pub enum Statement {
//     Assignment { id: u32, value: Box<Expression> },
//     Expression { expr: Box<Expression> },
// }

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
        val: u32,
    },
    // Variable {
    //     id: u32,
    // },
}
