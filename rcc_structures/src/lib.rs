#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Negation,
    BitwiseComplement,
    LogicalNegation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Modulo,

    LogicalOr,
    LogicalAnd,
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEquals,
    GreaterThan,
    GreaterThanOrEquals,

    LeftShift,
    RightShift,

    BitwiseOr,
    ExclusiveOr,
    BitwiseAnd,
}
