use std::fmt::{self, Display};

/// Bytecode instruction used by `rcc` internally.
///
/// It targets a register machine (currently only 32-bit signed registers),
/// with an "infinite" number of registers.
/// (the x86 backend overflows registers to the stack)
#[derive(Clone)]
pub enum Instruction {
    /// Moves a value into a register.
    Move(RegisterOrConst, Register),

    /// Returns from the currently executing function with the specified value.
    Return(RegisterOrConst),

    /// Applies the given binary operation to the two values.
    ///
    /// Note that the first location is the LHS and the second the RHS.
    /// The result is stored in the LHS location.
    BinaryOp(BinOp, Register, RegisterOrConst),

    /// Applies the given unary operation to the specified register.
    UnaryOp(UnaryOp, Register),

    /// If the given value is equal to `bool` (in the C style: all non-zero
    /// integers are truthy), then jump to the specified location.
    ///
    /// Used in loops to exit if the the condition is false, and in
    /// short-circuiting binary boolean operations.
    CompareJump(RegisterOrConst, bool, u32),

    /// Normalize the second clause of a logical boolean binary operation
    /// (i.e. make all non-zero = 1)
    NormalizeBoolean(Register),

    /// Modifies/declares a local variable.
    ///
    /// Assigns the variable with the given identifier, for x86 these are
    /// multiplied and converted to `EBP` offsets.
    AssignVariable(u32, RegisterOrConst),

    /// Loads the local variable into the given location.
    ///
    /// Uses the same logic as [`Instruction::AssignVariable`] for finding
    /// stack offsets.
    LoadVariable(u32, Register),

    /// Calls the given function and puts the return value in the given place
    CallFunction(String, Register),

    /// An instruction which specifies where another instruction should jump
    /// to.
    ///
    /// Effectively expands to (in the x86 backend) a label.
    // FIXME: remove this, we could fix this with instruction addressing
    //        instead of the ad-hoc allocation currently.
    JumpDummy(u32),

    /// Performs an unconditional jump to the specififed location.
    ///
    /// One-to-one mapping to the x86 JMP instruction.
    UnconditionalJump(u32),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Move(from, to) => write!(f, "{to} = {from}"),
            Instruction::Return(reg) => write!(f, "ret {reg}"),
            Instruction::BinaryOp(op, lhs, rhs) => write!(f, "{lhs} = {lhs} {op:?} {rhs}"),
            Instruction::UnaryOp(op, reg) => write!(f, "{reg} = {op:?} {reg}"),
            Instruction::CompareJump(val, should_jump, label) => {
                write!(f, "if ({val} == {should_jump}) {{ goto {label} }}")
            }
            Instruction::NormalizeBoolean(reg) => write!(f, "{reg} = ({reg} == 0) ? 0 : 1"),
            Instruction::AssignVariable(var, val) => write!(f, "var{var} = {val}"),
            Instruction::LoadVariable(var, reg) => write!(f, "{reg} = var{var}"),
            Instruction::CallFunction(name, reg) => write!(f, "{reg} = call {name}"),
            Instruction::JumpDummy(label) => write!(f, "def {label}:"),
            Instruction::UnconditionalJump(label) => write!(f, "goto {label}"),
        }
    }
}

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

/// A value that can be used as an operand in a [`Instruction`].
///
/// Constants are especially useful here for optimization purposes.
#[derive(Debug, Clone)]
pub enum RegisterOrConst {
    /// We can read from any writable place as well.
    ///
    /// More and more this feels like I'm trying to mimic Rust's memory model. *sighh*
    Register(Register),

    /// The value is constant and is not stored anywhere.
    Constant(i32),
}

impl Display for RegisterOrConst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegisterOrConst::Register(reg) => write!(f, "{reg}"),
            RegisterOrConst::Constant(val) => write!(f, "${val}"),
        }
    }
}

/// A `rcc` bytecode register.
///
/// 256 registers exist in the `rcc` bytecode VM, these are mapped to hardware
/// registers and eventually overflow to the stack.
#[derive(Debug, Clone, PartialEq)]
pub struct Register(pub(crate) u8);

impl Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl Register {
    /// Convert the [`Register`] to a [`RegisterOrConst`].
    pub(crate) fn downgrade(self) -> RegisterOrConst {
        RegisterOrConst::Register(self)
    }

    /// Gets the actual underlying register's identifier.
    ///
    /// This value is then mapped to a hardware register, or some other
    /// storage location.
    pub fn register_number(&self) -> u8 {
        self.0
    }
}
