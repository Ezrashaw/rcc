use rcc_structures::{BinOp, UnaryOp};

/// Bytecode instruction used by `rcc` internally.
///
/// It targets a register machine (currently only 32-bit signed registers),
/// with an "infinite" number of registers.
/// (the x86 backend overflows registers to the stack)
#[derive(Debug, Clone)]
pub enum Instruction {
    /// Moves a value from one location into another.
    Move(ReadLocation, WriteLocation),

    /// Returns the value from the given location.
    Return(ReadLocation),

    /// Applies the given binary operation to the two locations.
    ///
    /// Note that the first location is the LHS and the second the RHS.
    /// The result is stored in the LHS location.
    ///
    /// IMPORTANT: binary logical boolean operations are not implemented with
    /// this instruction, use [`Instruction::ShortCircuit`] and
    /// [`Instruction::BinaryBooleanOp`] instead.
    BinaryOp(BinOp, WriteLocation, ReadLocation),

    /// Applies the given unary operation to the specified location.
    UnaryOp(UnaryOp, WriteLocation),

    /// If the given location is equal to `bool` (in the C style: all non-zero
    /// integers are truthy), then jump to the specified location.
    ///
    /// Used in loops to exit if the the condition is false, and in
    /// short-circuiting binary boolean operations.
    ///
    /// *HACK*: currently this needs a [`WriteLocation`], x86 cannot compare two constants (fair enough).
    // FIXME: see doc-comment
    CompareJump(WriteLocation, bool, u32),

    /// Normalize the second clause of a logical boolean binary operation
    /// (i.e. make all non-zero = 1)
    ///
    /// Also provides position for [`Instruction::CompareJump`] to jump to.
    // FIXME: I don't like the operation-specific bytecode instructions.
    BinaryBooleanOp(WriteLocation, u32),

    /// Modifies/declares a local variable.
    ///
    /// Assigns the variable with the given identifier, for x86 these are
    /// multiplied and converted to `EBP` offsets.
    AssignVariable(u32, ReadLocation),

    /// Loads the local variable into the given location.
    ///
    /// Uses the same logic as [`Instruction::AssignVariable`] for finding
    /// stack offsets.
    LoadVariable(u32, WriteLocation),

    /// Checks the given location and either jumps to the false branch or falls
    /// through to the true branch.
    IfThen(u32, ReadLocation),

    /// An instruction which comes after a conditional expressions to
    /// jump to a location and also define a location.
    ///
    /// Used in `if` statements and loops.
    PostConditional(u32, u32),

    /// An instruction which specifies where another instruction should jump
    /// to.
    ///
    /// Effectively expands to (in the x86 backend) a label.
    // FIXME: remove this, we could fix this with instruction addressing
    //        instead of the ad-hoc allocation currently.
    JumpDummy(u32),
}

/// Someplace where a value can be read.
///
/// Could be constant, in memory, or a register.
#[derive(Debug, Clone)]
pub enum ReadLocation {
    /// We can read from any writable place as well.
    ///
    /// More and more this feels like I'm trying to mimic Rust's memory model. *sighh*
    Writable(WriteLocation),

    /// The value is constant and is not stored anywhere.
    Constant(i32),
}

/// A location where a value can be stored.
///
/// Currently only can be a register.
/// Note that the value doesn't have to be in a hardware register, bytecode
/// registers and hardware registers are different.
#[derive(Debug, Clone, PartialEq)]
pub struct WriteLocation(pub(crate) u8);

impl WriteLocation {
    pub(crate) fn downgrade(&self) -> ReadLocation {
        ReadLocation::Writable(self.clone())
    }

    pub fn reg(&self) -> u8 {
        self.0
    }
}
