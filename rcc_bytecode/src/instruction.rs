use rcc_structures::{BinOp, UnaryOp};

/// Bytecode instruction used by `rcc` internally.
///
/// It targets a register machine (currently only 32-bit signed registers),
/// with an "infinite" number of registers.
/// (the x86 backend overflows registers to the stack)
#[derive(Debug, Clone)]
pub enum Instruction {
    /// Loads a constant integer into the given location.
    // FIXME: should this really be a readloc->writeloc conversion?
    LoadInt(i32, WriteLocation),

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

    /// Short-circuit a logical boolean binary operation:
    ///
    /// If `bool` is true, then short-circuit when loc is true (logical or).
    /// If `bool` is false, then short-circuit when loc is false (logical and).
    ///
    /// The second `u32` points to the end of the second sub-expression (short-circuit).
    /// 
    /// HACK: currently this needs a [`WriteLocation`], x86 cannot compare two constants (fair enough). 
    // FIXME: see doc-comment
    ShortCircuit(WriteLocation, bool, u32),

    /// Normalize the second clause of a logical boolean binary operation
    /// (i.e. make all non-zero = 1)
    ///
    /// Also provides position for [`Instruction::ShortCircuit`] to jump to.
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
