use crate::{Bytecode, Instruction, Register, RegisterOrConst};

/// Private utility functions for the bytecode machine.
///
/// For instance, it contains the register allocation mechanism.
impl Bytecode<'_> {
    pub(crate) fn alloc_reg(&mut self) -> Register {
        let loc = (0..255)
            .map(Register)
            .find(|loc| !self.allocated_registers.contains(loc))
            .unwrap_or_else(|| panic!("exhausted registers during bytecode generations: {self:?}"));

        self.allocated_registers.push(loc.clone());

        // println!("alloc reg {loc:?}, here:");
        // println!("{}", std::backtrace::Backtrace::force_capture());

        loc
    }

    /// Deallocates a register, accepts a [`ReadLocation`] for convience purposes (no-op on constant).
    pub(crate) fn dealloc_reg(&mut self, value: RegisterOrConst) {
        if let RegisterOrConst::Register(reg) = value {
            let idx = self
                .allocated_registers
                .iter()
                .position(|l| l == &reg)
                .expect("provided register was not allocated");

            self.allocated_registers.swap_remove(idx);

            // println!("dealloc reg {idx}, here:");
            // println!("{}", std::backtrace::Backtrace::force_capture());
        }
    }

    /// "Upgrades" a value to a register.
    ///
    /// In other words ensures that a value is in a register, this is done by
    /// allocating a register and issuing a [`Instruction::Move`] if the
    /// provided value is a constant.
    pub(crate) fn upgrade_readable(&mut self, val: RegisterOrConst) -> Register {
        match val {
            RegisterOrConst::Register(reg) => reg,
            RegisterOrConst::Constant(val) => {
                let reg = self.alloc_reg();
                self.append_instruction(Instruction::Move(
                    RegisterOrConst::Constant(val),
                    reg.clone(),
                ));

                reg
            }
        }
    }

    /// Helper function to add an instruction to the bytcode.
    pub(crate) fn append_instruction(&mut self, instr: Instruction) {
        self.instr.push(instr);
    }
}
