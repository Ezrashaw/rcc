use crate::{Bytecode, Instruction, ReadLocation, WriteLocation};

/// Private utility functions for the bytecode machine.
///
/// For instance, it contains the register allocation mechanism.
impl Bytecode<'_> {
    pub(crate) fn alloc_reg(&mut self) -> WriteLocation {
        let loc = (0..255)
            .map(|i| WriteLocation(i))
            .filter(|loc| !self.allocated_registers.contains(loc))
            .next()
            .expect(&format!(
                "exhausted registers during bytecode generations: {:?}",
                self
            ));

        self.allocated_registers.push(loc.clone());

        loc
    }

    /// Deallocates a register, accepts a [`ReadLocation`] for convience purposes (no-op on constant).
    pub(crate) fn dealloc_reg(&mut self, reg: ReadLocation) {
        if let ReadLocation::Writable(reg) = reg {
            let idx = self
                .allocated_registers
                .iter()
                .position(|l| l == &reg)
                .expect("provided register was not allocated");

            self.allocated_registers.swap_remove(idx);
        }
    }

    pub(crate) fn upgrade_readable(&mut self, rloc: ReadLocation) -> WriteLocation {
        match rloc {
            ReadLocation::Writable(wloc) => wloc,
            ReadLocation::Constant(val) => {
                let reg = self.alloc_reg();
                self.append_instruction(Instruction::LoadInt(val, reg.clone()));

                reg
            }
        }
    }

    pub(crate) fn append_instruction(&mut self, instr: Instruction) {
        self.instr.push(instr)
    }
}