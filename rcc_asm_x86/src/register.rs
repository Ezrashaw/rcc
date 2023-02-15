#[allow(clippy::upper_case_acronyms)]
pub(crate) enum X86Register {
    ECX,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,

    /// Register overflow, value is saved on the stack instead
    Stack,
}

impl X86Register {
    pub(crate) fn from_u8(value: u8) -> Self {
        match value {
            0 => X86Register::ECX,
            1 => X86Register::R8,
            2 => X86Register::R9,
            3 => X86Register::R10,
            4 => X86Register::R11,
            5 => X86Register::R12,
            6 => X86Register::R13,
            7 => X86Register::R14,
            8 => X86Register::R15,

            _ => X86Register::Stack,
        }
    }

    pub fn get_low_8(&self) -> &'static str {
        match self {
            X86Register::ECX => "%cl",
            X86Register::R8 => "%r8b",
            X86Register::R9 => "%r9b",
            X86Register::R10 => "%r10b",
            X86Register::R11 => "%r11b",
            X86Register::R12 => "%r12b",
            X86Register::R13 => "%r13b",
            X86Register::R14 => "%r14b",
            X86Register::R15 => "%r15b",

            X86Register::Stack => todo!(),
        }
    }

    pub fn get_str(&self) -> &'static str {
        match self {
            X86Register::ECX => "%ecx",
            X86Register::R8 => "%r8d",
            X86Register::R9 => "%r9d",
            X86Register::R10 => "%r10d",
            X86Register::R11 => "%r11d",
            X86Register::R12 => "%r12d",
            X86Register::R13 => "%r13d",
            X86Register::R14 => "%r14d",
            X86Register::R15 => "%r15d",

            X86Register::Stack => todo!(),
        }
    }
}
