use std::fmt::{self, Display};

pub(crate) enum Register {
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

impl Register {
    pub(crate) fn from_u8(value: u8) -> Self {
        match value {
            0 => Register::ECX,
            1 => Register::R8,
            2 => Register::R9,
            3 => Register::R10,
            4 => Register::R11,
            5 => Register::R12,
            6 => Register::R13,
            7 => Register::R14,
            8 => Register::R15,

            _ => Register::Stack,
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Register::ECX => "ecx",
                Register::R8 => "r8d",
                Register::R9 => "r9d",
                Register::R10 => "r10d",
                Register::R11 => "r11d",
                Register::R12 => "r12d",
                Register::R13 => "r13d",
                Register::R14 => "r14d",
                Register::R15 => "r15d",

                Register::Stack => todo!(),
            }
        )
    }
}

impl Register {
    pub fn get_low_8(&self) -> &'static str {
        match self {
            Register::ECX => "cl",
            Register::R8 => "r8b",
            Register::R9 => "r9b",
            Register::R10 => "r10b",
            Register::R11 => "r11b",
            Register::R12 => "r12b",
            Register::R13 => "r13b",
            Register::R14 => "r14b",
            Register::R15 => "r15b",

            Register::Stack => todo!(),
        }
    }
}
