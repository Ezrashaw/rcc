use std::fmt::{self, Display};

#[allow(clippy::upper_case_acronyms)]
pub(crate) enum Register {
    // FIXME: more arm registers exist, but do we currently need them?
    W0,
    W1,
    W2,
    W3,
    W4,
    W5,
    W6,
    W7,
    W8,
    W9,

    /// Register overflow, value is saved on the stack instead
    Stack,
}

impl Register {
    pub(crate) fn from_u8(value: u8) -> Self {
        match value {
            0 => Register::W0,
            1 => Register::W1,
            2 => Register::W2,
            3 => Register::W3,
            4 => Register::W4,
            5 => Register::W5,
            6 => Register::W6,
            7 => Register::W7,
            8 => Register::W8,
            9 => Register::W9,

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
                Register::W0 => "w0",
                Register::W1 => "w1",
                Register::W2 => "w2",
                Register::W3 => "w3",
                Register::W4 => "w4",
                Register::W5 => "w5",
                Register::W6 => "w6",
                Register::W7 => "w7",
                Register::W8 => "w8",
                Register::W9 => "w9",

                Register::Stack => todo!(),
            }
        )
    }
}

// impl Register {
//     pub fn get_low_8(&self) -> &'static str {
//         match self {
//             Register::ECX => "cl",
//             Register::R8 => "r8b",
//             Register::R9 => "r9b",
//             Register::R10 => "r10b",
//             Register::R11 => "r11b",
//             Register::R12 => "r12b",
//             Register::R13 => "r13b",
//             Register::R14 => "r14b",
//             Register::R15 => "r15b",

//             Register::Stack => todo!(),
//         }
//     }
// }
