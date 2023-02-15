#[allow(clippy::upper_case_acronyms)]
pub(crate) enum ArmRegister {
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

impl ArmRegister {
    pub(crate) fn from_u8(value: u8) -> Self {
        match value {
            0 => ArmRegister::W0,
            1 => ArmRegister::W1,
            2 => ArmRegister::W2,
            3 => ArmRegister::W3,
            4 => ArmRegister::W4,
            5 => ArmRegister::W5,
            6 => ArmRegister::W6,
            7 => ArmRegister::W7,
            8 => ArmRegister::W8,
            9 => ArmRegister::W9,

            _ => ArmRegister::Stack,
        }
    }

    pub fn get_str(&self) -> &'static str {
        match self {
            ArmRegister::W0 => "w0",
            ArmRegister::W1 => "w1",
            ArmRegister::W2 => "w2",
            ArmRegister::W3 => "w3",
            ArmRegister::W4 => "w4",
            ArmRegister::W5 => "w5",
            ArmRegister::W6 => "w6",
            ArmRegister::W7 => "w7",
            ArmRegister::W8 => "w8",
            ArmRegister::W9 => "w9",

            ArmRegister::Stack => todo!(),
        }
    }
}
