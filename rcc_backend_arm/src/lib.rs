use rcc_bytecode::Bytecode;

pub struct ArmBackend {
    buf: String,
    indent_lvl: u32,
}

impl ArmBackend {
    pub fn gen_arm(bytecode: &Bytecode<'_>) -> String {
        let mut backend = Self {
            buf: String::new(),
            indent_lvl: 0,
        };

        backend.buf
    }
}
