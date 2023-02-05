use std::fmt::Write;

use rcc_bytecode::{Bytecode, Instruction};

pub trait Backend {
    fn write_function(&mut self, ctx: &mut BackendContext, fn_name: &str);
    fn write_instruction(&mut self, ctx: &mut BackendContext, instruction: &Instruction);
    fn write_function_end(&mut self, ctx: &mut BackendContext, fn_name: &str);
}

pub struct BackendContext {
    buf: String,
    indent_lvl: u32,
}

impl BackendContext {
    pub fn _buf(&mut self) -> &mut String {
        &mut self.buf
    }

    pub fn increment_indent(&mut self) {
        self.indent_lvl += 1;
    }

    pub fn decrement_indent(&mut self) {
        self.indent_lvl -= 1;
    }

    pub fn indent(&mut self) {
        write!(
            self.buf,
            "{:>width$}",
            "",
            width = (self.indent_lvl as usize * 4)
        )
        .unwrap()
    }
}

#[macro_export]
macro_rules! write_asm {
    ($ctx:expr, $($arg:tt)*) => {
        {
            $ctx.indent();
            writeln!($ctx._buf(), $($arg)*).unwrap();
        }
    };
}

#[macro_export]
macro_rules! write_asm_no_indent {
    ($ctx:expr, $($arg:tt)*) => {
        writeln!($ctx._buf(), $($arg)*).unwrap();
    };
}

pub fn generate_assembly(bytecode: &[Bytecode], backend: &mut impl Backend) -> String {
    let mut ctx = BackendContext {
        buf: String::new(),
        indent_lvl: 0,
    };

    for function in bytecode {
        generate_from_function(&mut ctx, function, backend);
    }

    ctx.buf
}

fn generate_from_function(
    ctx: &mut BackendContext,
    bytecode: &Bytecode,
    backend: &mut impl Backend,
) {
    backend.write_function(ctx, bytecode.fn_name());
    writeln!(ctx.buf).unwrap();

    for instr in bytecode.instructions() {
        write_asm!(ctx, "# {instr:?}");

        backend.write_instruction(ctx, instr);
        writeln!(ctx.buf).unwrap();
    }

    backend.write_function_end(ctx, bytecode.fn_name());

    writeln!(ctx.buf).unwrap();
}
