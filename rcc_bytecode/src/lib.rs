use rcc_parser::ast::Program;

mod instruction;

pub use instruction::Instruction;

#[derive(Debug)]
pub struct Bytecode<'a> {
    functions: Vec<(&'a str, Vec<Instruction>)>,
}

impl<'a> Bytecode<'a> {
    pub fn from_ast(ast: &Program<'a>) -> Self {
        let function = Instruction::from_function(&ast.function);

        Self {
            functions: vec![(ast.function.name, function)],
        }
    }

    pub fn function(&self) -> &(&str, Vec<Instruction>) {
        self.functions.first().unwrap()
    }
}
