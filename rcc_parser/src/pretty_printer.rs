use std::io::{self, Write};

use crate::ast::{Function, UnaryOp};

use super::ast::{Expression, Program, Statement};

pub struct PrettyPrinter<'a, 'b> {
    pub buf: &'a mut dyn Write,
    ast: &'b Program<'b>,
}

impl<'a, 'b> PrettyPrinter<'a, 'b> {
    pub fn new(buf: &'a mut dyn Write, ast: &'b Program<'b>) -> Self {
        Self { buf, ast }
    }

    fn ident(&mut self, lvl: u32) -> io::Result<()> {
        write!(self.buf, "{:>width$}", "", width = (lvl * 4) as usize)
    }

    pub fn print(mut self) -> io::Result<()> {
        self.print_fn(&self.ast.function)
    }

    fn print_fn(&mut self, function: &Function) -> io::Result<()> {
        writeln!(self.buf, "int {}() {{", function.name)?;

        self.print_stmt(&function.statement, 1)?;

        writeln!(self.buf, "}}")
    }

    fn print_stmt(&mut self, stmt: &Statement, ident_level: u32) -> io::Result<()> {
        self.ident(ident_level)?;

        match stmt {
            Statement::Return(expr) => {
                write!(self.buf, "return ")?;
                self.print_expr(expr)?;
            }
        };

        writeln!(self.buf, ";")
    }

    fn print_expr(&mut self, expr: &Expression) -> io::Result<()> {
        match expr {
            Expression::Literal { val } => write!(self.buf, "{val}"),
            Expression::UnaryOp { expr, op } => {
                write!(
                    self.buf,
                    "{}",
                    match op {
                        UnaryOp::Negation => "-",
                        UnaryOp::BitwiseComplement => "~",
                        UnaryOp::LogicalNegation => "!",
                    }
                )?;

                self.print_expr(expr)
            }
        }
    }
}
