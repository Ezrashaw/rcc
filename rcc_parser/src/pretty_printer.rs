use std::io::{self, Write};

use rcc_structures::{BinOp, UnaryOp};

use crate::ast::Function;

use super::ast::{Expression, Program, Statement};

pub struct PrettyPrinter<'a, 'b> {
    pub buf: &'a mut dyn Write,
    ast: &'b Program<'b>,
}

impl<'a, 'b> PrettyPrinter<'a, 'b> {
    pub fn new(buf: &'a mut dyn Write, ast: &'b Program<'b>) -> Self {
        Self { buf, ast }
    }

    fn indent(&mut self, lvl: u32) -> io::Result<()> {
        write!(self.buf, "{:>width$}", "", width = (lvl * 4) as usize)
    }

    fn print_variable(&mut self, var: u32) -> io::Result<()> {
        // FIXME: we won't always just have one function!
        let name = self.ast.function.locals[var as usize];

        write!(self.buf, "{name}")
    }

    pub fn print(mut self) -> io::Result<()> {
        self.print_fn(&self.ast.function)
    }

    fn print_fn(&mut self, function: &Function) -> io::Result<()> {
        writeln!(self.buf, "int {}() {{", function.name)?;

        for stmt in &function.statements {
            self.print_stmt(&stmt, 1)?;
        }

        writeln!(self.buf, "}}")
    }

    fn print_stmt(&mut self, stmt: &Statement, ident_level: u32) -> io::Result<()> {
        self.indent(ident_level)?;

        match stmt {
            Statement::Return(expr) => {
                write!(self.buf, "return ")?;
                self.print_expr(expr)?;
            }
            Statement::Declaration(var, expr) => {
                write!(self.buf, "int ")?;
                self.print_variable(*var)?;
                if let Some(expr) = expr {
                    write!(self.buf, " = ")?;
                    self.print_expr(expr)?;
                }
            }
            Statement::Expression(expr) => self.print_expr(expr)?,
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
            Expression::BinOp {
                has_parens,
                lhs,
                rhs,
                op,
            } => {
                if *has_parens {
                    write!(self.buf, "(")?;
                }

                self.print_expr(lhs)?;
                write!(
                    self.buf,
                    " {} ",
                    match op {
                        BinOp::Add => "+",
                        BinOp::Sub => "-",
                        BinOp::Mul => "*",
                        BinOp::Div => "/",
                        BinOp::LogicalOr => "||",
                        BinOp::LogicalAnd => "&&",
                        BinOp::Equals => "==",
                        BinOp::NotEquals => "!+",
                        BinOp::LessThan => "<",
                        BinOp::LessThanOrEquals => "<=",
                        BinOp::GreaterThan => ">",
                        BinOp::GreaterThanOrEquals => ">=",
                    }
                )?;
                self.print_expr(rhs)?;

                if *has_parens {
                    write!(self.buf, ")")?;
                }

                Ok(())
            }
            Expression::Assignment {
                identifier,
                expression,
            } => {
                self.print_variable(*identifier)?;
                write!(self.buf, " = ")?;
                self.print_expr(expression)
            }
            Expression::Variable { identifier } => self.print_variable(*identifier),
        }
    }
}
