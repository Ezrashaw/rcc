use std::io::{self, Write};

use rcc_structures::{BinOp, UnaryOp};

use crate::ast::{Block, BlockItem, Function};

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
        // FIXME: how to deal with blocks?
        // let name = self.ast.function.block.variables[var as usize];

        write!(self.buf, "%{var}")
    }

    pub fn print(mut self) -> io::Result<()> {
        self.print_fn(&self.ast.function)
    }

    fn print_fn(&mut self, function: &Function) -> io::Result<()> {
        write!(self.buf, "int {}() ", function.name)?;

        self.print_block(&function.block)
    }

    fn print_block(&mut self, block: &Block) -> io::Result<()> {
        writeln!(self.buf, "{{")?;

        for item in &block.block_items {
            self.print_block_item(item, 1)?;
        }

        writeln!(self.buf, "}}")
    }

    fn print_block_item(&mut self, item: &BlockItem, indent_level: u32) -> io::Result<()> {
        match item {
            BlockItem::Declaration(var, expr) => {
                self.indent(indent_level)?;

                write!(self.buf, "int ")?;
                self.print_variable(*var)?;
                if let Some(expr) = expr {
                    write!(self.buf, " = ")?;
                    self.print_expr(expr)?;
                }

                writeln!(self.buf, ";")?;
            }
            BlockItem::Statement(stmt) => self.print_stmt(stmt, indent_level)?,
        }

        Ok(())
    }

    fn print_stmt(&mut self, stmt: &Statement, indent_level: u32) -> io::Result<()> {
        self.indent(indent_level)?;

        match stmt {
            Statement::Return(expr) => {
                write!(self.buf, "return ")?;
                self.print_expr(expr)?;
            }

            Statement::Expression(expr) => {
                if let Some(expr) = expr {
                    self.print_expr(expr)?;
                }
            }
            Statement::Conditional(expr, if_true, if_false) => {
                write!(self.buf, "if (")?;
                self.print_expr(expr)?;
                writeln!(self.buf, ")")?;

                self.print_stmt(if_true, indent_level + 1)?;

                if let Some(if_false) = if_false {
                    self.indent(indent_level)?;
                    writeln!(self.buf, "else")?;

                    self.print_stmt(if_false, indent_level + 1)?;
                }
            }
            Statement::Compound(block) => self.print_block(block)?,
            Statement::While(_, _) | Statement::Do(_, _) => (),

            Statement::Break => write!(self.buf, "break")?,
            Statement::Continue => write!(self.buf, "continue")?,
        };

        if matches!(stmt, Statement::Conditional(..)) {
            writeln!(self.buf)
        } else {
            writeln!(self.buf, ";")
        }
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
            Expression::TernaryConditional {
                controlling,
                if_true,
                if_false,
            } => {
                self.print_expr(controlling)?;
                write!(self.buf, " ? ")?;

                self.print_expr(if_true)?;
                write!(self.buf, " : ")?;

                self.print_expr(if_false)
            }
        }
    }
}
