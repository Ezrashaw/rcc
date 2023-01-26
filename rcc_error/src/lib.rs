use std::{
    cmp,
    fmt::{self, Display},
    io::Write,
};

use termcolor::{Buffer, Color, ColorSpec, WriteColor};

use rcc_span::Span;

#[derive(Debug)]
pub struct SpannedError<'a> {
    msg: String,
    span: Option<Span<'a>>,
}

impl<'a> SpannedError<'a> {
    pub fn with_span(msg: impl ToString, span: Span<'a>) -> Self {
        Self {
            msg: msg.to_string(),
            span: Some(span),
        }
    }

    pub fn without_span(msg: impl ToString) -> Self {
        Self {
            msg: msg.to_string(),
            span: None,
        }
    }

    pub fn emit(self) -> ! {
        eprintln!("{self}");
        std::process::exit(1)
    }
}

impl Display for SpannedError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut buf = Buffer::ansi();

        buf.set_color(
            ColorSpec::new()
                .set_bold(true)
                .set_intense(true)
                .set_fg(Some(Color::Red)),
        )
        .unwrap();

        write!(buf, "error: ").unwrap();

        buf.reset().unwrap();
        writeln!(buf, "{}", self.msg).unwrap();

        if let Some(span) = &self.span {
            let pos = span.get_line_col();

            buf.set_color(
                ColorSpec::new()
                    .set_dimmed(true)
                    .set_fg(Some(Color::Ansi256(8))),
            )
            .unwrap();
            writeln!(buf, "╰──> {}:{}:{}", "<anon>", pos.0, pos.1).unwrap();

            buf.set_color(ColorSpec::new().set_bold(true).set_fg(Some(Color::Blue)))
                .unwrap();

            write!(buf, "{} | ", pos.0 + 1).unwrap();

            buf.reset().unwrap();
            writeln!(buf, "{}", span.input().lines().nth(pos.0 as usize).unwrap()).unwrap();

            let spaces = 4;
            let spaces = std::iter::repeat(' ')
                .take((spaces + pos.1).try_into().unwrap())
                .collect::<String>();

            let arrows = std::iter::repeat('^')
                .take(cmp::max(span.length().try_into().unwrap(), 1))
                .collect::<String>();

            buf.set_color(ColorSpec::new().set_bold(true).set_fg(Some(Color::Red)))
                .unwrap();
            writeln!(buf, "{spaces}{arrows}").unwrap();
        }

        f.write_str(&String::from_utf8(buf.into_inner()).unwrap())
    }
}

impl Drop for SpannedError<'_> {
    fn drop(&mut self) {
        panic!("Dropped `SpannedError`")
    }
}
