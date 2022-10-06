use std::fmt;

pub struct CompileError {
    kind: CompileErrorKind,
    // positioning?
}

impl CompileError {
    pub fn new(kind: CompileErrorKind) -> Self {
        Self { kind }
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Compile Error:")?;

        writeln!(
            f,
            "{}",
            match self.kind {
                CompileErrorKind::ExpectedXButFoundY { expected, found } =>
                    format!("Expected: {} but found: {} instead.", expected, found),
                CompileErrorKind::EOFReached =>
                    "Reached End-Of-File (EOF) but expected more.".to_owned(),
                CompileErrorKind::InternalOrUnimplemented => "Internal compiler error!".to_owned(),
            }
        )?;

        Ok(())
    }
}

pub enum CompileErrorKind {
    ExpectedXButFoundY {
        expected: &'static str,
        found: &'static str,
    },
    EOFReached,
    InternalOrUnimplemented,
}
