use std::fmt::Display;

use super::{error::LexError, token::Token};

pub struct TokenList(pub Vec<Result<Token, LexError>>);

impl Display for TokenList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for tok in &self.0 {
            match tok {
                Ok(tok) => writeln!(f, "{}", tok)?,
                Err(err) => writeln!(f, "ERROR: {}", err)?,
            }
        }

        Ok(())
    }
}
