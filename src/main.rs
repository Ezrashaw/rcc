use std::error::Error;

use rcc::lexer::{
    error::{LexError, LexErrorKind},
    Lexer,
};

fn main() {
    env_logger::builder().format_timestamp_12hour().init();

    let input = std::fs::read_to_string("input.c").unwrap();
    let mut lexer = Lexer::new(input.into());

    loop {
        let tok = lexer.next_token();
        if let Err(err) = tok {
            break;
        }
    }
}
