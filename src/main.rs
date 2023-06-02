use std::process;

use base::Outcome;
use lexer::Lexer;

use crate::tokens::Token;

mod base;
mod errors;
mod lexer;
mod tokens;

fn run_mila() -> Outcome<()> {
    let mut lexer = <dyn Lexer>::factory()?;

    loop {
        let token = lexer.next_token()?;
        println!("{token:?}");
        if token.token == Token::EOF {
            break;
        };
    };

    Ok(())
}

fn main() {
    eprintln!("Running");
    match run_mila() {
        Err(e) => {
            let msg = e.msg();
            eprintln!("Failed: {msg}");
            process::exit(1);
        },
        _ => {},
    }
}
