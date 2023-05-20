
use std::{io::Read, vec::IntoIter, iter::Peekable};

use crate::{tokens::{TokenInfo}, base::Outcome, errors::MilaError};

pub trait Lexer {
    fn next_token(&self) -> Outcome<TokenInfo>;
}

struct LexerImpl {
    itr: Peekable<IntoIter<char>>,
    line: u32,
    col: u32,
}

impl LexerImpl {
    fn new() -> Outcome<Self> {
        let mut buff = String::new();

        std::io::stdin()
        .read_to_string(&mut buff)
        .map_err(|_| {MilaError::ReadStdInFailed})?;

        let itr = buff.chars().collect::<Vec<_>>().into_iter();
        let peekable = itr.peekable();
        Ok(Self {itr: peekable, line: 1, col: 0})
    }

    fn next(&mut self) -> Option<char> {
        self.itr.next()
    }
    fn peek(&mut self) -> Option<&char> {
        self.itr.peek()
    }
}

impl Lexer for LexerImpl {
    fn next_token() -> Outcome<TokenInfo> {
        
    }
}
