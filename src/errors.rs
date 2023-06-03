use std::fmt::Display;

use crate::tokens::{Token, TokenInfo};


#[derive(Clone)]
pub enum MilaErr {
    // lexer
    ReadStdInFailed(String),
    ReadFileFailed(String, String),
    EOFReached{line: u32, col: u32},
    UnexpectedChar{c: char, line: u32, col: u32},
    UnexpectedNumberEnd{c: char, line: u32, col: u32},
    NoTokenMatched{line: u32, col: u32},

    // parser
    UnexpectedToken {
        expected: Token,
        actual: TokenInfo,
    },
    MissingProgramName,
}

impl Display for MilaErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // lexer
            Self::ReadStdInFailed(e) => 
                write!(f, "Failed to read stdin - {}", e),
            Self::ReadFileFailed(e, name) => 
                write!(f, "Failed to read file {name} - {}", e),
            Self::EOFReached { line, col } => 
                write!(f, "EOF reached to soon at {line}:{col}"),
            Self::UnexpectedChar { c, line, col } => 
                write!(f, "Unexpected char '{c}' at {line}:{col}"),
            Self::UnexpectedNumberEnd { c, line, col } => 
                write!(f, "Unexpected number end '{c}' at {line}:{col}"),
            Self::NoTokenMatched { line, col } => 
                write!(f, "No token matched at {line}:{col}"),

            // parser
            Self::UnexpectedToken { expected, actual } => 
                write!(f, "Unexpected token: Exp {:?}, Actual {}", expected, actual),
            Self::MissingProgramName =>
                write!(f, "Missing program name"),
        }
    }
}
