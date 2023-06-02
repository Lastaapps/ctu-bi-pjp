use std::fmt::Display;


pub enum MilaErr {
    ReadStdInFailed(std::io::Error),
    ReadFileFailed(std::io::Error, String),
    EOFReached{line: u32, col: u32},
    UnexpectedChar{c: char, line: u32, col: u32},
    UnexpectedNumberEnd{c: char, line: u32, col: u32},
    NoTokenMatched{line: u32, col: u32},
}

impl Display for MilaErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ReadStdInFailed(_) => 
                write!(f, "Failed to read stdin"),
            Self::ReadFileFailed(_, name) => 
                write!(f, "Failed to read file {name}"),
            Self::EOFReached { line, col } => 
                write!(f, "EOF reached to soon at {line}:{col}"),
            Self::UnexpectedChar { c, line, col } => 
                write!(f, "Unexpected char '{c}' at {line}:{col}"),
            Self::UnexpectedNumberEnd { c, line, col } => 
                write!(f, "Unexpected number end '{c}' at {line}:{col}"),
            Self::NoTokenMatched { line, col } => 
                write!(f, "No token matched at {line}:{col}"),
        }
    }
}
