use std::fmt::Display;

use crate::{tokens::{Token, TokenInfo}, ast::Expr};


#[derive(Clone)]
pub enum MilaErr {
    // lexer
    ReadStdInFailed(String),
    ReadFileFailed(String, String),
    EOFReached{line: u32, col: u32},
    UnexpectedChar{c: char, line: u32, col: u32},
    UnexpectedNumberEnd{c: char, line: u32, col: u32},
    NoTokenMatched{line: u32, col: u32},
    UnclosedString,
    UnknownEscapeSequence(char),


    // parser
    UnexpectedToken {
        exp: Token,
        act: TokenInfo,
    },
    InvalidToken {
        msg: String,
        act: TokenInfo,
    },
    MissingProgramName,
    MissingMainFunction,
    InvalidArrayRange{ from: i64, to: i64 },

    // llvm
    DuplicateGlobal(String),
    VoidAsVariable,
    WrongCast,
    AssignNotSupported(Expr),
    CannotIndexWithNonInteger,
    CannotUseIndexingOnNonArrayType{code: u8},
    VarNotFound(String),
    CannotChangeConstantVariable(String),
    LogicOnIntOnly,
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
            Self::UnclosedString => 
                write!(f, "Unclosed string, add \" at the end"),
            Self::UnknownEscapeSequence(c) => 
                write!(f, "Unknown escape sequence: '\\{c}'"),

            // parser
            Self::UnexpectedToken { exp: expected, act: actual } => 
                write!(f, "Unexpected token: Exp {:?}, Actual {}", expected, actual),
            Self::InvalidToken { msg: module, act: actual } => 
                write!(f, "Unexpected token: {}, got {}", module, actual),
            Self::MissingProgramName =>
                write!(f, "Missing the program name"),
            Self::MissingMainFunction =>
                write!(f, "Missing the main function"),
            Self::InvalidArrayRange { from, to } =>
                write!(f, "Invalid array range: {from} .. {to}"),

            // llvm
            Self::DuplicateGlobal(name) =>
                write!(f, "Duplicate global variable or constant: {name}"),
            Self::VoidAsVariable =>
                write!(f, "Variable cannot be of the type: void"),
            Self::WrongCast =>
                write!(f, "Wrong cast, somewhere..."),
            Self::AssignNotSupported(expr) =>
                write!(f, "Assign to {:?} not supported", expr),
            Self::CannotIndexWithNonInteger =>
                write!(f, "Cannot uses non integer for indexing"),
            Self::CannotUseIndexingOnNonArrayType { code } =>
                write!(f, "Cannot use index on non-array type, code: {code}"),
            Self::VarNotFound(name) =>
                write!(f, "Variable with name {name} not found"),
            Self::CannotChangeConstantVariable(name) =>
                write!(f, "Constant {name} cannot be changed"),
            Self::LogicOnIntOnly(name) =>
                write!(f, "Logic operations can be performed on integers only"),
        }
    }
}
