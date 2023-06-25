use std::fmt::{Debug, Display};

pub type KT = KeywordType;
#[derive(Debug, PartialEq, Clone)]
pub enum KeywordType {
    Begin,
    End,
    Const,
    Procedure,
    Forward,
    Function,
    If,
    Then,
    Else,
    Program,
    While,
    Exit,
    Break,
    Continue,
    Var,
    Integer,
    Float,
    For,
    Do,
    Array,
    Of,
}

pub type OT = OperatorType;
#[derive(Debug, PartialEq, Clone)]
pub enum OperatorType {
    Assign,
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
    And,
    Or,
    Xor,
    Not,
    Ranges,
    DownTo,
    To,
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    CastToInt,
    CastToFloat,
    Comma,     // ,
    Semicolon, // ;
    Colon,     // :
    Dot,       // .
}

pub type BI = BracketInfo;
#[derive(PartialEq, Clone)]
pub struct BracketInfo {
    pub sq: bool, // square
    pub op: bool, // open
}

impl Display for BracketInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{s:{}, o:{}}}", self.sq, self.op)
    }
}
impl Debug for BracketInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BI")
            .field("sqr", &self.sq)
            .field("opn", &self.op)
            .finish()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum BuiltInType {
    Dec,
    Inc,
    Write,
    WriteLine,
    ReadLine,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    BuiltIn(BuiltInType),
    Keyword(KeywordType),
    Integer(u64),
    Float(f64),
    Operator(OperatorType),
    Bracket(BracketInfo),
    String(String),
    EOF,
}

#[derive(Debug, Clone)]
pub struct TokenInfo {
    pub token: Token,
    pub line: u32,
    pub column: u32,
}

impl Display for TokenInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TI <{:2}:{:2}> {:?}", self.line, self.column, self.token)
    }
}
