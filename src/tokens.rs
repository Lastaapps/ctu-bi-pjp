use std::fmt::{Display, Debug};


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
    Ranges,
    Downto,
    To,
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
    Comma,
    Semicolon,
    Colon,
}

pub type BI = BracketInfo;
#[derive(PartialEq, Clone)]
pub struct BracketInfo {
    pub is_square: bool,
    pub is_open: bool,
}

impl Display for BracketInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{s:{}, o:{}}}", self.is_square, self.is_open)
    }
}
impl Debug for BracketInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BI")
            .field("sqr", &self.is_square)
            .field("opn", &self.is_open)
            .finish()
    }
}

pub type BIT = BuiltInType;
#[derive(Debug, PartialEq, Clone)]
pub enum BuiltInType {
    Dec,
    Inc,
    Writeln,
    Readln,
    Write,
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
