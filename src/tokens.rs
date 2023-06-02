use std::fmt::{Display, Debug};


#[derive(Debug, PartialEq)]
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
    For,
    Do,
    Array,
}

#[derive(Debug, PartialEq)]
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

#[derive(PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum BuiltInType {
    Dec,
    Inc,
    Writeln,
    Readln,
    Write,
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug)]
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
