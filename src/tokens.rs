
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

#[derive(Debug, PartialEq)]
pub struct BracketInfo {
    pub is_square: bool,
    pub is_open: bool,
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

