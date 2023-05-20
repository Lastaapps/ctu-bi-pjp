
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

pub struct BracketInfo {
    pub is_square: bool,
    pub is_open: bool,
}

pub enum BuiltInType {
    Dec,
    Inc,
    Writeln,
    Readln,
    Write,
}

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

pub struct TokenInfo {
    pub token: Token,
    pub line: u32,
    pub column: u32,
}

