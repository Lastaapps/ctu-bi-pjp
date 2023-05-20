
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
    is_square: bool,
    is_left: bool,
}

pub enum BuiltInType {
    Dec,
    Inc,
    Writeln,
    Readln,
    Write,
}

pub enum Token {
    Name(String),
    BuiltIn(BuiltInType),
    Keyword(KeywordType),
    Integer(i64),
    Float(f64),
    Operator(OperatorType),
    Bracket(BracketInfo),
}

pub struct TokenInfo {
    token: Token,
    line: u32,
    column: u32,
}

