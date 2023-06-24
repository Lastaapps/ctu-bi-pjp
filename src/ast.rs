use crate::tokens::BuiltInType;

#[derive(Debug, PartialEq, Clone)]
pub enum Kind {
    Integer,
    Float,
    String,
    Array(Box<Kind>, i64, i64),
    Void,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    IntValue(u64),
    FloatValue(f64),
    StringValue(String),
}

impl Value {
    pub fn to_type(&self) -> Kind {
        match self {
            Value::IntValue(_) => Kind::Integer,
            Value::FloatValue(_) => Kind::Float,
            Value::StringValue(_) => Kind::String,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub name: String,
    pub scope: Scope,
}

#[derive(Debug, PartialEq)]
pub struct Scope {
    pub vars: Vec<Variable>,
    pub constants: Vec<Constant>,
    pub declarations: Vec<Declaration>,
    pub functions: Vec<Function>,
    pub main: Statement,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable {
    pub name: String,
    pub kind: Kind,
}

#[derive(Debug, PartialEq)]
pub struct Constant {
    pub name: String,
    pub val: Value,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Declaration {
    pub name: String,
    pub params: Vec<Variable>,
    pub return_type: Kind,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub vars: Vec<Variable>,
    pub statement: Box<Statement>,
}

type BExpr = Box<Expr>;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Add(BExpr, BExpr),
    Sub(BExpr, BExpr),
    Mul(BExpr, BExpr),
    Div(BExpr, BExpr),
    Mod(BExpr, BExpr),
    Gt(BExpr, BExpr),
    Ge(BExpr, BExpr),
    Lt(BExpr, BExpr),
    Le(BExpr, BExpr),
    Eq(BExpr, BExpr),
    Ne(BExpr, BExpr),
    And(BExpr, BExpr),
    Or(BExpr, BExpr),

    Literal(Value),
    FunCall { name: String, args: Vec<Expr> },
    BuiltIn { kind: BuiltInType, args: Vec<Expr> },
    VarAccess(String),
    ArrayAccess(BExpr, BExpr),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Block {
        statements: Vec<Statement>,
    },
    ExprWrapper(Expr),
    Assign {
        space: Expr,
        expr: Expr,
    },
    For {
        var_name: String,
        from: Expr,
        to: Expr,
        is_to: bool,
        scope: Box<Statement>,
    },
    While {
        cond: Expr,
        scope: Box<Statement>,
    },
    If {
        cond: Expr,
        true_branch: Box<Statement>,
    },
    IfElse {
        cond: Expr,
        true_branch: Box<Statement>,
        false_branch: Box<Statement>,
    },
    Exit,
}
