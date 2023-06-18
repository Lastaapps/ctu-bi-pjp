use crate::tokens::BuiltInType;


#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Integer,
    Float,
    String,
    Array(Box<Type>, i64, i64),
    Void,
}

#[derive(Debug, PartialEq)]
pub enum Value {
    IntValue(u64),
    FloatValue(f64),
    StringValue(String),
}

impl Value {
    pub fn to_type(self) -> Type {
        match self {
            Value::IntValue(_) => Type::Integer,
            Value::FloatValue(_) => Type::Float,
            Value::StringValue(_) => Type::String,
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

pub type Variable = (String, Type);
pub type Constant = (String, Value);

#[derive(Debug, PartialEq)]
pub struct Declaration {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub vars: Vec<(String, Type)>,
    pub scope: Box<Statement>,
}

type BExpr = Box<Expr>;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Add(BExpr, BExpr),
    Sub(BExpr, BExpr),
    Mul(BExpr, BExpr),
    Div(BExpr, BExpr),
    Mod(BExpr, BExpr),
    Gt (BExpr, BExpr),
    Ge (BExpr, BExpr),
    Lt (BExpr, BExpr),
    Le (BExpr, BExpr),
    Eq (BExpr, BExpr),
    Ne (BExpr, BExpr),
    And(BExpr, BExpr),
    Or (BExpr, BExpr),

    Literal(Value),
    FunCall{
        name: String,
        args: Vec<Expr>,
    },
    BuiltIn{
        name: BuiltInType,
        args: Vec<Expr>,
    },
    VarAccess(String),
    ArrayAccess(BExpr, BExpr),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Block {
        statements: Vec<Statement>,
    },
    ExprWrapper(Expr),
    Assign{
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
