
pub enum Type {
    Integer,
    Float,
    String,
    Array(Box<Type>, u32),
}

pub enum Value {
    IntValue(i64),
    FloatValue(f64),
    StringValue(String),
}

impl Value {
    fn to_type(self) -> Type {
        match self {
            Value::IntValue(_) => Type::Integer,
            Value::FloatValue(_) => Type::Float,
            Value::StringValue(_) => Type::String,
        }
    }
}

pub struct Program {
    name: String,
    vars: Vec<(String, Type)>,
    constants: Vec<(String, Value)>,
    declarations: Vec<Declaration>,
    functions: Vec<Function>,
    main: Vec<Statement>,
}

struct Declaration {
    name: String,
    params: Vec<(String, Type)>,
    return_type: Type,
}

struct Function {
    name: String,
    vars: Vec<(String, Type)>,
    scope: Box<Statement>,
}

pub enum MemorySpace {
    VarAccess {
        name: String,
        var_type: Type,
    },
    ArrayAccess {
        name: String,
        expr: Box<Expr>,
        var_type: Type,
    }
}

type BExpr = Box<Expr>;

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
    Xor(BExpr, BExpr),

    Literal(Value),
    FunCall{
        name: String,
        params: Vec<BExpr>,
    },
    MemAccess(Box<MemorySpace>),
}


pub enum Statement {
    Block {
        statements: Vec<Statement>,
    },
    ExprWrapper(Expr),
    Assign{
        space: Box<MemorySpace>,
        expr: Expr,
    },
    For {
        var_name: String,
        from: i64,
        to: i64,
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
}
