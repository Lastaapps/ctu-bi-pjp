use std::{vec, iter::Peekable};

use either::Either;

use crate::{ast::{Program, Statement, Scope, Value, Constant, Declaration, Function, Variable, Type, Expr}, lexer::Lexer, base::Outcome, tokens::{TokenInfo, Token, KeywordType, OperatorType, KT, OT, BI}, errors::MilaErr};

struct LexerIterator {
    lexer: Box<dyn Lexer>,
}

impl Iterator for LexerIterator {
    type Item = Outcome<TokenInfo>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.lexer.next_token())
    }
}

pub struct Parser {
    iter: Peekable<LexerIterator>,
}

impl Parser {
    pub fn factory(lexer: Box<dyn Lexer>) -> Parser {
        let iter = LexerIterator { lexer: lexer };
        let peekable = iter.peekable();
        Parser {iter: peekable}
    }

    pub fn parse_ast(&mut self) -> Outcome<Program> {
        parse_program(self)
    }

    #[must_use]
    fn assert_token(&mut self, token: Token) -> Outcome<()> {
        let actual = self.peek()?;
        if token == actual.token {
            self.progress();
            Ok(())
        } else {
            Err(MilaErr::UnexpectedToken { exp: token, act: actual })
        }
    }

    // next token
    pub fn consume(&mut self) -> Outcome<TokenInfo> {
        self.iter.next().unwrap()
    }
    pub fn progress(&mut self) {
        self.iter.next();
    }
    pub fn peek(&mut self) -> Outcome<TokenInfo> {
        self.iter.peek().unwrap().clone() // this hurst, but haven't found a better solution
    }
}

fn parse_program(parser: &mut Parser) -> Outcome<Program> {
    parser.assert_token(Token::Keyword(KeywordType::Program))?;

    let name = {
        let name_token_info = parser.consume()?;
        let name_token = name_token_info.token;
        if let Token::Identifier(name) = name_token {
            name
        } else {
            return Err(MilaErr::MissingProgramName)
        }
    };
    
    parser.assert_token(Token::Operator(OperatorType::Semicolon))?;

    let scope = parse_scope(parser)?;

    Ok(Program {
        name: name,
        scope: scope,
    })
}

fn parse_scope(parser: &mut Parser) -> Outcome<Scope> {
    let mut vars : Vec<Variable> = vec![];
    let mut consts : Vec<Constant> = vec![];
    let mut declarations: Vec<Declaration> = vec![];
    let mut functions: Vec<Function> = vec![];
    let mut main: Option<Statement> = None;

    loop {
        let token_info = parser.peek()?;
        match token_info.token {
            Token::Keyword(KT::Var) => 
                vars.append(&mut parse_var(parser)?),

            Token::Keyword(KT::Const) => 
                consts.append(&mut parse_constant(parser)?),

            Token::Keyword(KT::Function) => {
                let (dec, func_opt) = parse_fun_or_dec(parser)?;
                // declarations.push(decs);
                if let Some(fun) = func_opt {
                    declarations.push(dec);
                    functions.push(fun);
                };
            },

            Token::Keyword(KT::Begin) => {
                if let Some(_) = main {
                    return Err(MilaErr::MissingMainFunction);
                }
                main = Some(parse_main(parser)?);
            }

            Token::EOF => {return Ok(Scope {
                vars: vars,
                constants: consts,
                declarations: declarations,
                functions: functions,
                main: main.ok_or(MilaErr::MissingMainFunction)?,
            }) },
            _ => return Err(MilaErr::InvalidToken { modl: String::new(), act: token_info }),
        };
    };
}

fn parse_var(parser: &mut Parser) -> Outcome<Vec<Variable>> {
    parser.assert_token(Token::Keyword(KT::Var))?;
    let mut vars : Vec<Variable> = vec![];
    
    loop {
        let mut names : Vec<String> = vec![];
        loop {
            let name_info = parser.consume()?;
            if let Token::Identifier(name) = name_info.token {
                names.push(name);
            } else {
                return Err(MilaErr::UnexpectedToken { exp: Token::Identifier(String::from("Idk")), act: name_info })
            };

            let next_token_info = parser.consume()?;
            match next_token_info.token {
                Token::Operator(OT::Comma) => continue,

                Token::Operator(OT::Colon) => {
                    let val_type = parse_type(parser)?;
                    for name in names {
                        vars.push((name, val_type.clone()));
                    };

                    let semicolon_info = parser.consume()?;
                    if Token::Operator(OT::Semicolon) != semicolon_info.token {
                        return Err(MilaErr::UnexpectedToken { exp: Token::Operator(OT::Semicolon), act: semicolon_info })
                    }
                    break;
                },
                _ => return Err(MilaErr::InvalidToken { modl: String::from("Var def error"), act: next_token_info }),
            };
        };

        if let Token::Identifier(_) = parser.peek()?.token {
        } else { break; };
    };

    Ok(vars)
}

fn parse_constant(parser: &mut Parser) -> Outcome<Vec<Constant>> {
    parser.assert_token(Token::Keyword(KT::Const))?;
    let mut consts : Vec<Constant> = vec![];
    
    loop {
        let name = parse_identifier(parser)?;

        parser.assert_token(Token::Operator(OT::Eq))?;

        let parsed_literal = parse_literal(parser)?;
        consts.push((name, parsed_literal));

        if let Token::Identifier(_) = parser.peek()?.token {
        } else { break; };
    };

    Ok(consts)
}

fn parse_literal(parser: &mut Parser) -> Outcome<Value> {
    let token_info = parser.consume()?;
    Ok(match token_info.token {
        Token::Integer(val) => Value::IntValue(val),
        Token::Float(val) => Value::FloatValue(val),
        // TODO string
        _ => return Err(MilaErr::InvalidToken { modl: String::from("Expected a literal"), act: token_info })
    })
}

fn parse_identifier(parser: &mut Parser) -> Outcome<String> {
    let name_info = parser.consume()?;
    if let Token::Identifier(name) = name_info.token {
        Ok(name)
    } else {
        Err(MilaErr::UnexpectedToken { exp: Token::Identifier(String::from("Expected an identifier")), act: name_info })
    }
}

// required for literals while creating an array type
// as it is required at the compile time
fn parse_next_int(parser: &mut Parser) -> Outcome<i64> {
    let token_info = parser.consume()?;
    match token_info.token {
        Token::Integer(val) => Ok(val as i64),
        Token::Operator(OT::Plus) => parse_next_int(parser),
        Token::Operator(OT::Minus) => parse_next_int(parser).map(|x| -1 * x),
        _ => Err(MilaErr::InvalidToken { modl: String::from("Expected an integer for array indexing"), act: token_info }),
    }
}

fn parse_type(parser: &mut Parser) -> Outcome<Type> {
    let token_info = parser.consume()?;
    Ok(match token_info.token {
        Token::Keyword(KT::Integer) => Type::Integer,

        Token::Keyword(KT::Float) => Type::Float,

        Token::Keyword(KT::Array) => {
            parser.assert_token(Token::Bracket(BI{is_square: true, is_open: true}))?;
            
            let index_from = parse_next_int(parser)?;

            parser.assert_token(Token::Operator(OT::Ranges))?;

            let index_to = parse_next_int(parser)?;

            parser.assert_token(Token::Bracket(BI{is_square: true, is_open: false}))?;
            parser.assert_token(Token::Keyword(KT::Of))?;

            let arr_type = parse_type(parser)?;

            Type::Array(Box::new(arr_type), index_from, index_to)
        },
        _ => return Err(MilaErr::InvalidToken { modl: String::from("Expected a literal"), act: token_info })
    })
}

fn parse_fun_or_dec(parser: &mut Parser) -> Outcome<(Declaration, Option<Function>)> {
    parser.assert_token(Token::Keyword(KT::Function))?;

    let name = parse_identifier(parser)?;

    parser.assert_token(Token::Bracket(BI{is_square: false, is_open: true}))?;
    
    let params = parse_params(parser)?;

    parser.assert_token(Token::Bracket(BI{is_square: false, is_open: false}))?;
    parser.assert_token(Token::Operator(OT::Colon))?;

    let return_type = parse_type(parser)?;
    let declaration = Declaration{name: name.clone(), params: params, return_type: return_type};

    if let Token::Keyword(KT::Forward) = parser.peek()?.token {
        parser.consume()?;
        parser.assert_token(Token::Operator(OT::Semicolon))?;
        return Ok((declaration, None));
    }

    let vars: Vec<Variable> = if let Token::Keyword(KT::Var) = parser.peek()?.token {
        parse_var(parser)?
    } else { vec![] };

    let code = parse_block(parser)?;
    
    Ok((declaration, Some(Function { name: name, vars: vars, scope: Box::new(code) })))
}

fn parse_params(parser: &mut Parser) -> Outcome<Vec<Variable>>{
    if let Token::Bracket(BI{is_square: false, is_open: false}) = parser.peek()?.token {
        return Ok(vec![]);
    }

    let mut params: Vec<Variable> = vec![];
    loop {
        let name = parse_identifier(parser)?;

        parser.assert_token(Token::Operator(OT::Colon))?;

        let param_type = parse_type(parser)?;

        params.push((name, param_type));

        if let Token::Operator(OT::Semicolon) = parser.peek()?.token {
            parser.consume()?;
        } else { break; }
    };
    Ok(params)
}

fn parse_main(parser: &mut Parser) -> Outcome<Statement> {
    let code = parse_block(parser)?;
    parser.assert_token(Token::Operator(OT::Comma))?;
    Ok(code)
}

fn parse_block(parser: &mut Parser) -> Outcome<Statement> {
    parser.assert_token(Token::Keyword(KT::Begin))?;
    
    let mut statements: Vec<Statement> = vec![];

    loop {
        let stmt = parse_statement(parser)?;
        statements.push(stmt);

        if let Token::Operator(OT::Semicolon) = parser.peek()?.token {
            parser.consume()?;
        } else { break; }

        if let Token::Keyword(KT::End) = parser.peek()?.token {
            break;
        }
    }

    parser.assert_token(Token::Keyword(KT::End))?;

    Ok(Statement::Block { statements: statements })
}

fn parse_statement(parser: &mut Parser) -> Outcome<Statement> {
    let next_token = parser.peek()?;
    match next_token.token {
        // TODO assign
        // TODO expr
        Token::Keyword(KT::For) => parse_for(parser),
        Token::Keyword(KT::While) => parse_while(parser),
        Token::Keyword(KT::If) => parse_if(parser),
        Token::Keyword(KT::Exit) => Ok(Statement::Exit),
        _ => Err(MilaErr::InvalidToken { modl: String::from("Invalid statement start"), act: next_token })
    }
}

fn parse_block_or_statement(parser: &mut Parser) -> Outcome<Statement> {
    if let Token::Keyword(KT::Begin) = parser.peek()?.token {
        parse_block(parser)
    } else {
        parse_statement(parser)
    }
}

fn parse_for(parser: &mut Parser) -> Outcome<Statement> {
    parser.assert_token(Token::Keyword(KT::For))?;

    // TODO

    let dir_token = parser.consume()?;
    let dir = match dir_token.token {
        Token::Operator(OT::To) => true,
        Token::Operator(OT::Downto) => false,
        _ => return Err(MilaErr::InvalidToken { modl: String::from("For wrong direction"), act: dir_token }),
    };

    let limit_boundary = parse_expr(parser)?;

    parser.assert_token(Token::Keyword(KT::Do))?;

    let code = parse_block_or_statement(parser)?;

    ;todo!()
}

fn parse_while(parser: &mut Parser) -> Outcome<Statement> {
    parser.assert_token(Token::Keyword(KT::While))?;

    let condition = parse_expr(parser)?;

    parser.assert_token(Token::Keyword(KT::Do))?;

    let code = parse_block_or_statement(parser)?;

    Ok(Statement::While { cond:condition, scope: Box::new(code) })
}

fn parse_if(parser: &mut Parser) -> Outcome<Statement> {
    parser.assert_token(Token::Keyword(KT::If))?;

    let condition = parse_expr(parser)?;

    parser.assert_token(Token::Keyword(KT::Then))?;
    let true_branch = parse_block_or_statement(parser)?;

    if Token::Keyword(KT::Else) != parser.peek()?.token {
        return Ok(Statement::If { cond: condition, true_branch: Box::new(true_branch) })
    };
    parser.consume()?;
    let false_branch = parse_block_or_statement(parser)?;

    Ok(Statement::IfElse { cond: condition, true_branch: Box::new(true_branch), false_branch: Box::new(false_branch) })
}

fn parse_expr(parser: &mut Parser) -> Outcome<Expr> {
    parse_op8(parser)

    // let mut lhs = parse_op(parser)?;
    // let peeked = parser.peek()?.token;

    // left associativity example
    // while Token::Operator(OT::) == parser.peek()?.token {
    //     parser.consume()?;
    //     let rhs = parse_op(parser)?;
    //     lhs = Expr::(Box::new(lhs), Box::new(rhs));
    // };

    // right associativity example
    // if Token::Operator(OT::) == parser.peek()?.token {
    //     parser.consume()?;
    //     let rhs = parse_op(parser)?;
    //     lhs = Expr::(Box::new(expr), Box::new(rhs));
    // };
}

// and
fn parse_op8(parser: &mut Parser) -> Outcome<Expr> {
    let mut lhs = parse_op7(parser)?;
    let peeked = parser.peek()?.token;

    while Token::Operator(OT::And) == peeked {
        parser.consume()?;
        let rhs = parse_op7(parser)?;
        lhs = Expr::And(Box::new(lhs), Box::new(rhs));
    };
    Ok(lhs)
}

// or
fn parse_op7(parser: &mut Parser) -> Outcome<Expr> {
    let mut lhs = parse_op6(parser)?;
    let peeked = parser.peek()?.token;

    while Token::Operator(OT::Or) == peeked {
        parser.consume()?;
        let rhs = parse_op6(parser)?;
        lhs = Expr::Or(Box::new(lhs), Box::new(rhs));
    };
    Ok(lhs)
}

// eq, ne
fn parse_op6(parser: &mut Parser) -> Outcome<Expr> {
    let mut lhs = parse_op5(parser)?;
    let peeked = parser.peek()?.token;

    loop {
        if Token::Operator(OT::Eq) == peeked {
            parser.consume()?;
            let rhs = parse_op5(parser)?;
            lhs = Expr::Eq(Box::new(lhs), Box::new(rhs));
        } else
        if Token::Operator(OT::Ne) == peeked {
            parser.consume()?;
            let rhs = parse_op5(parser)?;
            lhs = Expr::Ne(Box::new(lhs), Box::new(rhs));
        } else
        { break; }
    };
    Ok(lhs)
}

// gt, ge, lt, le
fn parse_op5(parser: &mut Parser) -> Outcome<Expr> {
    let mut lhs = parse_op4(parser)?;
    let peeked = parser.peek()?.token;

    loop {
        if Token::Operator(OT::Gt) == peeked {
            parser.consume()?;
            let rhs = parse_op4(parser)?;
            lhs = Expr::Gt(Box::new(lhs), Box::new(rhs));
        } else 
        if Token::Operator(OT::Ge) == peeked {
            parser.consume()?;
            let rhs = parse_op4(parser)?;
            lhs = Expr::Ge(Box::new(lhs), Box::new(rhs));
        } else 
        if Token::Operator(OT::Lt) == peeked {
            parser.consume()?;
            let rhs = parse_op4(parser)?;
            lhs = Expr::Lt(Box::new(lhs), Box::new(rhs));
        } else 
        if Token::Operator(OT::Le) == peeked {
            parser.consume()?;
            let rhs = parse_op4(parser)?;
            lhs = Expr::Le(Box::new(lhs), Box::new(rhs));
        } else 
        { break; }
    };
    Ok(lhs)
}

// mul, div, mod
fn parse_op4(parser: &mut Parser) -> Outcome<Expr> {
    let mut lhs = parse_op3(parser)?;
    let peeked = parser.peek()?.token;

    loop {
        if Token::Operator(OT::Mul) == peeked {
            parser.consume()?;
            let rhs = parse_op3(parser)?;
            lhs = Expr::Mul(Box::new(lhs), Box::new(rhs));
        } else 
        if Token::Operator(OT::Div) == peeked {
            parser.consume()?;
            let rhs = parse_op3(parser)?;
            lhs = Expr::Div(Box::new(lhs), Box::new(rhs));
        } else 
        if Token::Operator(OT::Mod) == peeked {
            parser.consume()?;
            let rhs = parse_op3(parser)?;
            lhs = Expr::Mod(Box::new(lhs), Box::new(rhs));
        } else 
        { break; }
    };
    Ok(lhs)
}

fn parse_op3(parser: &mut Parser) -> Outcome<Expr> {
    let mut lhs = parse_op2(parser)?;
    let peeked = parser.peek()?.token;

    loop {
        if Token::Operator(OT::Plus) == peeked {
            parser.consume()?;
            let rhs = parse_op2(parser)?;
            lhs = Expr::Add(Box::new(lhs), Box::new(rhs));
        } else 
        if Token::Operator(OT::Minus) == peeked {
            parser.consume()?;
            let rhs = parse_op2(parser)?;
            lhs = Expr::Sub(Box::new(lhs), Box::new(rhs));
        } else 
        { break; }
    };
    Ok(lhs)
}

fn parse_op2(parser: &mut Parser) -> Outcome<Expr> {
    let peeked = parser.peek()?.token;
    Ok(if Token::Operator(OT::Plus) == peeked {
        parser.consume()?;
        parse_op2(parser)?
    } else 
    if Token::Operator(OT::Minus) == peeked {
        parser.consume()?;
        let rhs = parse_op2(parser)?;
        Expr::Sub(Box::new(Expr::Literal(Value::IntValue(0))), Box::new(rhs))
    } else
    { parse_op1(parser)? })
}

fn parse_op1(parser: &mut Parser) -> Outcome<Expr> {
    let peeked = parser.peek()?.token;
    if Token::Bracket(BI { is_square: false, is_open: true}) == peeked {
        parser.consume()?;
        let expr = parse_expr(parser)?;
        parser.assert_token(Token::Bracket(BI { is_square: false, is_open: true}))?;
        return Ok(expr);
    };
    todo!()
}
