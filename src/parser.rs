use std::{iter::Peekable, vec};

use crate::{
    ast::{
        Constant, Declaration, Expr, Function, Kind, Program, Scope, Statement, Value, Variable,
    },
    base::Outcome,
    errors::MilaErr,
    lexer::Lexer,
    tokens::{KeywordType, OperatorType, Token, TokenInfo, BI, KT, OT},
};

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
        Parser { iter: peekable }
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
            Err(MilaErr::UnexpectedToken {
                exp: token,
                act: actual,
            })
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
            return Err(MilaErr::MissingProgramName);
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
    let mut vars: Vec<Variable> = vec![];
    let mut consts: Vec<Constant> = vec![];
    let mut declarations: Vec<Declaration> = vec![];
    let mut functions: Vec<Function> = vec![];
    let mut main: Option<Statement> = None;

    loop {
        let token_info = parser.peek()?;
        match token_info.token {
            Token::Keyword(KT::Var) => vars.append(&mut parse_var(parser)?),

            Token::Keyword(KT::Const) => consts.append(&mut parse_constant(parser)?),

            Token::Keyword(KT::Function) | Token::Keyword(KT::Procedure) => {
                let (dec, func_opt) = parse_fun_or_dec(parser)?;
                // declarations.push(decs);
                if let Some(fun) = func_opt {
                    declarations.push(dec);
                    functions.push(fun);
                };
            }

            Token::Keyword(KT::Begin) => {
                if let Some(_) = main {
                    return Err(MilaErr::MissingMainFunction);
                }
                main = Some(parse_main(parser)?);
            }

            Token::EOF => {
                return Ok(Scope {
                    vars: vars,
                    constants: consts,
                    declarations: declarations,
                    functions: functions,
                    main: main.ok_or(MilaErr::MissingMainFunction)?,
                })
            }
            _ => {
                return Err(MilaErr::InvalidToken {
                    msg: String::from("Failed to parse the app scope"),
                    act: token_info,
                })
            }
        };
    }
}

fn parse_var(parser: &mut Parser) -> Outcome<Vec<Variable>> {
    parser.assert_token(Token::Keyword(KT::Var))?;
    let mut vars: Vec<Variable> = vec![];

    loop {
        let mut names: Vec<String> = vec![];
        loop {
            let name_info = parser.consume()?;
            if let Token::Identifier(name) = name_info.token {
                names.push(name);
            } else {
                return Err(MilaErr::UnexpectedToken {
                    exp: Token::Identifier(String::from("Idk")),
                    act: name_info,
                });
            };

            let next_token_info = parser.consume()?;
            match next_token_info.token {
                Token::Operator(OT::Comma) => continue,

                Token::Operator(OT::Colon) => {
                    let val_kind = parse_type(parser)?;
                    for name in names {
                        vars.push(Variable {
                            name: name,
                            kind: val_kind.clone(),
                        });
                    }

                    let semicolon_info = parser.consume()?;
                    if Token::Operator(OT::Semicolon) != semicolon_info.token {
                        return Err(MilaErr::UnexpectedToken {
                            exp: Token::Operator(OT::Semicolon),
                            act: semicolon_info,
                        });
                    }
                    break;
                }
                _ => {
                    return Err(MilaErr::InvalidToken {
                        msg: String::from("Var def error"),
                        act: next_token_info,
                    })
                }
            };
        }

        if let Token::Identifier(_) = parser.peek()?.token {
        } else {
            break;
        };
    }

    Ok(vars)
}

fn parse_constant(parser: &mut Parser) -> Outcome<Vec<Constant>> {
    parser.assert_token(Token::Keyword(KT::Const))?;
    let mut consts: Vec<Constant> = vec![];

    loop {
        let name = parse_identifier(parser)?;

        parser.assert_token(Token::Operator(OT::Eq))?;

        let parsed_literal = parse_literal(parser)?;
        consts.push(Constant {
            name: name,
            val: parsed_literal,
        });

        parser.assert_token(Token::Operator(OT::Semicolon))?;

        if let Token::Identifier(_) = parser.peek()?.token {
        } else {
            break;
        };
    }

    Ok(consts)
}

fn parse_literal(parser: &mut Parser) -> Outcome<Value> {
    let token_info = parser.consume()?;
    Ok(match token_info.token {
        Token::Integer(val) => Value::IntValue(val),
        Token::Float(val) => Value::FloatValue(val),
        Token::String(val) => Value::StringValue(val),
        _ => {
            return Err(MilaErr::InvalidToken {
                msg: String::from("Expected a literal"),
                act: token_info,
            })
        }
    })
}

fn parse_identifier(parser: &mut Parser) -> Outcome<String> {
    let name_info = parser.consume()?;
    if let Token::Identifier(name) = name_info.token {
        Ok(name)
    } else {
        Err(MilaErr::UnexpectedToken {
            exp: Token::Identifier(String::from("Expected an identifier")),
            act: name_info,
        })
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
        _ => Err(MilaErr::InvalidToken {
            msg: String::from("Expected an integer for array indexing"),
            act: token_info,
        }),
    }
}

fn parse_type(parser: &mut Parser) -> Outcome<Kind> {
    let token_info = parser.consume()?;
    Ok(match token_info.token {
        Token::Keyword(KT::Integer) => Kind::Integer,

        Token::Keyword(KT::Float) => Kind::Float,

        Token::Keyword(KT::Array) => {
            parser.assert_token(Token::Bracket(BI { sq: true, op: true }))?;

            let index_from = parse_next_int(parser)?;

            parser.assert_token(Token::Operator(OT::Ranges))?;

            let index_to = parse_next_int(parser)?;

            parser.assert_token(Token::Bracket(BI {
                sq: true,
                op: false,
            }))?;
            parser.assert_token(Token::Keyword(KT::Of))?;

            let arr_type = parse_type(parser)?;

            if index_from > index_to {
                return Err(MilaErr::InvalidArrayRange {
                    from: index_from,
                    to: index_to,
                });
            }
            Kind::Array(Box::new(arr_type), index_from, index_to)
        }
        _ => {
            return Err(MilaErr::InvalidToken {
                msg: String::from("Expected a literal"),
                act: token_info,
            })
        }
    })
}

fn parse_fun_or_dec(parser: &mut Parser) -> Outcome<(Declaration, Option<Function>)> {
    let starting = parser.consume()?;
    let is_procedure = match starting.token {
        Token::Keyword(KT::Function) => false,
        Token::Keyword(KT::Procedure) => true,
        _ => {
            return Err(MilaErr::UnexpectedToken {
                exp: Token::Keyword(KT::Function),
                act: starting,
            })
        }
    };

    let name = parse_identifier(parser)?;

    parser.assert_token(Token::Bracket(BI {
        sq: false,
        op: true,
    }))?;

    let params = parse_params(parser)?;

    parser.assert_token(Token::Bracket(BI {
        sq: false,
        op: false,
    }))?;

    let return_type = if !is_procedure {
        parser.assert_token(Token::Operator(OT::Colon))?;
        parse_type(parser)?
    } else {
        Kind::Void
    };
    parser.assert_token(Token::Operator(OT::Semicolon))?;

    let declaration = Declaration {
        name: name.clone(),
        params: params,
        return_type: return_type,
    };

    if let Token::Keyword(KT::Forward) = parser.peek()?.token {
        parser.consume()?;
        parser.assert_token(Token::Operator(OT::Semicolon))?;
        return Ok((declaration, None));
    }

    let vars: Vec<Variable> = if let Token::Keyword(KT::Var) = parser.peek()?.token {
        parse_var(parser)?
    } else {
        vec![]
    };

    let code = parse_block(parser)?;
    parser.assert_token(Token::Operator(OT::Semicolon))?;

    Ok((
        declaration,
        Some(Function {
            name: name,
            local_vars: vars,
            statement: Box::new(code),
        }),
    ))
}

fn parse_params(parser: &mut Parser) -> Outcome<Vec<Variable>> {
    if let Token::Bracket(BI {
        sq: false,
        op: false,
    }) = parser.peek()?.token
    {
        return Ok(vec![]);
    }

    let mut params: Vec<Variable> = vec![];
    loop {
        let name = parse_identifier(parser)?;

        parser.assert_token(Token::Operator(OT::Colon))?;

        let kind = parse_type(parser)?;

        params.push(Variable {
            name: name,
            kind: kind,
        });

        if let Token::Operator(OT::Semicolon) = parser.peek()?.token {
            parser.consume()?;
        } else {
            break;
        }
    }
    Ok(params)
}

fn parse_main(parser: &mut Parser) -> Outcome<Statement> {
    let code = parse_block(parser)?;
    parser.assert_token(Token::Operator(OT::Dot))?;
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
        } else {
            break;
        }

        if let Token::Keyword(KT::End) = parser.peek()?.token {
            break;
        }
    }

    parser.assert_token(Token::Keyword(KT::End))?;

    Ok(Statement::Block {
        statements: statements,
    })
}

fn parse_statement(parser: &mut Parser) -> Outcome<Statement> {
    let next_token = parser.peek()?;
    match next_token.token {
        Token::Keyword(KT::For) => parse_for(parser),
        Token::Keyword(KT::While) => parse_while(parser),
        Token::Keyword(KT::If) => parse_if(parser),
        Token::Keyword(KT::Exit) => {
            parser.consume()?;
            Ok(Statement::Exit)
        }
        _ => parse_assign_or_expr(parser),
        // _ => Err(MilaErr::InvalidToken { msg: String::from("Invalid statement start"), act: next_token })
    }
}

fn parse_block_or_statement(parser: &mut Parser) -> Outcome<Statement> {
    if let Token::Keyword(KT::Begin) = parser.peek()?.token {
        parse_block(parser)
    } else {
        parse_statement(parser)
    }
}

fn parse_assign_or_expr(parser: &mut Parser) -> Outcome<Statement> {
    let lhs = parse_expr(parser)?;
    if Token::Operator(OT::Assign) != parser.peek()?.token {
        return Ok(Statement::ExprWrapper(lhs));
    }
    parser.consume()?;
    let rhs = parse_expr(parser)?;

    Ok(Statement::Assign {
        space: lhs,
        expr: rhs,
    })
}

fn parse_for(parser: &mut Parser) -> Outcome<Statement> {
    parser.assert_token(Token::Keyword(KT::For))?;

    let var_name = parse_identifier(parser)?;
    parser.assert_token(Token::Operator(OT::Assign))?;
    let start_index = parse_expr(parser)?;

    let dir_token = parser.consume()?;
    let dir = match dir_token.token {
        Token::Operator(OT::To) => true,
        Token::Operator(OT::DownTo) => false,
        _ => {
            return Err(MilaErr::InvalidToken {
                msg: String::from("For wrong direction"),
                act: dir_token,
            })
        }
    };

    let to_index = parse_expr(parser)?;

    parser.assert_token(Token::Keyword(KT::Do))?;

    let code = parse_block_or_statement(parser)?;

    Ok(Statement::For {
        var_name: var_name,
        from: start_index,
        to: to_index,
        is_to: dir,
        scope: Box::new(code),
    })
}

fn parse_while(parser: &mut Parser) -> Outcome<Statement> {
    parser.assert_token(Token::Keyword(KT::While))?;

    let condition = parse_expr(parser)?;

    parser.assert_token(Token::Keyword(KT::Do))?;

    let code = parse_block_or_statement(parser)?;

    Ok(Statement::While {
        cond: condition,
        scope: Box::new(code),
    })
}

fn parse_if(parser: &mut Parser) -> Outcome<Statement> {
    parser.assert_token(Token::Keyword(KT::If))?;

    let condition = parse_expr(parser)?;

    parser.assert_token(Token::Keyword(KT::Then))?;
    let true_branch = parse_block_or_statement(parser)?;

    if Token::Keyword(KT::Else) != parser.peek()?.token {
        return Ok(Statement::If {
            cond: condition,
            true_branch: Box::new(true_branch),
        });
    };
    parser.consume()?;
    let false_branch = parse_block_or_statement(parser)?;

    Ok(Statement::IfElse {
        cond: condition,
        true_branch: Box::new(true_branch),
        false_branch: Box::new(false_branch),
    })
}

fn parse_expr(parser: &mut Parser) -> Outcome<Expr> {
    parse_op9(parser)

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

// or
fn parse_op9(parser: &mut Parser) -> Outcome<Expr> {
    let mut lhs = parse_op8(parser)?;

    while Token::Operator(OT::Or) == parser.peek()?.token {
        parser.consume()?;
        let rhs = parse_op8(parser)?;
        lhs = Expr::Or(Box::new(lhs), Box::new(rhs));
    }
    Ok(lhs)
}

// and
fn parse_op8(parser: &mut Parser) -> Outcome<Expr> {
    let mut lhs = parse_op7(parser)?;

    while Token::Operator(OT::And) == parser.peek()?.token {
        parser.consume()?;
        let rhs = parse_op7(parser)?;
        lhs = Expr::And(Box::new(lhs), Box::new(rhs));
    }
    Ok(lhs)
}

// xor
fn parse_op7(parser: &mut Parser) -> Outcome<Expr> {
    let mut lhs = parse_op6(parser)?;

    while Token::Operator(OT::Xor) == parser.peek()?.token {
        parser.consume()?;
        let rhs = parse_op6(parser)?;
        lhs = Expr::Xor(Box::new(lhs), Box::new(rhs));
    }
    Ok(lhs)
}

// eq, ne
fn parse_op6(parser: &mut Parser) -> Outcome<Expr> {
    let mut lhs = parse_op5(parser)?;

    loop {
        let peeked = parser.peek()?.token;
        if Token::Operator(OT::Eq) == peeked {
            parser.consume()?;
            let rhs = parse_op5(parser)?;
            lhs = Expr::Eq(Box::new(lhs), Box::new(rhs));
        } else if Token::Operator(OT::Ne) == peeked {
            parser.consume()?;
            let rhs = parse_op5(parser)?;
            lhs = Expr::Ne(Box::new(lhs), Box::new(rhs));
        } else {
            break;
        }
    }
    Ok(lhs)
}

// gt, ge, lt, le
fn parse_op5(parser: &mut Parser) -> Outcome<Expr> {
    let mut lhs = parse_op4(parser)?;

    loop {
        let peeked = parser.peek()?.token;
        if Token::Operator(OT::Gt) == peeked {
            parser.consume()?;
            let rhs = parse_op4(parser)?;
            lhs = Expr::Gt(Box::new(lhs), Box::new(rhs));
        } else if Token::Operator(OT::Ge) == peeked {
            parser.consume()?;
            let rhs = parse_op4(parser)?;
            lhs = Expr::Ge(Box::new(lhs), Box::new(rhs));
        } else if Token::Operator(OT::Lt) == peeked {
            parser.consume()?;
            let rhs = parse_op4(parser)?;
            lhs = Expr::Lt(Box::new(lhs), Box::new(rhs));
        } else if Token::Operator(OT::Le) == peeked {
            parser.consume()?;
            let rhs = parse_op4(parser)?;
            lhs = Expr::Le(Box::new(lhs), Box::new(rhs));
        } else {
            break;
        }
    }
    Ok(lhs)
}

// mul, div, mod
fn parse_op4(parser: &mut Parser) -> Outcome<Expr> {
    let mut lhs = parse_op3(parser)?;

    loop {
        let peeked = parser.peek()?.token;
        if Token::Operator(OT::Plus) == peeked {
            parser.consume()?;
            let rhs = parse_op3(parser)?;
            lhs = Expr::Add(Box::new(lhs), Box::new(rhs));
        } else if Token::Operator(OT::Minus) == peeked {
            parser.consume()?;
            let rhs = parse_op3(parser)?;
            lhs = Expr::Sub(Box::new(lhs), Box::new(rhs));
        } else {
            break;
        }
    }
    Ok(lhs)
}

// add, sub
fn parse_op3(parser: &mut Parser) -> Outcome<Expr> {
    let mut lhs = parse_op2(parser)?;

    loop {
        let peeked = parser.peek()?.token;
        if Token::Operator(OT::Mul) == peeked {
            parser.consume()?;
            let rhs = parse_op2(parser)?;
            lhs = Expr::Mul(Box::new(lhs), Box::new(rhs));
        } else if Token::Operator(OT::Div) == peeked {
            parser.consume()?;
            let rhs = parse_op2(parser)?;
            lhs = Expr::Div(Box::new(lhs), Box::new(rhs));
        } else if Token::Operator(OT::Mod) == peeked {
            parser.consume()?;
            let rhs = parse_op2(parser)?;
            lhs = Expr::Mod(Box::new(lhs), Box::new(rhs));
        } else {
            break;
        }
    }
    Ok(lhs)
}

// unary plus, minus
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
    if Token::Operator(OT::Not) == peeked {
        parser.consume()?;
        let rhs = parse_op2(parser)?;
        Expr::Not(Box::new(rhs))
    } else 
    if Token::Operator(OT::CastToInt) == peeked {
        parser.consume()?;
        let rhs = parse_op2(parser)?;
        Expr::CastToInt(Box::new(rhs))
    } else 
    if Token::Operator(OT::CastToFloat) == peeked {
        parser.consume()?;
        let rhs = parse_op2(parser)?;
        Expr::CastToFloat(Box::new(rhs))
    } else 
    { parse_op1(parser)? }
)
}

// brackets, values
fn parse_op1(parser: &mut Parser) -> Outcome<Expr> {
    let peeked = parser.peek()?.token;

    if Token::Bracket(BI {
        sq: false,
        op: true,
    }) == peeked
    {
        parser.consume()?;
        let expr = parse_expr(parser)?;
        parser.assert_token(Token::Bracket(BI {
            sq: false,
            op: false,
        }))?;
        return Ok(expr);
    };

    if let Token::Identifier(_) = peeked {
        return parse_mem_access(parser);
    };
    if let Token::BuiltIn(_) = peeked {
        return parse_built_in(parser);
    };

    Ok(Expr::Literal(parse_literal(parser)?))
}

fn parse_mem_access(parser: &mut Parser) -> Outcome<Expr> {
    let token_into = parser.consume()?;
    let name = if let Token::Identifier(name) = token_into.token {
        name
    } else {
        return Err(MilaErr::InvalidToken {
            msg: String::from("Name expected"),
            act: token_into,
        });
    };

    // yes, this is lame, I won't be able to call invoke
    // on functions stored in an array or returned by other functions

    let mut data_source = if let Token::Bracket(BI {
        sq: false,
        op: true,
    }) = parser.peek()?.token
    {
        let args = parse_fun_args(parser)?;
        Expr::FunCall {
            name: name,
            args: args,
        }
    } else {
        Expr::VarAccess(name)
    };

    while let Token::Bracket(BI { sq: true, op: true }) = parser.peek()?.token {
        let index = parse_array_brackets(parser)?;
        data_source = Expr::ArrayAccess(Box::new(data_source), Box::new(index))
    }

    Ok(data_source)
}

// yeah, code duplication, I don't care for these
// badly specified functions
fn parse_built_in(parser: &mut Parser) -> Outcome<Expr> {
    let token_into = parser.consume()?;
    let name = if let Token::BuiltIn(name) = token_into.token {
        name
    } else {
        return Err(MilaErr::InvalidToken {
            msg: String::from("Built in expected"),
            act: token_into,
        });
    };

    // yes, this is lame, I won't be able to call invoke
    // on functions stored in an array or returned by other functions

    let peeked = parser.peek()?;
    let mut data_source = if let Token::Bracket(BI {
        sq: false,
        op: true,
    }) = peeked.token
    {
        let args = parse_fun_args(parser)?;
        Expr::BuiltIn {
            kind: name,
            args: args,
        }
    } else {
        return Err(MilaErr::InvalidToken {
            msg: String::from("Built in args expected"),
            act: peeked,
        });
    };

    while let Token::Bracket(BI { sq: true, op: true }) = parser.peek()?.token {
        let index = parse_array_brackets(parser)?;
        data_source = Expr::ArrayAccess(Box::new(data_source), Box::new(index))
    }

    Ok(data_source)
}

fn parse_fun_args(parser: &mut Parser) -> Outcome<Vec<Expr>> {
    let mut args: Vec<Expr> = vec![];

    parser.assert_token(Token::Bracket(BI {
        sq: false,
        op: true,
    }))?;

    let mut is_first = true;
    loop {
        let peeked = parser.peek()?;
        if Token::Bracket(BI {
            sq: false,
            op: false,
        }) == peeked.token
        {
            parser.consume()?;
            break;
        };

        if !is_first {
            parser.assert_token(Token::Operator(OT::Comma))?;
        } else {
            is_first = false;
        }

        let expr = parse_expr(parser)?;
        args.push(expr);
    }

    Ok(args)
}

fn parse_array_brackets(parser: &mut Parser) -> Outcome<Expr> {
    parser.assert_token(Token::Bracket(BI { sq: true, op: true }))?;

    let expr = parse_expr(parser)?;

    parser.assert_token(Token::Bracket(BI {
        sq: true,
        op: false,
    }))?;

    Ok(expr)
}
