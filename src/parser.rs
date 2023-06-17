use std::{vec, iter::Peekable};

use either::Either;

use crate::{ast::{Program, Statement, Scope, Value, Constant, Declaration, Function, Variable, Type}, lexer::Lexer, base::Outcome, tokens::{TokenInfo, Token, KeywordType, OperatorType, KT, OT, BI}, errors::MilaErr};

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
            parser.consume();
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
    ;todo!()
}
