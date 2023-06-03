use std::{vec, iter::Peekable};

use crate::{ast::{Program, Statement}, lexer::Lexer, base::Outcome, tokens::{TokenInfo, Token, KeywordType, OperatorType}, errors::MilaErr};

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
        self.parser_program()
    }

    fn parser_program(&mut self) -> Outcome<Program> {
        self.assert_token(Token::Keyword(KeywordType::Program))?;

        let name = {
            let name_token_info = self.consume()?;
            let name_token = name_token_info.token;
            if let Token::Identifier(name) = name_token {
                name
            } else {
                return Err(MilaErr::MissingProgramName)
            }
        };
        
        self.assert_token(Token::Operator(OperatorType::Semicolon))?;

        Ok(Program {
            name: name,
            vars: vec![],
            constants: vec![],
            declarations: vec![],
            functions: vec![],
            main: Statement::Block { statements: vec![] },
        })
    }

    #[must_use]
    fn assert_token(&mut self, token: Token) -> Outcome<()> {
        let actual = self.peek()?;
        if token == actual.token {
            self.progress();
            Ok(())
        } else {
            Err(MilaErr::UnexpectedToken { expected: token, actual: actual })
        }
    }

    // next token
    fn consume(&mut self) -> Outcome<TokenInfo> {
        self.iter.next().unwrap()
    }
    fn progress(&mut self) {
        self.iter.next();
    }
    fn peek(&mut self) -> Outcome<TokenInfo> {
        self.iter.peek().unwrap().clone() // this hurst, but haven't found a better solution
    }
}
