
use core::panic;
use std::{io::Read, vec::IntoIter, iter::Peekable};

use either::Either::{self, Left, Right};

use crate::{tokens::{TokenInfo, Token, OperatorType, BracketInfo}, base::Outcome, errors::MilaErr};

pub trait Lexer {
    fn next_token(&mut self) -> Outcome<TokenInfo>;
}

struct LexerImpl {
    itr: Peekable<IntoIter<char>>,
    line: u32,
    col: u32,
}

enum NumBase {
    Bin, Oct, Dec, Hex,
}
impl NumBase {
    fn is_valid(&self, c: &char) -> bool {
        match self {
            Self::Bin => ('0'..='1').contains(c),
            Self::Oct => ('0'..='7').contains(c),
            Self::Dec => ('0'..='9').contains(c),
            Self::Hex => 
                match c {
                    '0'..='9' | 'a'..='f' | 'A'..='F' => true,
                    _ => false,
                }
        }
    }
    fn add_digit_to_num(&self, num: u64, c: char) -> u64 {
        match self {
            Self::Bin => num * 2  + (c as u8 - b'0') as u64,
            Self::Oct => num * 8  + (c as u8 - b'0') as u64,
            Self::Dec => num * 10 + (c as u8 - b'0') as u64,
            Self::Hex => match c {
                '0'..='9' => num * 16 + (c as u8 - b'0') as u64,
                'a'..='f' => num * 16 + (c as u8 - b'a') as u64,
                'A'..='F' => num * 16 + (c as u8 - b'A') as u64,
            },
        }
    }
    fn inc_base(&self) -> f64 {
        match self {
            Self::Bin => 2,
            Self::Oct => 8,
            Self::Dec => 10,
            Self::Hex => 16,
        }
    }
}

impl LexerImpl {

    fn new() -> Outcome<Self> {
        let mut buff = String::new();

        std::io::stdin()
        .read_to_string(&mut buff)
        .map_err(|err| {MilaErr::ReadStdInFailed(err)})?;

        let itr = buff.chars().collect::<Vec<_>>().into_iter();
        let peekable = itr.peekable();
        Ok(Self {itr: peekable, line: 1, col: 0})
    }

    fn next(&mut self) -> Option<char> {
        let res = self.itr.next();
        if let Some(next) = res {
            if next == '\n' {
                self.line += 1;
                self.col = 0;
            };
            self.col += 1;
        };
        res
    }
    fn peek(&mut self) -> Option<&char> {
        self.itr.peek()
    }
    fn progress(&mut self) -> Option<()> {
        self.next().map(|_| {()})
    }

    fn err_eof(&self) -> MilaErr {
        MilaErr::EOFReached { line: self.line, col: self.col}
    }
    fn err_char(&self, c: char) -> MilaErr {
        MilaErr::UnexpectedChar { c: c, line: self.line, col: self.col}
    }

    fn read_digit_sequence(&mut self, base: NumBase) -> Outcome<(u64, u16)> {
        let mut acu = 0u64;
        let mut len = 0u16;
        let mut any_read = false;

        loop {
            let next_opt = self.peek();
            let next = *match next_opt {
                Some(s) => s,
                None => 
                    if !any_read {
                        return Err(MilaErr::EOFReached { line: self.line, col: self.col });
                    } else { 
                        break 
                    },
            };

            if base.is_valid(&next) {
                acu = base.add_digit_to_num(acu, next);
                any_read = true;
            } else {
                if !any_read {
                    return Err(MilaErr::UnexpectedNumberEnd { c: next, line: self.line, col: self.col })
                }
            }
        };
        return Ok((acu, len));
    }

    fn process_digits_in_base(&mut self, base: NumBase) -> Outcome<Either<u64, f64>> {
        let main = self.read_digit_sequence(base)?;

        match self.peek() {
            Some('.') => (),
            _ => return Ok(Left(main.0)),
        }
        self.progress();

        let dec = self.read_digit_sequence(base)?;
        let powed = base.inc_base().powf(dec.1.into());
        let res = main.0 as f64 + dec.0 as f64 / powed;

        return Ok(Right(res));
    }

    fn process_number(&mut self) -> Outcome<Token> {
        
        let next_opt = self.peek();
        let next = match next_opt {
            Some(next) => next,
            None => return Ok(Token::EOF),
        };

        let res = match next {
            '%' => {
                self.progress();
                self.process_digits_in_base(NumBase::Bin)
            }
            '&' => {
                self.progress();
                self.process_digits_in_base(NumBase::Oct)
            }
            '$' => {
                self.progress();
                self.process_digits_in_base(NumBase::Hex)
            }
            '0'..='9' =>
                self.process_digits_in_base(NumBase::Dec)
        }?;
        Ok(match res {
            Left(v) => Token::Integer(v),
            Right(v) => Token::Float(v),
        })
    }

    fn process_operator_or_bracket(&mut self) -> Outcome<Option<Token>> {
        let next_opt = self.peek();
        let next = match next_opt {
            Some(next) => next,
            None => return Ok(Some(Token::EOF)),
        };
        Ok(Some(match next {
            ':' => {
                self.progress();
                let sec = self.peek().ok_or_else(|| self.err_eof())?;
                match sec {
                    '=' => {
                        self.progress();
                        Token::Operator(OperatorType::Assign)
                    } ,
                    _ => Token::Operator(OperatorType::Colon)
                }
            },
            '=' => {
                self.progress();
                Token::Operator(OperatorType::Eq)
            },
            '!' => {
                self.progress();
                let sec = self.peek().ok_or_else(|| self.err_eof())?;
                match sec {
                    '=' => {
                        self.progress();
                        Token::Operator(OperatorType::Ne)
                    },
                    _ => return Err(self.err_char(*sec)),

                }
            },
            '>' => {
                self.progress();
                let sec = self.next().ok_or_else(|| self.err_eof())?;
                match sec {
                    '=' => {
                        self.progress();
                        Token::Operator(OperatorType::Ge)
                    },
                    _ => Token::Operator(OperatorType::Gt)

                }
            },
            '<' => {
                self.progress();
                let sec = self.next().ok_or_else(|| self.err_eof())?;
                match sec {
                    '=' => {
                        self.progress();
                        Token::Operator(OperatorType::Le)
                    },
                    _ => Token::Operator(OperatorType::Lt)

                }
            },
            '+' => {
                self.progress();
                Token::Operator(OperatorType::Plus)
            },
            '-' => {
                self.progress();
                Token::Operator(OperatorType::Minus)
            },
            '*' => {
                self.progress();
                Token::Operator(OperatorType::Mult)
            },
            '/' => {
                self.progress();
                Token::Operator(OperatorType::Div)
            },
            ',' => {
                self.progress();
                Token::Operator(OperatorType::Comma)
            },
            ';' => {
                self.progress();
                Token::Operator(OperatorType::Semicolon)
            },
            '(' => {
                self.progress();
                Token::Bracket(BracketInfo{is_square: false, is_open: true})
            },
            ')' => {
                self.progress();
                Token::Bracket(BracketInfo{is_square: false, is_open: false})
            },
            '[' => {
                self.progress();
                Token::Bracket(BracketInfo{is_square: true, is_open: true})
            },
            ']' => {
                self.progress();
                Token::Bracket(BracketInfo{is_square: true, is_open: false})
            },
        }))
    }
}

impl Lexer for LexerImpl {

    fn next_token(&mut self) -> Outcome<TokenInfo> {
        
        // skip spaces
        loop {
            let next = self.peek();
            match next {
                Some(next) => {
                    if next.is_whitespace() {
                        self.progress();
                        continue;
                    } else {
                        break
                    }
                }
                None => break
            }
        };

        match self.peek() {
            None => {Ok(TokenInfo{
                token: Token::EOF,
                line: self.line,
                column: self.col,
            })}
            
            Some(read) => {
                // operator
                // number
                // text
                ; panic!("")
            }
        }
    }
}
