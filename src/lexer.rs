use core::panic;
use std::{iter::Peekable, vec::IntoIter};

use either::Either::{self, Left, Right};

use crate::{
    base::Outcome,
    errors::MilaErr,
    tokens::{BracketInfo, BuiltInType, KeywordType, OperatorType, Token, TokenInfo},
};

pub trait Lexer {
    fn next_token(&mut self) -> Outcome<TokenInfo>;
}

pub type LexerItr = Peekable<IntoIter<char>>;

struct LexerImpl {
    itr: LexerItr,
    line: u32,
    col: u32,
}

impl dyn Lexer {
    pub fn factory(iter: LexerItr) -> Outcome<Box<dyn Lexer>> {
        Ok(Box::new(LexerImpl::new(iter)?))
    }
}

enum NumBase {
    Bin,
    Oct,
    Dec,
    Hex,
}
impl NumBase {
    fn is_valid(&self, c: &char) -> bool {
        match self {
            Self::Bin => ('0'..='1').contains(c),
            Self::Oct => ('0'..='7').contains(c),
            Self::Dec => ('0'..='9').contains(c),
            Self::Hex => match c {
                '0'..='9' | 'a'..='f' | 'A'..='F' => true,
                _ => false,
            },
        }
    }
    fn add_digit_to_num(&self, num: u64, c: char) -> u64 {
        match self {
            Self::Bin => num * 2 + (c as u8 - b'0') as u64,
            Self::Oct => num * 8 + (c as u8 - b'0') as u64,
            Self::Dec => num * 10 + (c as u8 - b'0') as u64,
            Self::Hex => match c {
                '0'..='9' => num * 16 + (c as u8 - b'0') as u64,
                'a'..='f' => num * 16 + (c as u8 - b'a') as u64,
                'A'..='F' => num * 16 + (c as u8 - b'A') as u64,
                _ => panic!("Non-valid char passed"),
            },
        }
    }
    fn radix(&self) -> u8 {
        match self {
            Self::Bin => 2,
            Self::Oct => 8,
            Self::Dec => 10,
            Self::Hex => 16,
        }
    }
}

impl LexerImpl {
    fn new(iter: LexerItr) -> Outcome<Self> {
        Ok(Self {
            itr: iter,
            line: 1,
            col: 0,
        })
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
    fn peek(&mut self) -> Option<char> {
        self.itr.peek().map(|x| *x)
    }
    fn progress(&mut self) -> Option<()> {
        self.next().map(|_| ())
    }

    fn err_eof(&self) -> MilaErr {
        MilaErr::EOFReached {
            line: self.line,
            col: self.col,
        }
    }
    fn err_char(&self, c: char) -> MilaErr {
        MilaErr::UnexpectedChar {
            c: c,
            line: self.line,
            col: self.col,
        }
    }

    fn match_keyword(keyword: String) -> Token {
        match keyword.as_str() {
            "begin" => Token::Keyword(KeywordType::Begin),
            "end" => Token::Keyword(KeywordType::End),
            "const" => Token::Keyword(KeywordType::Const),
            "procedure" => Token::Keyword(KeywordType::Procedure),
            "forward" => Token::Keyword(KeywordType::Forward),
            "function" => Token::Keyword(KeywordType::Function),
            "if" => Token::Keyword(KeywordType::If),
            "then" => Token::Keyword(KeywordType::Then),
            "else" => Token::Keyword(KeywordType::Else),
            "program" => Token::Keyword(KeywordType::Program),
            "while" => Token::Keyword(KeywordType::While),
            "exit" => Token::Keyword(KeywordType::Exit),
            "var" => Token::Keyword(KeywordType::Var),
            "integer" => Token::Keyword(KeywordType::Integer),
            "float" => Token::Keyword(KeywordType::Float),
            "for" => Token::Keyword(KeywordType::For),
            "do" => Token::Keyword(KeywordType::Do),
            "array" => Token::Keyword(KeywordType::Array),
            "of" => Token::Keyword(KeywordType::Of),

            "and" => Token::Operator(OperatorType::And),
            "or" => Token::Operator(OperatorType::Or),
            "downto" => Token::Operator(OperatorType::Downto),
            "to" => Token::Operator(OperatorType::To),
            "mod" => Token::Operator(OperatorType::Mod),
            "div" => Token::Operator(OperatorType::Div),

            "dec" => Token::BuiltIn(BuiltInType::Dec),
            "inc" => Token::BuiltIn(BuiltInType::Inc),
            "write" => Token::BuiltIn(BuiltInType::Write),
            "writeln" => Token::BuiltIn(BuiltInType::WriteLine),
            "readln" => Token::BuiltIn(BuiltInType::ReadLine),
            "print" => Token::BuiltIn(BuiltInType::Print),
            "println" => Token::BuiltIn(BuiltInType::PrintLine),

            _ => Token::Identifier(keyword),
        }
    }

    fn process_identifier(&mut self) -> Outcome<Option<Token>> {
        match self.peek() {
            Some(next) => next,
            None => return Ok(Some(Token::EOF)),
        };

        let mut is_first = true;
        let mut buff = String::new();
        loop {
            let next = match self.peek() {
                Some(next) => next,
                None => break,
            };

            if next.is_alphabetic() || (!is_first && (next.is_ascii_digit() || next == '_')) {
                buff.push(next);
                self.progress();
            } else {
                break;
            };

            is_first = false;
        }

        if buff.len() == 0 {
            return Ok(None);
        }

        buff = buff.to_lowercase();

        Ok(Some(Self::match_keyword(buff)))
    }

    fn read_digit_sequence(&mut self, base: &NumBase) -> Outcome<(u64, u16)> {
        let mut acu = 0u64;
        let mut len = 0u16;
        let mut any_read = false;

        loop {
            let next_opt = self.peek();
            let next = match next_opt {
                Some(s) => s,
                None => {
                    if !any_read {
                        return Err(MilaErr::EOFReached {
                            line: self.line,
                            col: self.col,
                        });
                    } else {
                        break;
                    }
                }
            };

            if base.is_valid(&next) {
                acu = base.add_digit_to_num(acu, next);
                len += 1;
                any_read = true;
            } else {
                if !any_read {
                    return Err(MilaErr::UnexpectedNumberEnd {
                        c: next,
                        line: self.line,
                        col: self.col,
                    });
                } else {
                    break;
                }
            }
            self.progress();
        }
        return Ok((acu, len));
    }

    fn process_digits_in_base(&mut self, base: NumBase) -> Outcome<Either<u64, f64>> {
        let main = self.read_digit_sequence(&base)?;

        match self.peek() {
            Some('.') => (),
            _ => return Ok(Left(main.0)),
        }
        self.progress();

        let dec = self.read_digit_sequence(&base)?;
        let powed = (base.radix() as f64).powi(dec.1.into());
        let res = main.0 as f64 + dec.0 as f64 / powed;

        return Ok(Right(res));
    }

    fn process_number(&mut self) -> Outcome<Option<Token>> {
        let next_opt = self.peek();
        let next = match next_opt {
            Some(next) => next,
            None => return Ok(Some(Token::EOF)),
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
            '0'..='9' => self.process_digits_in_base(NumBase::Dec),
            _ => return Ok(None),
        }?;
        Ok(Some(match res {
            Left(v) => Token::Integer(v),
            Right(v) => Token::Float(v),
        }))
    }

    fn process_string(&mut self) -> Outcome<Option<Token>> {
        let next_opt = self.peek();
        let next = match next_opt {
            Some(next) => next,
            None => return Ok(Some(Token::EOF)),
        };

        let start = if next == '\'' || next == '\"' {
            next.clone()
        } else {
            return Ok(None);
        };
        self.next();

        let mut is_escaped = false;
        let mut buffer = String::new();

        loop {
            let next_opt = self.next();
            let next = match next_opt {
                Some(next) => next,
                None => return Err(MilaErr::UnclosedString),
            };

            if is_escaped {
                is_escaped = false;
                match next {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\\' => '\\',
                    '"' => '\"',
                    '\'' => '\'',
                    _ => return Err(MilaErr::UnknownEscapeSequence(next)),
                };
            } else {
                match next {
                    '\\' => {
                        is_escaped = true;
                        continue;
                    }
                    next => {
                        if next == start {
                            return Ok(Some(Token::String(buffer)));
                        }
                        buffer.push(next)
                    },
                }
            }
        }
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
                let sec_opt = self.peek().ok_or_else(|| self.err_eof())?;
                match sec_opt {
                    '=' => {
                        self.progress();
                        Token::Operator(OperatorType::Assign)
                    }
                    _ => Token::Operator(OperatorType::Colon),
                }
            }
            '=' => {
                self.progress();
                Token::Operator(OperatorType::Eq)
            }
            '!' => {
                self.progress();
                let sec = self.peek().ok_or_else(|| self.err_eof())?;
                match sec {
                    '=' => {
                        self.progress();
                        Token::Operator(OperatorType::Ne)
                    }
                    _ => return Err(self.err_char(sec)),
                }
            }
            '>' => {
                self.progress();
                let sec = self.peek().ok_or_else(|| self.err_eof())?;
                match sec {
                    '=' => {
                        self.progress();
                        Token::Operator(OperatorType::Ge)
                    }
                    _ => Token::Operator(OperatorType::Gt),
                }
            }
            '<' => {
                self.progress();
                let sec = self.peek().ok_or_else(|| self.err_eof())?;
                match sec {
                    '=' => {
                        self.progress();
                        Token::Operator(OperatorType::Le)
                    }
                    '>' => {
                        self.progress();
                        Token::Operator(OperatorType::Ne)
                    }
                    _ => Token::Operator(OperatorType::Lt),
                }
            }
            '+' => {
                self.progress();
                Token::Operator(OperatorType::Plus)
            }
            '-' => {
                self.progress();
                Token::Operator(OperatorType::Minus)
            }
            '*' => {
                self.progress();
                Token::Operator(OperatorType::Mul)
            }
            '/' => {
                self.progress();
                Token::Operator(OperatorType::Div)
            }
            ',' => {
                self.progress();
                Token::Operator(OperatorType::Comma)
            }
            ';' => {
                self.progress();
                Token::Operator(OperatorType::Semicolon)
            }
            '.' => {
                self.progress();
                let sec = self.peek().ok_or_else(|| self.err_eof())?;
                match sec {
                    '.' => {
                        self.progress();
                        Token::Operator(OperatorType::Ranges)
                    }
                    _ => Token::Operator(OperatorType::Dot),
                }
            }
            '(' => {
                self.progress();
                Token::Bracket(BracketInfo {
                    sq: false,
                    op: true,
                })
            }
            ')' => {
                self.progress();
                Token::Bracket(BracketInfo {
                    sq: false,
                    op: false,
                })
            }
            '[' => {
                self.progress();
                Token::Bracket(BracketInfo { sq: true, op: true })
            }
            ']' => {
                self.progress();
                Token::Bracket(BracketInfo {
                    sq: true,
                    op: false,
                })
            }
            _ => return Ok(None),
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
                        break;
                    }
                }
                None => break,
            }
        }

        let line_col = (self.line, self.col);

        // dint't manage to write it using or_else
        let res_token_opt = match self.process_operator_or_bracket()? {
            Some(token) => Some(token),
            None => match self.process_number()? {
                Some(token) => Some(token),
                None => match self.process_identifier()? {
                    Some(token) => Some(token),
                    None => self.process_string()?,
                },
            },
        };
        let res_token = match res_token_opt {
            Some(token) => token,
            None => {
                if self.peek() == None {
                    Token::EOF
                } else {
                    return Err(MilaErr::NoTokenMatched {
                        line: self.line,
                        col: self.col,
                    });
                }
            }
        };

        let info = TokenInfo {
            token: res_token,
            line: line_col.0,
            column: line_col.1,
        };
        eprintln!("{:?}", info);
        Ok(info)
    }
}
