use std::{env, fs::File, io::Read, path::Path, process};

use base::Outcome;
use errors::MilaErr;

use lexer::{Lexer, LexerItr};
use parser::Parser;



mod ast;
mod base;
mod errors;
mod lexer;
mod llvm;
mod parser;
mod tokens;

enum AppMode {
    StdIn,
    File(Vec<String>),
}

fn modify_buffer(buff: &mut String) {
    // added so that every operation does not have to check if the end is reached
    // and can freely ask for the next char
    buff.push(' ');
}

fn stdin_iter() -> Outcome<LexerItr> {
    let mut buff = String::new();

    std::io::stdin()
        .read_to_string(&mut buff)
        .map_err(|err| MilaErr::ReadStdInFailed(err.to_string()))?;

    modify_buffer(&mut buff);

    let itr = buff.chars().collect::<Vec<_>>().into_iter();
    let peekable = itr.peekable();
    Ok(peekable)
}

fn file_iter(names: Vec<String>) -> Outcome<LexerItr> {
    let mut buff = String::new();

    for name in names {
        let path = Path::new(&name);
        let display = path.display();

        let mut file = match File::open(&path) {
            Err(why) => panic!("couldn't open {}: {}", display, why),
            Ok(file) => file,
        };

        file.read_to_string(&mut buff)
            .map_err(|err| MilaErr::ReadFileFailed(err.to_string(), name))?;
    }

    modify_buffer(&mut buff);

    let itr = buff.chars().collect::<Vec<_>>().into_iter();
    let peekable = itr.peekable();
    Ok(peekable)
}

fn run_mila(mode: AppMode) -> Outcome<()> {
    let iter = match mode {
        AppMode::StdIn => stdin_iter(),
        AppMode::File(names) => file_iter(names),
    }?;
    let lexer = <dyn Lexer>::factory(iter)?;

    // TODO cli args

    // loop {
    //     let token = lexer.next_token()?;
    //     println!("{token}");
    //     if token.token == Token::EOF {
    //         break;
    //     };
    // };
    // return Ok(());

    let mut parser = Parser::factory(lexer);
    let ast = parser.parse_ast()?;
    // println!("{:?}", ast);
    // return Ok(());

    println!("{}", ast.compile()?);

    Ok(())
}

fn main() {
    eprintln!("Running");

    let args: Vec<String> = env::args().skip(1).take(1).collect();
    let mode = if args.len() > 0 {
        AppMode::File(args)
    } else {
        AppMode::StdIn
    };

    match run_mila(mode) {
        Err(e) => {
            eprintln!("Compilation failed: {e}");
            eprintln!("They got the Ring! Our age is over.");
            process::exit(42);
        }
        _ => {}
    }

    eprintln!("It's over Mr. Frodo. It's done!");
}
