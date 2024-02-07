use mirame::{eval::eval, lexer::Lexer, object::env::Env, parser::Parser};
use std::{
    cell::RefCell,
    io::{self, Write},
    rc::Rc,
};

pub const PROMPT: &str = "> ";

pub fn start(istream: std::io::Stdin) {
    let env = Rc::new(RefCell::new(Env::new()));

    istream.lines().for_each(|line| {
        if let Ok(line) = line {
            let mut parser = Parser::new(Lexer::new(line));

            let program = parser.parse_program();
            if !parser.errors().is_empty() {
                println!(" parser errors:");
                for msg in parser.errors() {
                    println!("\t{:?}", msg);
                }
                return;
            }

            match eval(&program, Rc::clone(&env)) {
                Ok(obj) => {
                    println!("{}", obj);
                }
                Err(e) => {
                    println!("Error: {}", e);
                }
            }
            print!("{} ", PROMPT);
            let _ = io::stdout().flush();
        }
    })
}
