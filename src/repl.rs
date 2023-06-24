use crate::{lexer::Lexer, token::Token};
use std::io::{self, Write};

pub const PROMPT: &str = "> ";

pub fn start(istream: std::io::Stdin) {
    istream.lines().for_each(|line| {
        if let Ok(line) = line {
            let mut lex = Lexer::new(line);
            let mut token = lex.next_token();
            while token != Token::Eof {
                println!("{}", token);
                token = lex.next_token();
            }
            print!("{} ", PROMPT);
            let _ = io::stdout().flush();
        }
    })
}
