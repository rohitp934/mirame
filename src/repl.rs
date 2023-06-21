use crate::{lexer::Lexer, token::Token};
use std::io::{self, Write};

pub const PROMPT: &str = "> ";

pub fn start(istream: std::io::Stdin) {
    istream.lines().for_each(|line| {
        if let Ok(line) = line {
            let mut lex = Lexer::new(line);

            while let Ok(token) = lex.next_token() {
                println!("{}", token);
                if let Token::Eof = token {
                    break;
                }
            }
            print!("{} ", PROMPT);
            let _ = io::stdout().flush();
        }
    })
}
