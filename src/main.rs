mod lexer;
mod repl;
mod token;
use std::io::{self, Write};

use crate::repl::PROMPT;

fn main() {
    let stdin = std::io::stdin();
    println!("Hello {}! This is the Mirame REPL!\n", whoami::username());
    println!("You can get started by typing some commands.\n");
    print!("{} ", PROMPT);
    let _ = io::stdout().flush();
    repl::start(stdin);
}
