// src/lexer/mod.rs

pub struct Lexer {
    pub input: String,
    pub position: usize,
    pub next_position: usize,
    pub ch: char,
}
