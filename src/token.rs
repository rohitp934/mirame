// src/token/mod.rs

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    Int(String),
    Illegal,
    Eof,
    // Keywords
    Function,
    Let,
    Return,
    True,
    False,
    If,
    Else,
    // Operators
    Assign,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Bang,
    Lt,
    Gt,
    Eq,
    Neq,
    // Delimiters
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Comma,
    Semicolon,
}
