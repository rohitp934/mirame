// src/token/mod.rs

use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
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
    Leq,
    Geq,
    Neq,
    Inc,
    Dec,
    // Delimiters
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Comma,
    Semicolon,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Identifier(x) => write!(f, "Ident({})", x),
            Token::Int(x) => write!(f, "Int({})", x),
            Token::Illegal => write!(f, "Illegal"),
            Token::Eof => write!(f, "Eof"),
            Token::Assign => write!(f, "Assign"),
            Token::Bang => write!(f, "Bang"),
            Token::Minus => write!(f, "Minus"),
            Token::Slash => write!(f, "Slash"),
            Token::Asterisk => write!(f, "Asterisk"),
            Token::Eq => write!(f, "Equal"),
            Token::Neq => write!(f, "NotEqual"),
            Token::Lt => write!(f, "LessThan"),
            Token::Gt => write!(f, "GreaterThan"),
            Token::Leq => write!(f, "LessThanEqual"),
            Token::Geq => write!(f, "GreaterThanEqual"),
            Token::Plus => write!(f, "Plus"),
            Token::Inc => write!(f, "Increment"),
            Token::Dec => write!(f, "Decrement"),
            Token::Comma => write!(f, "Comma"),
            Token::Semicolon => write!(f, "Semicolon"),
            Token::Lparen => write!(f, "Lparen"),
            Token::Rparen => write!(f, "Rparen"),
            Token::Lbrace => write!(f, "Lbrace"),
            Token::Rbrace => write!(f, "Rbrace"),
            Token::Function => write!(f, "Function"),
            Token::Let => write!(f, "Let"),
            Token::If => write!(f, "If"),
            Token::Else => write!(f, "Else"),
            Token::Return => write!(f, "Return"),
            Token::True => write!(f, "True"),
            Token::False => write!(f, "False"),
        }
    }
}
