// src/token/mod.rs
pub type TokenType = String;

pub struct Token {
    pub _type: TokenType,
    pub literal: String,
}

// Terminal tokens
pub const ILLEGAL: &str = "ILLEGAL";
pub const EOF: &str = "EOF";

// Identifiers + literals
pub const IDENTIFIER: &str = "IDENTIFIER"; // add, foobar, x, y, ...
pub const INT: &str = "INT"; // 1343456

// Keywords
pub const FUNCTION: &str = "FUNCTION"; // fn
pub const LET: &str = "LET"; // let
pub const RETURN: &str = "RETURN"; // return

// Operators
pub const ASSIGN: &str = "ASSIGN"; // =
pub const PLUS: &str = "PLUS"; // +
pub const MINUS: &str = "MINUS"; // -

// Delimiters
pub const LPAREN: &str = "LPAREN"; // (
pub const RPAREN: &str = "RPAREN"; //
pub const LBRACE: &str = "LBRACE"; // {
pub const RBRACE: &str = "RBRACE"; // }
pub const COMMA: &str = "COMMA"; // ,
pub const SEMICOLON: &str = "SEMICOLON"; // ;
