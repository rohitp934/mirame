// src/token/mod.rs
type TokenType = String;

pub struct Token {
    pub _type: TokenType,
    pub literal: String,
}

// Terminal tokens
const ILLEGAL: String = String::from("ILLEGAL");
const EOF: String = String::from("EOF");

// Identifiers + literals
const IDENTIFIER: String = String::from("IDENTIFIER"); // add, foobar, x, y, ...
const INT: String = String::from("INT"); // 1343456

// Keywords
const FUNCTION: String = String::from("FUNCTION"); // fn
const LET: String = String::from("LET"); // let
const RETURN: String = String::from("RETURN"); // return

// Operators
const ASSIGN: String = String::from("ASSIGN"); // =
const PLUS: String = String::from("PLUS"); // +
const MINUS: String = String::from("MINUS"); // -

// Delimiters
const LPAREN: String = String::from("LPAREN"); // (
const RPAREN: String = String::from("RPAREN"); // )
const LBRACE: String = String::from("LBRACE"); // {
const RBRACE: String = String::from("RBRACE"); // }
const COMMA: String = String::from("COMMA"); // ,
const SEMICOLON: String = String::from("SEMICOLON"); // ;
