// src/lexer/mod.rs

use crate::token;

#[derive(Debug)]
pub struct Lexer {
    pub input: String,
    pub position: usize,
    pub next_position: usize,
    pub ch: char,
}

impl Lexer {
    fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            next_position: 0,
            ch: '\0',
        };

        l.read_char();
        return l;
    }

    fn read_char(&mut self) {
        if self.next_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.next_position).unwrap();
        }
        self.position = self.next_position;
        self.next_position += 1;
    }

    fn next_token(&mut self) -> token::Token {
        let tok: token::Token;

        match self.ch {
            '=' => tok = new_token(token::ASSIGN, self.ch),
            '+' => tok = new_token(token::PLUS, self.ch),
            '-' => tok = new_token(token::MINUS, self.ch),
            ',' => tok = new_token(token::COMMA, self.ch),
            ';' => tok = new_token(token::SEMICOLON, self.ch),
            '(' => tok = new_token(token::LPAREN, self.ch),
            ')' => tok = new_token(token::RPAREN, self.ch),
            '{' => tok = new_token(token::LBRACE, self.ch),
            '}' => tok = new_token(token::RBRACE, self.ch),
            '\0' => tok = new_token(token::EOF, self.ch),
            _ => tok = new_token(token::ILLEGAL, self.ch),
        }

        self.read_char();
        tok
    }
}

fn new_token(tok: token::TokenType, ch: char) -> token::Token {
    token::Token {
        _type: tok,
        literal: ch.to_string(),
    }
}

#[cfg(test)]
mod lexer_test {
    use crate::token::{self, Token};

    use super::*;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";

        let tests = [
            Token {
                _type: token::ASSIGN,
                literal: "=".to_string(),
            },
            Token {
                _type: token::PLUS,
                literal: "+".to_string(),
            },
            Token {
                _type: token::LPAREN,
                literal: "(".to_string(),
            },
            Token {
                _type: token::RPAREN,
                literal: ")".to_string(),
            },
            Token {
                _type: token::LBRACE,
                literal: "{".to_string(),
            },
            Token {
                _type: token::RBRACE,
                literal: "}".to_string(),
            },
            Token {
                _type: token::COMMA,
                literal: ",".to_string(),
            },
            Token {
                _type: token::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                _type: token::EOF,
                literal: "\0".to_string(),
            },
        ];

        let mut l = Lexer::new(input.to_string());

        for (i, tt) in tests.iter().enumerate() {
            let tok = l.next_token();

            assert_eq!(
                tok._type, tt._type,
                "Test[{}]: Failed, tokentype wrong. Expected: {}, Received: {}",
                i, tt._type, tok._type
            );

            assert_eq!(
                tok.literal, tt.literal,
                "Test[{}]: Failed, literal wrong. Expected: {}, Received: {}",
                i, tt.literal, tok.literal
            );
        }
    }
}
