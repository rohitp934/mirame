// src/lexer/mod.rs
#![allow(dead_code)]

use crate::token::Token;

#[derive(Debug)]
pub struct Lexer {
    pub input: Vec<u8>,
    pub position: usize,
    pub next_position: usize,
    pub ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input: input.into_bytes(),
            position: 0,
            next_position: 0,
            ch: 0,
        };

        l.read_char();
        l
    }

    pub fn input(&self) -> String {
        String::from_utf8(self.input.clone()).expect("Error")
    }

    fn read_char(&mut self) {
        if self.next_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.next_position];
        }
        self.position = self.next_position;
        self.next_position += 1;
    }

    fn peek_char(&self) -> u8 {
        if self.next_position >= self.input.len() {
            return 0;
        }
        self.input[self.next_position]
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            b'+' => {
                if self.peek_char() == b'+' {
                    self.read_char();
                    Token::Inc
                } else {
                    Token::Plus
                }
            }
            b'-' => {
                if self.peek_char() == b'-' {
                    self.read_char();
                    Token::Dec
                } else {
                    Token::Minus
                }
            }
            b'*' => Token::Asterisk,
            b'/' => Token::Slash,
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::Neq
                } else {
                    Token::Bang
                }
            }
            b'<' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::Leq
                } else {
                    Token::Lt
                }
            }
            b'>' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::Geq
                } else {
                    Token::Gt
                }
            }
            b',' => Token::Comma,
            b';' => Token::Semicolon,
            b':' => Token::Colon,
            b'(' => Token::Lparen,
            b')' => Token::Rparen,
            b'{' => Token::Lbrace,
            b'}' => Token::Rbrace,
            b'[' => Token::Lbracket,
            b']' => Token::Rbracket,
            b'"' => Token::String(self.read_string()),
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_identifier();
                return match ident.as_str() {
                    "fn" => Token::Function,
                    "let" => Token::Let,
                    "return" => Token::Return,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "true" => Token::True,
                    "false" => Token::False,
                    _ => Token::Identifier(ident),
                };
            }
            0 => Token::Eof,
            _ => {
                if is_digit(self.ch) {
                    let integer_part = self.read_number().to_string();
                    if self.ch == b'.' {
                        self.read_char();
                        if is_digit(self.ch) {
                            return Token::Float(format!(
                                "{}.{}",
                                integer_part,
                                self.read_number()
                            ));
                        }
                        return Token::Float(format!("{}.0", integer_part));
                    }
                    return Token::Int(integer_part);
                } else {
                    Token::Illegal
                }
            }
        };

        self.read_char();
        tok
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.position;
        while self.ch.is_ascii_alphabetic() || self.ch == b'_' {
            self.read_char();
        }
        // Return string from pos to self.position
        String::from_utf8_lossy(&self.input[pos..self.position]).to_string()
    }

    fn read_string(&mut self) -> String {
        let pos = self.position + 1;
        loop {
            self.read_char();
            if self.ch == b'"' {
                break;
            }
        }

        String::from_utf8_lossy(&self.input[pos..self.position]).to_string()
    }

    fn read_number(&mut self) -> String {
        let pos = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }
        // Return string from pos to self.position
        String::from_utf8_lossy(&self.input[pos..self.position]).to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }
}

fn is_digit(ch: u8) -> bool {
    ch.is_ascii_digit()
}

#[cfg(test)]
mod lexer_test {
    use super::*;
    use crate::token::Token;

    #[test]
    fn test_next_token() -> anyhow::Result<()> {
        let input = r#"let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;
        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        "foobar";
        "foo bar";
        [1, 2, 3];
        5.5;
        1.;
        {"foo": "bar"};
        "#;

        let mut lex = Lexer::new(input.into());

        let tokens = vec![
            Token::Let,
            Token::Identifier(String::from("five")),
            Token::Assign,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("ten")),
            Token::Assign,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Identifier(String::from("x")),
            Token::Comma,
            Token::Identifier(String::from("y")),
            Token::Rparen,
            Token::Lbrace,
            Token::Identifier(String::from("x")),
            Token::Plus,
            Token::Identifier(String::from("y")),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("result")),
            Token::Assign,
            Token::Identifier(String::from("add")),
            Token::Lparen,
            Token::Identifier(String::from("five")),
            Token::Comma,
            Token::Identifier(String::from("ten")),
            Token::Rparen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Int(String::from("5")),
            Token::Lt,
            Token::Int(String::from("10")),
            Token::Gt,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::If,
            Token::Lparen,
            Token::Int(String::from("5")),
            Token::Lt,
            Token::Int(String::from("10")),
            Token::Rparen,
            Token::Lbrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::Rbrace,
            Token::Int(String::from("10")),
            Token::Eq,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Int(String::from("10")),
            Token::Neq,
            Token::Int(String::from("9")),
            Token::Semicolon,
            Token::String(String::from("foobar")),
            Token::Semicolon,
            Token::String(String::from("foo bar")),
            Token::Semicolon,
            Token::Lbracket,
            Token::Int(String::from("1")),
            Token::Comma,
            Token::Int(String::from("2")),
            Token::Comma,
            Token::Int(String::from("3")),
            Token::Rbracket,
            Token::Semicolon,
            Token::Float(String::from("5.5")),
            Token::Semicolon,
            Token::Float(String::from("1.0")),
            Token::Semicolon,
            Token::Lbrace,
            Token::String(String::from("foo")),
            Token::Colon,
            Token::String(String::from("bar")),
            Token::Rbrace,
            Token::Semicolon,
            Token::Eof,
        ];

        for token in tokens {
            let next_token = lex.next_token();
            println!("expected: {:?}, received {:?}", token, next_token);
            assert_eq!(token, next_token);
        }

        Ok(())
    }
}
