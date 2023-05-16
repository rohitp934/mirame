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

        self.skip_whitespace();

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
            _ => {
                if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    tok = token::Token {
                        _type: token::lookup_identifier(literal.as_str()),
                        literal,
                    };
                    return tok;
                } else if is_digit(self.ch) {
                    tok = token::Token {
                        _type: token::INT,
                        literal: self.read_number(),
                    };
                    return tok;
                } else {
                    tok = new_token(token::ILLEGAL, self.ch);
                }
            }
        }

        self.read_char();
        tok
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        // Return string from pos to self.position
        self.input[pos..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let pos = self.position;
        while is_digit(self.ch) {
            self.read_char();
        }
        // Return string from pos to self.position
        self.input[pos..self.position].to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\r' || self.ch == '\n' || self.ch == '\t' {
            self.read_char();
        }
    }
}

fn is_letter(ch: char) -> bool {
    if 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' {
        return true;
    }
    false
}

fn is_digit(ch: char) -> bool {
    if '0' <= ch && ch <= '9' {
        return true;
    }
    false
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
        let input = "let five = 5;

let ten = 10;

let add = fn(x, y) {
  return x + y;
};

let result = add(five, ten);";

        let tests = [
            Token {
                _type: token::LET,
                literal: "let".to_string(),
            },
            Token {
                _type: token::IDENTIFIER,
                literal: "five".to_string(),
            },
            Token {
                _type: token::ASSIGN,
                literal: "=".to_string(),
            },
            Token {
                _type: token::INT,
                literal: "5".to_string(),
            },
            Token {
                _type: token::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                _type: token::LET,
                literal: "let".to_string(),
            },
            Token {
                _type: token::IDENTIFIER,
                literal: "ten".to_string(),
            },
            Token {
                _type: token::ASSIGN,
                literal: "=".to_string(),
            },
            Token {
                _type: token::INT,
                literal: "10".to_string(),
            },
            Token {
                _type: token::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                _type: token::LET,
                literal: "let".to_string(),
            },
            Token {
                _type: token::IDENTIFIER,
                literal: "add".to_string(),
            },
            Token {
                _type: token::ASSIGN,
                literal: "=".to_string(),
            },
            Token {
                _type: token::FUNCTION,
                literal: "fn".to_string(),
            },
            Token {
                _type: token::LPAREN,
                literal: "(".to_string(),
            },
            Token {
                _type: token::IDENTIFIER,
                literal: "x".to_string(),
            },
            Token {
                _type: token::COMMA,
                literal: ",".to_string(),
            },
            Token {
                _type: token::IDENTIFIER,
                literal: "y".to_string(),
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
                _type: token::RETURN,
                literal: "return".to_string(),
            },
            Token {
                _type: token::IDENTIFIER,
                literal: "x".to_string(),
            },
            Token {
                _type: token::PLUS,
                literal: "+".to_string(),
            },
            Token {
                _type: token::IDENTIFIER,
                literal: "y".to_string(),
            },
            Token {
                _type: token::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                _type: token::RBRACE,
                literal: "}".to_string(),
            },
            Token {
                _type: token::SEMICOLON,
                literal: ";".to_string(),
            },
            Token {
                _type: token::LET,
                literal: "let".to_string(),
            },
            Token {
                _type: token::IDENTIFIER,
                literal: "result".to_string(),
            },
            Token {
                _type: token::ASSIGN,
                literal: "=".to_string(),
            },
            Token {
                _type: token::IDENTIFIER,
                literal: "add".to_string(),
            },
            Token {
                _type: token::LPAREN,
                literal: "(".to_string(),
            },
            Token {
                _type: token::IDENTIFIER,
                literal: "five".to_string(),
            },
            Token {
                _type: token::COMMA,
                literal: ",".to_string(),
            },
            Token {
                _type: token::IDENTIFIER,
                literal: "ten".to_string(),
            },
            Token {
                _type: token::RPAREN,
                literal: ")".to_string(),
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
