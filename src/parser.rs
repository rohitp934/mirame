use crate::{ast::Program, lexer::Lexer, token::Token};

struct Parser {
    lex: Lexer,
    curr_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(lex: Lexer) -> Self {
        let mut p = Parser {
            lex,
            curr_token: Token::Illegal,
            peek_token: Token::Illegal,
        };

        // Eg: let x = 5;
        // Calling twice because initially currToken = Illegal, nextToken = let.
        p.next_token();
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.curr_token = std::mem::replace(&mut self.peek_token, self.lex.next_token());
    }

    pub fn parse_program() -> Program {
        unimplemented!();
    }
}
