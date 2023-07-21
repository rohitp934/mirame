#![allow(dead_code)]
use crate::{
    ast::{Expression, Program, Statement},
    lexer::Lexer,
    token::Token,
};

struct Parser {
    lex: Lexer,
    curr_token: Token,
    peek_token: Token,
}

#[derive(Debug)]
enum ParserError {
    ExpectedAssign(Token),
    ExpectedIdent(Token),
}

type Result<T> = std::result::Result<T, ParserError>;

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

    pub fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Statement> = vec![];

        while self.curr_token != Token::Eof {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    eprintln!("Error from parse_statement() :: {:?}", err)
                }
            }
            self.next_token();
        }

        Program { statements }
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.curr_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => unimplemented!(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
        // Current token :: let

        let name;
        if let Token::Identifier(ident) = self.peek_token.clone() {
            self.next_token();
            name = ident;
        } else {
            return Err(ParserError::ExpectedIdent(self.peek_token.clone()));
        }

        self.expect_peek(Token::Assign, ParserError::ExpectedAssign)
            .unwrap();
        // current token :: =
        //TODO: Skipping expression statements for now.
        while self.curr_token != Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Let(name, Expression::Exp))
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        // Current token :: return

        self.next_token();

        //TODO: Skipping expression until semicolon;
        while self.curr_token != Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Return(None))
    }

    fn expect_peek(&mut self, t: Token, expected: fn(Token) -> ParserError) -> Result<()> {
        if self.peek_token != t {
            return Err(expected(self.peek_token.clone()));
        }
        self.next_token();
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn let_statement() {
        let input = r#"
            let x = 5;
            let y = 1023;
            let foobar = x + y;
        "#;
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(
            program.statements,
            vec![
                Statement::Let("x".to_string(), Expression::Exp),
                Statement::Let("y".to_string(), Expression::Exp),
                Statement::Let("foobar".to_string(), Expression::Exp)
            ]
        );
    }

    #[test]
    fn return_statement() {
        let input = r#"
            return 10;
            return;
            return 10 + 10;
        "#;

        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(
            program.statements,
            vec![
                Statement::Return(None),
                Statement::Return(None),
                Statement::Return(None),
            ]
        );
    }
}
