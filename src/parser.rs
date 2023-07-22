#![allow(dead_code)]
use crate::{
    ast::{Expression, Program, Statement},
    lexer::Lexer,
    token::Token,
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Precedence {
    Lowest,
    Equals,
    Comparison,
    Sum,
    Product,
    Prefix,
    Call,
}

pub struct Parser {
    lex: Lexer,
    curr_token: Token,
    peek_token: Token,

    errors: Vec<ParserError>,
}

#[derive(Debug)]
pub enum ParserError {
    ExpectedAssign(Token),
    ExpectedIdent(Token),
    ExpectedInt(Token),
    ExpectedPrefixToken(Token),
    ParseInt(String),
}

type Result<T> = std::result::Result<T, ParserError>;

type PrefixParseFn = fn(&mut Parser) -> Result<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Result<Expression>;

impl Parser {
    pub fn new(lex: Lexer) -> Self {
        let mut p = Parser {
            lex,
            curr_token: Token::Illegal,
            peek_token: Token::Illegal,
            errors: vec![],
        };

        // Eg: let x = 5;
        // Calling twice because initially currToken = Illegal, nextToken = let.
        p.next_token();
        p.next_token();
        p
    }

    pub fn input(&self) -> String {
        self.lex.input()
    }

    pub fn errors(&self) -> &[ParserError] {
        &self.errors
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
                    self.errors.push(err);
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
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let exp = self.parse_expression(Precedence::Lowest);

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        exp.map(Statement::Expression)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        let prefix = self
            .prefix_parse_fn()
            .ok_or_else(|| ParserError::ExpectedPrefixToken(self.curr_token.clone()))?;

        let left_exp = prefix(self);

        left_exp
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

    fn prefix_parse_fn(&self) -> Option<PrefixParseFn> {
        match &self.curr_token {
            Token::Identifier(_) => Some(Parser::parse_ident),
            Token::Int(_) => Some(Parser::parse_integer_literal),
            _ => None,
        }
    }

    fn parse_ident(&mut self) -> Result<Expression> {
        if let Token::Identifier(ident) = &self.curr_token {
            Ok(Expression::Identifier(ident.to_string()))
        } else {
            Err(ParserError::ExpectedIdent(self.curr_token.clone()))
        }
    }

    fn parse_integer_literal(&mut self) -> Result<Expression> {
        if let Token::Int(integer) = &self.curr_token {
            match integer.parse::<i64>() {
                Ok(val) => Ok(Expression::IntegerLiteral(val)),
                Err(_) => Err(ParserError::ParseInt(integer.to_string())),
            }
        } else {
            Err(ParserError::ExpectedInt(self.curr_token.clone()))
        }
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
        check_parser_errors(&parser);

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
        check_parser_errors(&parser);

        assert_eq!(
            program.statements,
            vec![
                Statement::Return(None),
                Statement::Return(None),
                Statement::Return(None),
            ]
        );
    }

    #[test]
    fn identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements,
            vec![Statement::Expression(Expression::Identifier(
                "foobar".to_string()
            ))]
        )
    }

    #[test]
    fn integer_expression() {
        let input = "5;";

        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements,
            vec![Statement::Expression(Expression::IntegerLiteral(5))]
        )
    }

    fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors();

        if errors.len() > 0 {
            panic!("Input: {}, Errors: {:?}", parser.input(), errors)
        }
    }
}
