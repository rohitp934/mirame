#![allow(dead_code)]
use crate::{
    ast::{Expression, Infix, Prefix, Program, Statement},
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
    ExpectedBoolToken(Token),
    ExpectedPrefixToken(Token),
    ExpectedInfixToken(Token),
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

        let mut left_exp = prefix(self)?;

        while self.peek_token != Token::Semicolon
            && precedence < self.infix_token(&self.peek_token).0
        {
            if let Some(infix) = self.infix_parse_fn() {
                // curr_token = the infix operator
                self.next_token();
                left_exp = infix(self, left_exp)?;
                // curr_token = the last token of right_exp
            } else {
                return Ok(left_exp);
            }
        }

        Ok(left_exp)
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
            Token::True => Some(Parser::parse_boolean),
            Token::False => Some(Parser::parse_boolean),
            Token::Bang => Some(Parser::parse_prefix_exp),
            Token::Minus => Some(Parser::parse_prefix_exp),
            Token::Inc => Some(Parser::parse_prefix_exp),
            Token::Dec => Some(Parser::parse_prefix_exp),
            _ => None,
        }
    }

    fn infix_parse_fn(&self) -> Option<InfixParseFn> {
        match &self.peek_token {
            Token::Eq => Some(Parser::parse_infix_exp),
            Token::Neq => Some(Parser::parse_infix_exp),
            Token::Lt => Some(Parser::parse_infix_exp),
            Token::Gt => Some(Parser::parse_infix_exp),
            Token::Leq => Some(Parser::parse_infix_exp),
            Token::Geq => Some(Parser::parse_infix_exp),
            Token::Plus => Some(Parser::parse_infix_exp),
            Token::Minus => Some(Parser::parse_infix_exp),
            Token::Asterisk => Some(Parser::parse_infix_exp),
            Token::Slash => Some(Parser::parse_infix_exp),
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

    fn parse_boolean(&mut self) -> Result<Expression> {
        match &self.curr_token {
            Token::True => Ok(Expression::Bool(true)),
            Token::False => Ok(Expression::Bool(false)),
            _ => Err(ParserError::ExpectedBoolToken(self.curr_token.clone())),
        }
    }

    fn parse_prefix_exp(&mut self) -> Result<Expression> {
        let prefix = self.prefix_token(&self.curr_token)?;

        self.next_token();

        let right_exp = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::Prefix(prefix, Box::new(right_exp)))
    }

    fn parse_infix_exp(&mut self, left_exp: Expression) -> Result<Expression> {
        let (precedence, infix) = self.infix_token(&self.curr_token);

        let operator =
            infix.ok_or_else(|| ParserError::ExpectedInfixToken(self.curr_token.clone()))?;
        self.next_token();

        let right_exp = self.parse_expression(precedence)?;

        Ok(Expression::Infix(
            Box::new(left_exp),
            operator,
            Box::new(right_exp),
        ))
    }

    fn expect_peek(&mut self, t: Token, expected: fn(Token) -> ParserError) -> Result<()> {
        if self.peek_token != t {
            return Err(expected(self.peek_token.clone()));
        }
        self.next_token();
        Ok(())
    }

    fn prefix_token(&self, token: &Token) -> Result<Prefix> {
        match token {
            Token::Bang => Ok(Prefix::Bang),
            Token::Minus => Ok(Prefix::Minus),
            Token::Inc => Ok(Prefix::Inc),
            Token::Dec => Ok(Prefix::Dec),
            token => Err(ParserError::ExpectedPrefixToken(token.clone())),
        }
    }

    fn infix_token(&self, token: &Token) -> (Precedence, Option<Infix>) {
        match token {
            Token::Eq => (Precedence::Equals, Some(Infix::Eq)),
            Token::Neq => (Precedence::Equals, Some(Infix::Neq)),
            Token::Lt => (Precedence::Comparison, Some(Infix::Lt)),
            Token::Gt => (Precedence::Comparison, Some(Infix::Gt)),
            Token::Leq => (Precedence::Comparison, Some(Infix::Leq)),
            Token::Geq => (Precedence::Comparison, Some(Infix::Geq)),
            Token::Plus => (Precedence::Sum, Some(Infix::Plus)),
            Token::Minus => (Precedence::Sum, Some(Infix::Minus)),
            Token::Asterisk => (Precedence::Product, Some(Infix::Asterisk)),
            Token::Slash => (Precedence::Product, Some(Infix::Slash)),
            _ => (Precedence::Lowest, None),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Infix, Prefix, Statement};
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

    #[test]
    fn prefix_expression() {
        let tests = vec![
            ("!5;", Prefix::Bang, Expression::IntegerLiteral(5)),
            ("-5;", Prefix::Minus, Expression::IntegerLiteral(5)),
            ("++5;", Prefix::Inc, Expression::IntegerLiteral(5)),
            ("--5;", Prefix::Dec, Expression::IntegerLiteral(5)),
        ];

        for (input, op, exp) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(
                program.statements,
                vec![Statement::Expression(Expression::Prefix(op, Box::new(exp)))]
            )
        }
    }

    #[test]
    fn infix_expression_integer() {
        let tests = vec![
            ("5 + 5;", 5, Infix::Plus, 5),
            ("5 - 5;", 5, Infix::Minus, 5),
            ("5 * 5;", 5, Infix::Asterisk, 5),
            ("5 / 5;", 5, Infix::Slash, 5),
            ("5 > 5;", 5, Infix::Gt, 5),
            ("5 < 5;", 5, Infix::Lt, 5),
            ("5 == 5;", 5, Infix::Eq, 5),
            ("5 != 5;", 5, Infix::Neq, 5),
        ];
        for (input, left, operator, right) in tests {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(
                program.statements,
                vec![Statement::Expression(Expression::Infix(
                    Box::new(Expression::IntegerLiteral(left)),
                    operator,
                    Box::new(Expression::IntegerLiteral(right))
                ))]
            );
        }
    }

    #[test]
    fn infix_expression_bool() {
        let tests = vec![
            ("true == true", true, Infix::Eq, true),
            ("true != false", true, Infix::Neq, false),
            ("false == false", false, Infix::Eq, false),
        ];
        for (input, left, operator, right) in tests {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(
                program.statements,
                vec![Statement::Expression(Expression::Infix(
                    Box::new(Expression::Bool(left)),
                    operator,
                    Box::new(Expression::Bool(right))
                ))]
            );
        }
    }

    #[test]
    fn operator_precedence() {
        test_parsing(vec![
            ("-a * b", "((-a) * b);"),
            ("!-a", "(!(-a));"),
            ("a + b + c", "((a + b) + c);"),
            ("a + b - c", "((a + b) - c);"),
            ("a * b * c", "((a * b) * c);"),
            ("a * b / c", "((a * b) / c);"),
            ("a + b / c", "(a + (b / c));"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f);"),
            ("3 + 4; -5 * 5", "(3 + 4);((-5) * 5);"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4));"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4));"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
            ),
            ("true", "true;"),
            ("false", "false;"),
            ("3 > 5 == false", "((3 > 5) == false);"),
            ("3 < 5 == true", "((3 < 5) == true);"),
        ]);
    }

    fn test_parsing(tests: Vec<(&str, &str)>) {
        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.to_string(), expected);
        }
    }

    fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors();

        if !errors.is_empty() {
            panic!("Input: {}, Errors: {:?}", parser.input(), errors)
        }
    }
}
