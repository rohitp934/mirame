// src/ast.rs
#![allow(dead_code)]

use std::fmt;
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(String, Expression),
    Return(Option<Expression>),
    Expression(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(ident, val) => write!(f, "let {} = {};", ident, val),
            Statement::Return(None) => write!(f, "return;"),
            Statement::Return(Some(exp)) => write!(f, "return {};", exp),
            Statement::Expression(exp) => write!(f, "{};", exp),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i64),
    //TODO: Need to fill out the expressions
    Exp,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(ident) => write!(f, "{}", ident),
            Expression::IntegerLiteral(val) => write!(f, "{}", val),
            Expression::Exp => write!(f, ""),
        }
    }
}
