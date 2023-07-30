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
    Prefix(Prefix, Box<Expression>),
    Infix(Box<Expression>, Infix, Box<Expression>),
    //TODO: Need to fill out the expressions
    Exp,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(ident) => write!(f, "{}", ident),
            Expression::IntegerLiteral(val) => write!(f, "{}", val),
            Expression::Prefix(op, exp) => write!(f, "({}{})", op, exp),
            Expression::Infix(exp_a, op, exp_b) => write!(f, "({} {} {})", exp_a, op, exp_b),
            Expression::Exp => write!(f, ""),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Prefix {
    Bang,
    Minus,
    Inc,
    Dec,
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Prefix::Bang => write!(f, "!"),
            Prefix::Minus => write!(f, "-"),
            Prefix::Inc => write!(f, "++"),
            Prefix::Dec => write!(f, "--"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Infix {
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    Plus,
    Minus,
    Asterisk,
    Slash,
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Infix::Eq => write!(f, "=="),
            Infix::Neq => write!(f, "!="),
            Infix::Lt => write!(f, "<"),
            Infix::Gt => write!(f, ">"),
            Infix::Leq => write!(f, "<="),
            Infix::Geq => write!(f, ">="),
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Asterisk => write!(f, "*"),
            Infix::Slash => write!(f, "/"),
        }
    }
}
