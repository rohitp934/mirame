// src/ast.rs
#![allow(dead_code)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(String, Expression),
    Return(Option<Expression>),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(String),
    //TODO: Need to fill out the expressions
    Exp,
}
