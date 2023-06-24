// src/ast.rs

pub struct Program {
    statements: Vec<Statement>
}

pub enum Statement {
    Let(String, Expression)
}

pub enum Expression {
    Identifier(String)
}
