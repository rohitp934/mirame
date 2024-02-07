// src/object.rs
#![allow(dead_code)]

pub mod env;

use crate::ast::{Infix, Prefix};
use core::fmt;

pub type EvalResult = Result<Object, EvalError>;

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i64),
    Bool(bool),
    Null,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Bool(b) => write!(f, "{}", b),
            Object::Null => write!(f, "null"),
        }
    }
}

impl Object {
    pub fn obj_type(&self) -> &str {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::Bool(_) => "BOOL",
            Object::Null => "NULL",
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Bool(val) => *val,
            Object::Null => false,
            _ => true,
        }
    }
}

#[derive(Debug)]
pub enum EvalError {
    TypeMismatch(Object, Infix, Object),
    UnknownPrefixOperator(Prefix, Object),
    UnknownInfixOperator(Object, Infix, Object),
    IdentifierNotFound(String),
    NotCallable(String),
    WrongArgCount { expected: usize, got: usize },
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::TypeMismatch(left, infix, right) => write!(
                f,
                "type mismatch: {} {} {}",
                left.obj_type(),
                infix,
                right.obj_type()
            ),
            EvalError::UnknownPrefixOperator(prefix, right) => {
                write!(f, "unknown operator: {}{}", prefix, right.obj_type())
            }
            EvalError::UnknownInfixOperator(left, infix, right) => {
                write!(
                    f,
                    "unknown operator: {} {} {}",
                    left.obj_type(),
                    infix,
                    right.obj_type()
                )
            }
            EvalError::IdentifierNotFound(ident) => write!(f, "identifier not found: {}", ident),
            EvalError::NotCallable(function) => {
                write!(f, "not a function or a closure: {}", function)
            }
            EvalError::WrongArgCount { expected, got } => write!(
                f,
                "wrong number of args: expected {}, got {}",
                expected, got
            ),
        }
    }
}

pub fn assert_arg_count(expected: usize, args: &[Object]) -> Result<(), EvalError> {
    if args.len() != expected {
        return Err(EvalError::WrongArgCount {
            expected,
            got: args.len(),
        });
    }
    Ok(())
}
