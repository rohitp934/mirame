// src/object.rs
#![allow(dead_code)]

pub mod env;

use crate::ast::{BlockStatement, Infix, Prefix};
use core::fmt;
use std::{cell::RefCell, rc::Rc};

use self::env::Env;

pub type EvalResult = Result<Object, EvalError>;

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i64),
    String(String),
    Bool(bool),
    Null,
    ReturnValue(Box<Object>),
    Function(Vec<String>, BlockStatement, Rc<RefCell<Env>>),
    Array(Vec<Object>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::String(s) => write!(f, "\"{}\"", s),
            Object::Bool(b) => write!(f, "{}", b),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(val) => write!(f, "{}", val),
            Object::Function(args, body, _) => {
                write!(f, "fn({}) {{\n{}\n}}", args.join(", "), body)
            }
            Object::Array(elements) => {
                let values = elements
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "[{}]", values)
            }
        }
    }
}

impl Object {
    pub fn obj_type(&self) -> &str {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::String(_) => "STRING",
            Object::Bool(_) => "BOOL",
            Object::Null => "NULL",
            Object::ReturnValue(_) => "RETURN_VALUE",
            Object::Function(_, _, _) => "FUNCTION",
            Object::Array(_) => "ARRAY",
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
    UnknownIndexOperator(Object, Object),
    IdentifierNotFound(String),
    NotCallable(Object),
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
            EvalError::UnknownIndexOperator(left, index) => write!(
                f,
                "unknown index operator: {}[{}]",
                left.obj_type(),
                index.obj_type()
            ),
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
