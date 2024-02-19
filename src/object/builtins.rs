use std::collections::HashMap;

use crate::object::{assert_arg_count, EvalError, EvalResult, Object};

pub struct Builtin {
    pub name: &'static str,
    pub builtin: Object,
}

macro_rules! builtin {
    ($name:ident) => {
        Builtin {
            name: stringify!($name),
            builtin: Object::Builtin($name),
        }
    };
}

fn builtins() -> HashMap<&'static str, Builtin> {
    HashMap::from([
        ("len", builtin!(len)),
        ("first", builtin!(first)),
        ("last", builtin!(last)),
        ("tail", builtin!(tail)),
        ("push", builtin!(push)),
        ("print", builtin!(print)),
    ])
}

pub fn lookup(name: &str) -> Option<Object> {
    builtins().get(name).map(|b| b.builtin.clone())
}

pub fn len(args: Vec<Object>) -> EvalResult {
    assert_arg_count(1, &args)?;
    match &args[0] {
        Object::String(val) => Ok(Object::Integer(val.len() as i64)),
        Object::Array(arr) => Ok(Object::Integer(arr.len() as i64)),
        _ => Err(EvalError::UnsupportedArgs("len".to_string(), args)),
    }
}

pub fn first(args: Vec<Object>) -> EvalResult {
    assert_arg_count(1, &args)?;
    match &args[0] {
        Object::Array(arr) => Ok(match arr.first() {
            None => Object::Null,
            Some(val) => val.clone(),
        }),
        _ => Err(EvalError::UnsupportedArgs("first".to_string(), args)),
    }
}

pub fn last(args: Vec<Object>) -> EvalResult {
    assert_arg_count(1, &args)?;
    match &args[0] {
        Object::Array(arr) => Ok(match arr.last() {
            None => Object::Null,
            Some(first) => first.clone(),
        }),
        _ => Err(EvalError::UnsupportedArgs("last".to_string(), args)),
    }
}

pub fn tail(args: Vec<Object>) -> EvalResult {
    assert_arg_count(1, &args)?;
    match &args[0] {
        Object::Array(arr) => {
            if arr.is_empty() {
                return Ok(Object::Null);
            }
            Ok(Object::Array(arr[1..].to_vec()))
        }
        _ => Err(EvalError::UnsupportedArgs("tail".to_string(), args)),
    }
}

pub fn push(args: Vec<Object>) -> EvalResult {
    assert_arg_count(2, &args)?;
    match &args[0] {
        Object::Array(arr) => {
            let mut new_arr = arr.clone();
            new_arr.push(args[1].clone());
            Ok(Object::Array(new_arr))
        }
        _ => Err(EvalError::UnsupportedArgs("push".to_string(), args)),
    }
}

pub fn print(args: Vec<Object>) -> EvalResult {
    for arg in args {
        println!("{}", arg);
    }

    Ok(Object::Null)
}
