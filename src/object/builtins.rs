use std::collections::HashMap;

use crate::object::{assert_arg_count, EvalError, EvalResult, Object};

#[derive(Clone)]
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
    HashMap::from([("len", builtin!(len))])
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
