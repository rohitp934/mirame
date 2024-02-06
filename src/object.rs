// src/object.rs
#![allow(dead_code)]

use core::fmt;

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
}
