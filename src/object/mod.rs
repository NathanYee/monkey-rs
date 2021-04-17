pub mod builtin;
pub mod environment;

use crate::{
    ast::{csv_str, BlockStmt, Expr},
    evaluator::EvalError,
};
use environment::MutEnv;
use std::{
    collections::HashMap,
    fmt::Display,
    hash::{Hash, Hasher},
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    Str(String),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
    Null,
    Return(Box<Object>),
    Function(Vec<Expr>, BlockStmt, MutEnv),
    Builtin(String, fn(Vec<Object>) -> Result<Object, EvalError>),
}

pub const NULL: Object = Object::Null;
pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Str(s) => write!(f, "{}", s),
            Object::Array(a) => write!(f, "[{}]", csv_str(a)),
            Object::Hash(map) => {
                let pairs: Vec<String> = map.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
                let pairs = csv_str(&pairs);
                write!(f, "{{{}}}", pairs)
            }
            Object::Null => write!(f, "null"),
            Object::Return(object) => write!(f, "return {}", object),
            Object::Function(parameters, body, _) => {
                write!(f, "fn({}) {}", csv_str(parameters), body)
            }
            Object::Builtin(name, _) => write!(f, "{}", name),
        }
    }
}

impl Object {
    pub fn debug_type(&self) -> String {
        match self {
            Object::Integer(_) => "Integer",
            Object::Boolean(_) => "Boolean",
            Object::Str(_) => "String",
            Object::Array(_) => "Array",
            Object::Hash(_) => "Hash",
            Object::Null => "Null",
            Object::Return(_) => "Return",
            Object::Function(_, _, _) => "Function",
            Object::Builtin(_, _) => "Builtin",
        }
        .to_string()
    }

    pub fn get_bool(b: bool) -> Object {
        if b {
            TRUE
        } else {
            FALSE
        }
    }
}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::Integer(i) => i.hash(state),
            Object::Boolean(b) => b.hash(state),
            Object::Str(s) => s.hash(state),
            _ => 0.hash(state),
        }
    }
}
