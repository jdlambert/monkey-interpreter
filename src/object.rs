use crate::{ast::BlockStatement, builtins::BuiltInFn, environment::Environment};
use std::{fmt, collections::HashMap};

#[derive(Clone, Debug)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    String(std::string::String),
    Function(Vec<std::string::String>, BlockStatement, Environment),
    BuiltIn(&'static BuiltInFn),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>)
}

use Object::*;

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Null => write!(f, "{}", "null"),
            Integer(value) => write!(f, "{}", value),
            Boolean(value) => write!(f, "{}", value),
            String(value) => write!(f, "\"{}\"", value),
            Function(args, body, _) => write!(f, "fn({}) {}", args.join(", "), body),
            BuiltIn(builtin) => write!(f, "{:?}", builtin),
            Array(members) => {
                let members = members
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<std::string::String>>()
                    .join(", ");
                write!(f, "[{}]", members)
            }
        }
    }
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        *self != Object::Null && *self != Object::Boolean(false)
    }
}
