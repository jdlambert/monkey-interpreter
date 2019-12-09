use crate::{ast::BlockStatement, environment::Environment};
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    String(std::string::String),
    Function(Vec<std::string::String>, BlockStatement, Environment),
}

use Object::*;

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Null => write!(f, "{}", "null"),
            Integer(value) => write!(f, "{}", value),
            Boolean(value) => write!(f, "{}", value),
            String(value) => write!(f, "{}", value),
            Function(args, body, _) => write!(f, "fn({}) {}", args.join(", "), body),
        }
    }
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        *self != Object::Null && *self != Object::Boolean(false)
    }
}
