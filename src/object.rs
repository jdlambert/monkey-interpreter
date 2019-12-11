use crate::{ast::BlockStatement, builtins::BuiltInFn, environment::Environment, eval};
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
    Hash(HashMap<HashKey, Box<Object>>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Null => write!(f, "{}", "null"),
            Object::Integer(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::String(value) => write!(f, "\"{}\"", value),
            Object::Function(args, body, _) => write!(f, "fn({}) {}", args.join(", "), body),
            Object::BuiltIn(builtin) => write!(f, "{:?}", builtin),
            Object::Array(members) => {
                let members = members
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<std::string::String>>()
                    .join(", ");
                write!(f, "[{}]", members)
            },
            Object::Hash(pairs) => {
                let pairs = pairs
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<std::string::String>>()
                    .join(", ");
                write!(f, "[{}]", pairs)
            },
        }
    }
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match *self {
            Object::Null | Object::Boolean(false) => false,
            _ => true,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum HashKey {
    Integer(i64),
    String(String),
    Boolean(bool),
}

impl fmt::Display for HashKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HashKey::Integer(value) => write!(f, "{}", value),
            HashKey::String(value) => write!(f, "\"{}\"", value),
            HashKey::Boolean(value) => write!(f, "{}", value),
        }
    }
}

impl HashKey {
    pub fn from_object(obj: Object) -> Result<HashKey, eval::EvalError> {
        match obj {
            Object::Integer(value) => Ok(HashKey::Integer(value)),
            Object::String(value) => Ok(HashKey::String(value.to_string())),
            Object::Boolean(value) => Ok(HashKey::Boolean(value)),
            _ => Err(eval::EvalError::CannotBeHashed(obj.clone())),
        }
    }
}