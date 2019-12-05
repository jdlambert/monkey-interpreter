use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
  Null,
  Integer(i64),
  Boolean(bool),
}

use Object::*;

impl fmt::Display for Object {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
        Null  => write!(f, "{}", "null"),
        Integer(value) => write!(f, "{}", value),
        Boolean(value) => write!(f, "{}", value),
    } 
  }
}

