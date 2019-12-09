use crate::{
    eval::{self, EvalError},
    object::Object,
};
use std::{cmp, fmt};

pub struct BuiltInFn {
    pub name: &'static str,
    pub func: fn(&Vec<Object>) -> eval::Result,
}

impl fmt::Debug for BuiltInFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<builtin: {} >", self.name)
    }
}

impl cmp::PartialEq for BuiltInFn {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

macro_rules! builtin {
    ($name:ident) => {
        BuiltInFn {
            name: stringify!($name),
            func: $name,
        }
    };
}

const BUILTINS: &[BuiltInFn] = &[builtin!(len)];

pub fn get(name: &str) -> Option<Object> {
    match BUILTINS.iter().find(|builtin| name == builtin.name) {
        Some(builtin) => Some(Object::BuiltIn(&builtin)),
        None => None,
    }
}

fn check_arguments_len(arguments: &Vec<Object>, expected: usize) -> Result<(), EvalError> {
    let actual = arguments.len();
    if actual == expected {
        Ok(())
    } else {
        Err(EvalError::WrongNumberOfArgs { actual, expected })
    }
}

fn len(arguments: &Vec<Object>) -> eval::Result {
    check_arguments_len(arguments, 1)?;
    if let Some(obj) = arguments.first() {
        if let Object::String(s) = obj {
            Ok(Object::Integer(s.len() as i64))
        } else {
            Err(EvalError::InvalidArgument {
                to: "len".to_string(),
                arg: obj.clone(),
            })
        }
    } else {
        Err(EvalError::Unimplemented)
    }
}
