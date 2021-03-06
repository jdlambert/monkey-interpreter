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
        write!(f, "{}", self.name)
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

const BUILTINS: &[BuiltInFn] = &[
    builtin!(len),
    builtin!(first),
    builtin!(last),
    builtin!(rest),
    builtin!(push),
    builtin!(puts),
];

pub fn get(name: &str) -> Option<Object> {
    match BUILTINS.iter().find(|builtin| name == builtin.name) {
        Some(builtin) => Some(Object::BuiltIn(&builtin)),
        None => None,
    }
}

fn len(arguments: &Vec<Object>) -> eval::Result {
    eval::check_arguments_len(arguments, 1)?;
    match arguments.first() {
        Some(Object::String(s)) => Ok(Object::Integer(s.len() as i64)),
        Some(Object::Array(a)) => Ok(Object::Integer(a.len() as i64)),
        _ => Err(EvalError::InvalidArgument {
            to: "len".to_string(),
            arg: if let Some(obj) = arguments.first() {
                obj.clone()
            } else {
                Object::Null
            },
        }),
    }
}

fn first(arguments: &Vec<Object>) -> eval::Result {
    eval::check_arguments_len(arguments, 1)?;
    match arguments.first() {
        Some(Object::Array(a)) => Ok(a[0].clone()),
        _ => Err(EvalError::InvalidArgument {
            to: "first".to_string(),
            arg: if let Some(obj) = arguments.first() {
                obj.clone()
            } else {
                Object::Null
            },
        }),
    }
}

fn last(arguments: &Vec<Object>) -> eval::Result {
    eval::check_arguments_len(arguments, 1)?;
    match arguments.first() {
        Some(Object::Array(a)) => Ok(a.last().unwrap().clone()),
        _ => Err(EvalError::InvalidArgument {
            to: "first".to_string(),
            arg: if let Some(obj) = arguments.first() {
                obj.clone()
            } else {
                Object::Null
            },
        }),
    }
}

fn rest(arguments: &Vec<Object>) -> eval::Result {
    eval::check_arguments_len(arguments, 1)?;
    match arguments.first() {
        Some(Object::Array(a)) => {
            let mut rest = a.clone();
            rest.remove(0);
            Ok(Object::Array(rest))
        }
        _ => Err(EvalError::InvalidArgument {
            to: "first".to_string(),
            arg: if let Some(obj) = arguments.first() {
                obj.clone()
            } else {
                Object::Null
            },
        }),
    }
}

fn push(arguments: &Vec<Object>) -> eval::Result {
    eval::check_arguments_len(arguments, 2)?;
    match arguments.first() {
        Some(Object::Array(a)) => {
            let mut new = a.clone();
            new.push(arguments[1].clone());
            Ok(Object::Array(new))
        }
        _ => Err(EvalError::InvalidArgument {
            to: "first".to_string(),
            arg: if let Some(obj) = arguments.first() {
                obj.clone()
            } else {
                Object::Null
            },
        }),
    }
}

fn puts(arguments: &Vec<Object>) -> eval::Result {
    eval::check_arguments_len(arguments, 1)?;
    println!("{}", arguments.first().unwrap());
    Ok(Object::Null)
}