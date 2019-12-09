use crate::object::Object;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Environment {
    store: Rc<RefCell<HashMap<String, Object>>>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        let store = self.store.borrow();
        match store.get(name) {
            Some(value) => Some(value.clone()),
            None => {
                if let Some(outer) = &self.outer {
                    outer.get(name)
                } else {
                    None
                }
            },
        }
    }

    pub fn set(&self, name: &str, val: Object) {
        let mut store = self.store.borrow_mut();
        store.insert(name.to_string(), val);
    }

    pub fn extend(&self) -> Self {
        Environment {
            outer: Some(Box::new(self.clone())),
            ..Default::default()
        }
    }

}