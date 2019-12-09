use crate::object::Object;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Environment {
    store: Rc<RefCell<HashMap<String, Object>>>,
}

impl Environment {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        let store = self.store.borrow();
        match store.get(name) {
            Some(value) => Some(value.clone()),
            None => None,
        }
    }

    pub fn set(&self, name: &str, val: Object) {
        let mut store = self.store.borrow_mut();
        store.insert(name.to_string(), val);
    }
}
