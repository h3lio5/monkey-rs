use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::token::Token;

use super::{errors::EnvironmentErrors, Object};

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Option<Rc<Environment>>) -> Self {
        Self {
            store: HashMap::new(),
            outer,
        }
    }

    pub fn set(&mut self, key: &str, value: Object) {
        self.store.insert(key.to_owned(), value);
    }

    pub fn get(&self, key: &str) -> Result<Object, EnvironmentErrors> {
        // check the current environment
        if let Some(value) = self.store.get(key) {
            return Ok(value.clone());
        }

        // if not found, check the outer environment
        if let Some(outer) = &self.outer {
            return outer.get(key);
        }

        Err(EnvironmentErrors::KeyNotFound {
            key: key.to_owned(),
        })
    }
}
