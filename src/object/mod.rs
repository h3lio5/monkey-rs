use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::{ast::BlockStatement, token::Token};
use environment::Environment;

mod builtins;
pub(super) mod environment;
mod errors;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Int(i64),
    String(String),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Func(FuncObject),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FuncObject {
    pub(crate) parameters: Vec<Token>,
    pub(crate) body: BlockStatement,
    pub(crate) env: Rc<RefCell<Environment>>,
}

impl Eq for Object {}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Object::Int(ref i) => i.hash(state),
            Object::Boolean(ref b) => b.hash(state),
            Object::String(ref s) => s.hash(state),
            _ => "".hash(state),
        }
    }
}
