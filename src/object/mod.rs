use std::rc::Rc;

use crate::ast::{Identifier, Statement};
use environment::Environment;

pub mod environment;
mod errors;

#[derive(Debug, Clone)]
pub enum Object {
    Int(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Func(FuncObject),
}

#[derive(Debug, Clone)]
pub struct FuncObject {
    parameters: Vec<Identifier>,
    body: Vec<Statement>,
    env: Rc<Environment>,
}
