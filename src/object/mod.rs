use std::rc::Rc;

use environment::Environment;
use crate::ast::{Identifier, Statement};

pub mod environment;
mod errors;

#[derive(Debug, Clone)]
pub enum Object {
    Int(i64),
    Boolean(bool),
    Null,
    Error(String),
    Func(FuncObject)
}

#[derive(Debug, Clone)]
pub struct FuncObject {
    parameters: Vec<Identifier>,
    body: Vec<Statement>,
    env: Rc<Environment>
}