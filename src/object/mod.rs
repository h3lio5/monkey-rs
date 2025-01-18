use std::cell::RefCell;
use std::rc::Rc;

use crate::{ast::BlockStatement, token::Token};
use environment::Environment;

pub(super) mod environment;
mod errors;
mod builtins;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Int(i64),
    String(String),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Func(FuncObject),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FuncObject {
    pub(crate) parameters: Vec<Token>,
    pub(crate) body: BlockStatement,
    pub(crate) env: Rc<RefCell<Environment>>,
}
