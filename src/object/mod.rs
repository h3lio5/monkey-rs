use std::cell::RefCell;
use std::rc::Rc;

use crate::{ast::BlockStatement, token::Token};
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
pub(crate) struct FuncObject {
    pub(crate) parameters: Vec<Token>,
    pub(crate) body: BlockStatement,
    pub(crate) env: Rc<RefCell<Environment>>,
}
