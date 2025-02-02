use crate::object::Object;

use super::opcode::OpCode;
/// Custom error type for evaluation errors
use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum VmError {
    #[error("Stackoverflow!")]
    StackOverflowError,
    #[error("error decoding opcode {opcode:?}")]
    OpcodeDecodingError { opcode: OpCode },
    #[error("Stack is empty!")]
    StackEmptyError,
    #[error("Incorrect Type")]
    TypeError,
    #[error("Division by 0 error!")]
    DivByZeroError,
    #[error("Unsupported binary operation")]
    UnsupportedBinaryOperation,
}

pub(super) type VmResult<T> = Result<T, VmError>;
