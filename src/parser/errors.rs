use thiserror::Error;

use crate::token::Token;

#[derive(Debug, Error, PartialEq)]
pub enum ParseError {
    #[error("parsing error")]
    ParseError,
    #[error("mismatched tokens (expected {expected:?}, found {found:?})")]
    MismatchedToken { expected: Token, found: Token },
    #[error("No prefix parse function defined for the token: {token:?}")]
    NoPrefixParseFunction { token: Token },
    #[error("Error parsing the expression: {expression:?}")]
    ParseExpressionError { expression: String },
}
