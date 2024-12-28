use thiserror::Error;

use crate::token::Token;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("parsing error")]
    ParseError,
    #[error("mismatched tokens (expected {expected:?}, found {found:?})")]
    MismatchedToken {
        expected: Token,
        found: Token
    },
}