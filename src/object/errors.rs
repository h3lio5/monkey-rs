use thiserror::Error;

use crate::token::Token;

#[derive(Debug, Error)]
pub enum EnvironmentErrors {
    #[error("Key Not Found: {key:?}")]
    KeyNotFound { key: String },
}
