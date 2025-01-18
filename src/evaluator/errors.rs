/// Custom error type for evaluation errors
#[derive(Debug, Clone, PartialEq)]
pub(super) enum EvalError {
    UnknownIdentifier(String),
    TypeMismatch(String),
    UnsupportedOperation(String),
    InvalidFunction,
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownIdentifier(id) => write!(f, "identifier not found: {}", id),
            Self::TypeMismatch(msg) => write!(f, "type mismatch: {}", msg),
            Self::UnsupportedOperation(msg) => write!(f, "unsupported operation: {}", msg),
            Self::InvalidFunction => write!(f, "not a function"),
        }
    }
}

pub(super) type EvalResult<T> = Result<T, EvalError>;
