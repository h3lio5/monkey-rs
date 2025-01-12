use thiserror::Error;

#[derive(Debug, Error)]
pub enum EnvironmentErrors {
    #[error("Key Not Found: {key:?}")]
    KeyNotFound {key: String}
}