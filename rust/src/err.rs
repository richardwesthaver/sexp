//! err.rs --- SEXP Errors
use serde::de::Error as DeError;
use serde::ser::Error as SerError;
use std::fmt::Display;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
  #[error("expected token: {token}, found: {found})")]
  UnexpectedToken { token: String, found: String },
  #[error("custom: {field}")]
  Custom { field: String },
  #[error("unsupported operation: '{operation}'")]
  UnsupportedOperation { operation: String },
  #[error("IO error: {source}")]
  Io {
    #[from]
    source: ::std::io::Error,
  },
  #[error("FromUtf8Error: {source}")]
  FromUtf8Error {
    #[from]
    source: ::std::string::FromUtf8Error,
  },
  #[error("ParseIntError: {source}")]
  ParseIntError {
    #[from]
    source: ::std::num::ParseIntError,
  },
  #[error("ParseFloatError: {source}")]
  ParseFloatError {
    #[from]
    source: ::std::num::ParseFloatError,
  },
  #[error("ParseBoolError: {source}")]
  ParseBoolError {
    #[from]
    source: ::std::str::ParseBoolError,
  },
}

pub type Result<T> = std::result::Result<T, Error>;

impl DeError for Error {
  fn custom<T: Display>(msg: T) -> Self {
    Error::Custom {
      field: msg.to_string(),
    }
  }
}

impl SerError for Error {
  fn custom<T: Display>(msg: T) -> Self {
    Error::Custom {
      field: msg.to_string(),
    }
  }
}
