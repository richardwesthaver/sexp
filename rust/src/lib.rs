//! lib.rs --- S-Expression data format

//! S-Expressions (S-Expr, sexp) are a flexible notation for tree-like
//! data.

//! * example

//! ```rust
//! use serde_derive::{Deserialize, Serialize};
//! use sexp::to_string; // from_str
//!
//! #[derive(Debug, Serialize, Deserialize, PartialEq)]
//! struct Item {
//!     name: String,
//!     source: String,
//! }
//!
//! fn main() {
//!     let src = r#"(:name "Banana" :source "Store")"#;
//!     let should_be = Item {
//!         name: "Banana".to_string(),
//!         source: "Store".to_string(),
//!     };
//!
//!     // let item: Item = from_str(src).unwrap();
//!     // assert_eq!(item, should_be);
//!
//!     let reserialized_item = to_string(&should_be).unwrap();
//!     assert_eq!(src, reserialized_item);
//! }
//! ```

mod err;
pub use err::{Error, Result};

macro_rules! tri {
  ($e:expr $(,)?) => {
    match $e {
      core::result::Result::Ok(val) => val,
      core::result::Result::Err(err) => return core::result::Result::Err(err),
    }
  };
}

// pub mod de;
pub mod ast;
pub mod fmt;
pub mod macs;
pub mod ser;
pub mod tok;

// pub use crate::de::{from_reader, from_str, Deserializer};
pub use ast::Sexp;
pub use fmt::{CanonicalFormatter, Formatter};
pub use macs::{Macro, MacroObject, ReadTable, WriteTable};
pub use ser::{to_string, to_vec, to_writer, Serializer};
pub use tok::Token;
