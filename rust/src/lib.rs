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

#[macro_export(local_inner_macros)]
macro_rules! sexp {
  ($($sexp:tt)+) => {
    _sexp!($($sexp)+)
  };
}

// may not be necessary
#[macro_export(local_inner_macros)]
#[doc(hidden)]
macro_rules! _sexp {
  (@array [$($elems:expr),*]) => {
    sexp_vec![$($elems),*]
  };
  // Next element is `nil`.
  (@array [$($elems:expr,)*] nil $($rest:tt)*) => {
    _sexp!(@array [$($elems,)* _sexp!(nil)] $($rest)*)
  };
  // Unexpected token after most recent element.
  (@array [$($elems:expr),*] $unexpected:tt $($rest:tt)*) => {
    sexp_unexpected!($unexpected)
  };

  //////////////////////////////////////////////////////////////////////////
  // TT muncher for parsing the inside of a object (:foo "bar"). Each entry is
  // inserted into the given map variable.
  //
  // Must be invoked as: _sexp!(@object $map () ($($tt)*) ($($tt)*))
  //
  // We require two copies of the input tokens so that we can match on one
  // copy and trigger errors on the other copy.
  //////////////////////////////////////////////////////////////////////////

  // Done.
  (@object $object:ident () () ()) => {};

  // Insert the current entry.
  (@object $object:ident ( [$($key:tt)+] ($value:expr) ) $($rest:tt)*) => {
    let _ = $object.insert(($($key)+).into(), $value);
    _sexp!(@object $object () ($($rest)*) ($($rest)*));
    };

    // Current entry followed by unexpected token.
    (@object $object:ident [$($key:tt)+] ($value:expr) $unexpected:tt $($rest:tt)*) => {
        sexp_unexpected!($unexpected);
    };

    // Insert the last entry
    (@object $object:ident [$($key:tt)+] ($value:expr)) => {
        let _ = $object.insert(($($key)+).into(), $value);
    };

    // Next value is `nil`.
    (@object $object:ident ($($key:tt)+) (nil $($rest:tt)*) $copy:tt) => {
        json_internal!(@object $object [$($key)+] (json_internal!(null)) $($rest)*);
    };

}
