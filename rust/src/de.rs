//! de.rs --- SXP deserializer
use crate::read::{self, Fused, Reference};
use crate::{
  fmt::{DefaultFormatter, Formatter},
  Error, Result,
};
use alloc::string::String;
use alloc::vec::Vec;
use core::iter::FusedIterator;
use core::marker::PhantomData;
use core::result;
use core::str::FromStr;
use serde::de::{self, Deserialize, Expected, Unexpected};

pub use crate::read::{Read, SliceRead, StrRead};

#[cfg(feature = "std")]
pub use crate::read::IoRead;

pub struct Deserializer<R, F> {
  read: R,
  scratch: Vec<u8>,
  formatter: F,
  limit: Option<usize>,
}

impl<'de, R: Read<'de>, F: Formatter> Deserializer<R, F> {
  pub fn new(read: R, formatter: F) -> Self {
    Deserializer {
      read,
      scratch: Vec::new(),
      formatter,
      limit: None,
    }
  }
  /// signal the end of a SXP byte-stream (no more tokens)
  pub fn end(&mut self) -> Result<()> {
    //  TODO 2023-07-14
    Ok(())
  }

  fn peek(&mut self) -> Result<Option<u8>> {
    self.read.peek()
  }
  // peek_char_or_nil
  fn eat_char(&mut self) {
    self.read.discard();
  }

  fn next_char(&mut self) -> Result<Option<u8>> {
    self.read.next()
  }
  // peek_char_or_nil
  // error
  // peek_error
  /// Returns the first non-whitespace byte without consuming it, or `None` if
  /// EOF is encountered.
  fn parse_whitespace(&mut self) -> Result<Option<u8>> {
    loop {
      match tri!(self.peek()) {
        Some(b' ') | Some(b'\n') | Some(b'\t') | Some(b'\r') => {
          self.eat_char();
        }
        other => {
          return Ok(other);
        }
      }
    }
  }
}

// impl<'de, 'a, R: Read, F: Formatter> de::Deserializer<'de> for &'a mut
// Deserializer<'de, R, F> {   type Error = Error;
// }

// fn from_trait<'de, R, T>(read: R) -> Result<T>
// where
//     R: Read,
//     T: de::Deserialize<'de>,
// {
//     let mut de = Deserializer::new(read, DefaultFormatter);
//     let value = tri!(de::Deserialize::deserialize(&mut de));

//     // Make sure the whole stream has been consumed.
//     tri!(de.end());
//     Ok(value)
// }

// pub fn from_read<'de, R: Read, T: de::Deserialize<'de>>(read: R) -> Result<T>
// {   T::deserialize(&mut Deserializer::new(read, DefaultFormatter))
// }

// pub fn from_str<'de, T: ?Sized + de::Deserialize<'de>>(s: &str) -> Result<T>
// {   from_read(s.as_bytes())
// }

// pub fn from_slice<'de, T: Deserialize<'de>>(value: &[u8]) -> Result<T> {
//   from_trait()
// }
