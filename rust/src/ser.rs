use std::{collections::HashMap, io::Write};
use crate::{Error, Result};
use serde::ser::Serialize;
use log::debug;

pub fn to_writer<W: Write, S: Serialize>(writer: W, value: &S) -> Result<()> {
    let mut ser = Serializer::new(writer);
    value.serialize(&mut ser)
}

pub fn to_string<S: Serialize>(value: &S) -> Result<String> {
    // Create a buffer and serialize our nodes into it
    let mut writer = Vec::with_capacity(128);
    to_writer(&mut writer, value)?;

    // We then check that the serialized string is the same as what we expect
    let string = String::from_utf8(writer)?;
    Ok(string)
}

pub struct Serializer<W>
where
  W: Write,
{
  root: bool,
  cursor: usize
}

impl<W> Serializer<W>
where
  W: Write,
{
//  fn new(writer: W) -> Self {
//  }
  // TODO
}

impl<'ser, W: Write> serde::ser::Serializer for &'ser mut Serializer<W> {
  type Ok = ();
  type Error = Error;
  // TODO
}
