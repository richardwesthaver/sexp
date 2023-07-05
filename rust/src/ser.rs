use std::{collections::HashMap, io::Write};
use core::fmt::{self, Display};
use crate::{Error, Result};
use serde::ser::Serialize;
use log::debug;

pub fn to_writer<W: Write, S: Serialize>(writer: W, value: &S) -> Result<()> {
    let mut ser = Serializer::new(writer);
    value.serialize(&mut ser)
}

pub fn to_string<S: Serialize>(value: &S) -> Result<String> {
    let mut writer = Vec::new(); // TODO: with-capacity 128? bench
    to_writer(&mut writer, value)?;
    // We then check that the serialized string is the same as what we expect
    let string = String::from_utf8(writer)?;
    Ok(string)
}

pub struct Serializer<W>
where
  W: Write,
{
  writer: W,
}

impl<W> Serializer<W>
where
  W: Write,
{
  fn new(writer: W) -> Self {
  }
}

impl<'ser, W: Write> serde::ser::Serializer for &'ser mut Serializer<W> {
  type Ok = (); // could be usize for bytes written
  type Error = Error;
  type SerializeSeq = Self;
  type SerializeTuple = Self;
  type SerializeTupleStruct = Self;
  type SerializeTupleVariant = Self;
  type SerializeMap = Self;
  type SerializeStruct = Self;
  type SerializeStructVariant = Self;
  // TODO
  fn serialize_bool(self, v: bool) -> Result<()> {
    self.writer.write(&[0]);
    Ok(())
  }
}
