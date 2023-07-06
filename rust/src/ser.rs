//! ser.rs --- SEXP Serializer
use std::{collections::HashMap, io::Write};
use core::fmt::{self, Display};
use crate::{Error, Result};
use serde::ser::Serialize;
use log::debug;

pub struct Finalizer<'fin,W:Write,F:Formatter>(pub &'fin W, pub &'fin F);

pub fn to_writer<W: Write, S: Serialize>(writer: W, value: &S) -> Result<()> {
    let mut ser = Serializer::from_writer(writer);
    value.serialize(&mut ser)
}

pub fn to_finalizer<S: Serialize>(finalizer: Finalizer, value:&S) -> Result<()> {
  let mut ser = Serializer::from_finalizer(finalizer);
  value.serialize(&mut ser)
}

pub fn to_string<S: Serialize>(value: &S) -> Result<String> {
    let mut writer = Vec::new(); // TODO: with-capacity 128? bench
    to_writer(&mut writer, value)?;
    // We then check that the serialized string is the same as what we expect
    let string = String::from_utf8(writer)?;
    Ok(string)
}

pub struct Serializer<W:Write,F:Formatter>
{
  writer: W,
  formatter: Option<F>
}

impl<W:Write,F:Formatter> Serializer<W,F> {
  fn new(writer: W, formatter: Option<F>) -> Self {
    Serializer { writer, formatter }
  }
  fn from_writer(writer: W) -> Self {
    Serializer { writer, formatter: None }
  }
  fn from_finalizer(f:Finalizer) -> Self {
    Serializer { writer: *f.0, formatter: *f.1 }
  }
}

impl<'ser,W:Write,F:Formatter> serde::ser::Serializer for &'ser mut Serializer<W,F> {
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
