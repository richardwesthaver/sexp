//! ser.rs --- SEXP Serializer
use crate::fmt::{CanonicalFormatter, Formatter};
use crate::tok::{format_escaped_str, format_escaped_str_contents};
use crate::{Error, Result};
use core::fmt::{self, Display};
use core::num::FpCategory;
use serde::ser::{self, Serialize};
use std::{
  collections::HashMap,
  io::{self, Write},
};

#[inline]
pub fn to_writer<W: Write, S: ?Sized + Serialize>(
  writer: W,
  value: &S,
) -> Result<()> {
  let mut ser = Serializer::new(writer);
  value.serialize(&mut ser)
}

/// Serialize the given data structure as a canonical SEXP byte vector.
#[inline]
pub fn to_vec<S: ?Sized + Serialize>(value: &S) -> Result<Vec<u8>> {
  let mut writer = Vec::with_capacity(128);
  tri!(to_writer(&mut writer, value));
  Ok(writer)
}

#[inline]
pub fn to_string<S: ?Sized + Serialize>(value: &S) -> Result<String> {
  let mut writer = Vec::new(); // TODO: with-capacity 128? bench
  to_writer(&mut writer, value)?;
  // We then check that the serialized string is the same as what we expect
  let string = String::from_utf8(writer)?;
  Ok(string)
}

pub struct Serializer<W, F = CanonicalFormatter> {
  writer: W,
  formatter: F,
}

impl<W: Write> Serializer<W> {
  #[inline]
  fn new(writer: W) -> Self {
    Serializer::with_formatter(writer, CanonicalFormatter)
  }
}
impl<W: Write, F: Formatter> Serializer<W, F> {
  fn with_formatter(writer: W, formatter: F) -> Self {
    Serializer { writer, formatter }
  }
  #[inline]
  pub fn into_inner(self) -> W {
    self.writer
  }
}

// impl<'a, W:Write> Serializer<W, PrettyFormatter<'a>> {...}

impl<'ser, W: Write, F: Formatter> serde::ser::Serializer
  for &'ser mut Serializer<W, F>
{
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
    self
      .formatter
      .write_bool(&mut self.writer, v)
      .map_err(|e| e.into())
  }
  #[inline]
  fn serialize_i8(self, value: i8) -> Result<()> {
    self
      .formatter
      .write_i8(&mut self.writer, value)
      .map_err(|e| e.into())
  }

  #[inline]
  fn serialize_i16(self, value: i16) -> Result<()> {
    self
      .formatter
      .write_i16(&mut self.writer, value)
      .map_err(|e| e.into())
  }

  #[inline]
  fn serialize_i32(self, value: i32) -> Result<()> {
    self
      .formatter
      .write_i32(&mut self.writer, value)
      .map_err(|e| e.into())
  }

  #[inline]
  fn serialize_i64(self, value: i64) -> Result<()> {
    self
      .formatter
      .write_i64(&mut self.writer, value)
      .map_err(|e| e.into())
  }

  fn serialize_i128(self, value: i128) -> Result<()> {
    self
      .formatter
      .write_i128(&mut self.writer, value)
      .map_err(|e| e.into())
  }

  #[inline]
  fn serialize_u8(self, value: u8) -> Result<()> {
    self
      .formatter
      .write_u8(&mut self.writer, value)
      .map_err(|e| e.into())
  }

  #[inline]
  fn serialize_u16(self, value: u16) -> Result<()> {
    self
      .formatter
      .write_u16(&mut self.writer, value)
      .map_err(|e| e.into())
  }

  #[inline]
  fn serialize_u32(self, value: u32) -> Result<()> {
    self
      .formatter
      .write_u32(&mut self.writer, value)
      .map_err(|e| e.into())
  }

  #[inline]
  fn serialize_u64(self, value: u64) -> Result<()> {
    self
      .formatter
      .write_u64(&mut self.writer, value)
      .map_err(|e| e.into())
  }

  fn serialize_u128(self, value: u128) -> Result<()> {
    self
      .formatter
      .write_u128(&mut self.writer, value)
      .map_err(|e| e.into())
  }

  #[inline]
  fn serialize_f32(self, value: f32) -> Result<()> {
    match value.classify() {
      // TODO
      FpCategory::Nan | FpCategory::Infinite => self
        .formatter
        .write_nil(&mut self.writer)
        .map_err(|e| e.into()),
      _ => self
        .formatter
        .write_f32(&mut self.writer, value)
        .map_err(|e| e.into()),
    }
  }
  #[inline]
  fn serialize_f64(self, value: f64) -> Result<()> {
    match value.classify() {
      FpCategory::Nan | FpCategory::Infinite => self
        .formatter
        .write_nil(&mut self.writer)
        .map_err(|e| e.into()),
      _ => self
        .formatter
        .write_f64(&mut self.writer, value)
        .map_err(|e| e.into()),
    }
  }
  #[inline]
  fn serialize_char(self, value: char) -> Result<()> {
    // A char encoded as UTF-8 takes 4 bytes at most.
    let mut buf = [0; 4];
    self.serialize_str(value.encode_utf8(&mut buf))
  }
  #[inline]
  fn serialize_str(self, value: &str) -> Result<()> {
    format_escaped_str(&mut self.writer, &mut self.formatter, value)
      .map_err(|e| e.into())
  }
  #[inline]
  fn serialize_bytes(self, value: &[u8]) -> Result<()> {
    use serde::ser::SerializeSeq;
    let mut seq = tri!(self.serialize_seq(Some(value.len())));
    for byte in value {
      tri!(seq.serialize_element(byte));
    }
    seq.end()
  }
  #[inline]
  fn serialize_unit(self) -> Result<()> {
    self
      .formatter
      .write_nil(&mut self.writer)
      .map_err(|e| e.into())
  }
  #[inline]
  fn serialize_unit_struct(self, _name: &'static str) -> Result<()> {
    self.serialize_unit()
  }

  #[inline]
  fn serialize_unit_variant(
    self,
    _name: &'static str,
    _variant_index: u32,
    variant: &'static str,
  ) -> Result<()> {
    self.serialize_str(variant)
  }

  /// Serialize newtypes without an object wrapper.
  #[inline]
  fn serialize_newtype_struct<T>(
    self,
    _name: &'static str,
    value: &T,
  ) -> Result<()>
  where
    T: ?Sized + Serialize,
  {
    value.serialize(self)
  }

  #[inline]
  fn serialize_newtype_variant<T>(
    self,
    _name: &'static str,
    _variant_index: u32,
    variant: &'static str,
    value: &T,
  ) -> Result<()>
  where
    T: ?Sized + Serialize,
  {
    tri!(self
      .formatter
      .begin_list(&mut self.writer)
      .map_err(|e| e.into()));
    tri!(self
      .formatter
      .begin_slot_key(&mut self.writer)
      .map_err(|e| e.into()));
    tri!(self.serialize_str(variant));
    // tri!(self
    //     .formatter
    //     .end_object_key(&mut self.writer)
    //     .map_err(|e| e.into()));
    // tri!(self
    //     .formatter
    //     .begin_object_value(&mut self.writer)
    //     .map_err(|e| e.into()));
    tri!(value.serialize(&mut *self));
    // tri!(self
    //     .formatter
    //     .end_object_value(&mut self.writer)
    //     .map_err(|e| e.into()));
    self
      .formatter
      .end_list(&mut self.writer)
      .map_err(|e| e.into())
  }

  #[inline]
  fn serialize_none(self) -> Result<()> {
    self.serialize_unit()
  }

  #[inline]
  fn serialize_some<T>(self, value: &T) -> Result<()>
  where
    T: ?Sized + Serialize,
  {
    value.serialize(self)
  }

  #[inline]
  fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq> {
    // TODO: impl nil for seq of 0 length
    tri!(self
      .formatter
      .begin_list(&mut self.writer)
      .map_err(|e| e.into()));
    if len == Some(0) {
      tri!(self
        .formatter
        .end_list(&mut self.writer)
        .map_err(|e| e.into()));
      Ok(Compound::Map {
        ser: self,
        state: State::Empty,
      })
    } else {
      Ok(Compound::Map {
        ser: self,
        state: State::First,
      })
    }
  }

  #[inline]
  fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
    self.serialize_seq(Some(len))
  }

  #[inline]
  fn serialize_tuple_struct(
    self,
    _name: &'static str,
    len: usize,
  ) -> Result<Self::SerializeTupleStruct> {
    self.serialize_seq(Some(len))
  }

  #[inline]
  fn serialize_tuple_variant(
    self,
    _name: &'static str,
    _variant_index: u32,
    variant: &'static str,
    len: usize,
  ) -> Result<Self::SerializeTupleVariant> {
    tri!(self
      .formatter
      .begin_list(&mut self.writer)
      .map_err(|e| e.into()));
    tri!(self
      .formatter
      .begin_slot_key(&mut self.writer)
      .map_err(|e| e.into()));
    tri!(self.serialize_str(variant));
    // tri!(self
    //     .formatter
    //     .end_object_key(&mut self.writer)
    //     .map_err(|e| e.into()));
    // tri!(self
    //     .formatter
    //     .begin_object_value(&mut self.writer)
    //     .map_err(|e| e.into()));
    self.serialize_seq(Some(len))
  }

  #[inline]
  fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap> {
    tri!(self
      .formatter
      .begin_list(&mut self.writer)
      .map_err(|e| e.into()));
    if len == Some(0) {
      tri!(self
        .formatter
        .end_list(&mut self.writer)
        .map_err(|e| e.into()));
      Ok(Compound::Map {
        ser: self,
        state: State::Empty,
      })
    } else {
      Ok(Compound::Map {
        ser: self,
        state: State::First,
      })
    }
  }

  #[inline]
  fn serialize_struct(
    self,
    name: &'static str,
    len: usize,
  ) -> Result<Self::SerializeStruct> {
    self.serialize_map(Some(len))
  }

  #[inline]
  fn serialize_struct_variant(
    self,
    _name: &'static str,
    _variant_index: u32,
    variant: &'static str,
    len: usize,
  ) -> Result<Self::SerializeStructVariant> {
    tri!(self
      .formatter
      .begin_list(&mut self.writer)
      .map_err(|e| e.into()));
    tri!(self
      .formatter
      .begin_slot_key(&mut self.writer)
      .map_err(|e| e.into()));
    tri!(self.serialize_str(variant));
    // tri!(self
    //     .formatter
    //     .end_object_key(&mut self.writer)
    //     .map_err(|e| e.into()));
    // tri!(self
    //     .formatter
    //     .begin_object_value(&mut self.writer)
    //     .map_err(|e| e.into()));
    self.serialize_map(Some(len))
  }

  fn collect_str<T>(self, value: &T) -> Result<()>
  where
    T: ?Sized + Display,
  {
    use self::fmt::Write;

    struct Adapter<'ser, W: 'ser, F: 'ser> {
      writer: &'ser mut W,
      formatter: &'ser mut F,
      error: Option<io::Error>,
    }

    impl<'ser, W, F> Write for Adapter<'ser, W, F>
    where
      W: io::Write,
      F: Formatter,
    {
      fn write_str(&mut self, s: &str) -> fmt::Result {
        debug_assert!(self.error.is_none());
        match format_escaped_str_contents(self.writer, self.formatter, s) {
          Ok(()) => Ok(()),
          Err(err) => {
            self.error = Some(err);
            Err(fmt::Error)
          }
        }
      }
    }

    tri!(self
      .formatter
      .begin_string(&mut self.writer)
      .map_err(|e| e.into()));
    {
      let mut adapter = Adapter {
        writer: &mut self.writer,
        formatter: &mut self.formatter,
        error: None,
      };
      match write!(adapter, "{}", value) {
        Ok(()) => debug_assert!(adapter.error.is_none()),
        Err(e) => {
          return Err(Error::Custom {
            field: "invalid input".to_owned(),
          })
        }
      }
    }
    self
      .formatter
      .end_string(&mut self.writer)
      .map_err(|e| e.into())
  }
}

impl<'ser, W: Write, F: Formatter> ser::SerializeSeq
  for &'ser mut Serializer<W, F>
{
  type Ok = ();
  type Error = Error;
  #[inline]
  fn serialize_element<T: ?Sized + Serialize>(
    &mut self,
    value: &T,
  ) -> Result<()> {
    //    match self {
    //
    //      {
    Ok(())
  }
}
