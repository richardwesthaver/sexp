//! de.rs --- SXP deserializer
use std::{io::Read, marker::PhantomData};
use serde::de::{self, Deserialize};
use crate::{Error, Result, fmt::Formatter};

pub fn from_reader<'de, R: Read, T: de::Deserialize<'de>>(reader: R) -> Result<T> {
    T::deserialize(&mut Deserializer::new_from_reader(reader))
}

pub fn from_str<'de, T: de::Deserialize<'de>>(s: &str) -> Result<T> {
  from_reader(s.as_bytes())
}

pub struct Deserializer<'de, R, F> {
  reader: &'de R,
  formatter: Option<F>,
}

impl<'de, 'a, R: Read, F: Formatter> de::Deserializer<'de>
  for &'a mut Deserializer<'de, R, F> {
    type Error = Error;
  }
