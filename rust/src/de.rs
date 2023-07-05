use std::{io::Read, marker::PhantomData};
use serde::de::{self, Deserialize};
use crate::{Error, Result};

pub fn from_reader<'de, R: Read, T: de::Deserialize<'de>>(reader: R) -> Result<T> {
    T::deserialize(&mut Deserializer::new_from_reader(reader))
}

pub fn from_str<'de, T: de::Deserialize<'de>>(s: &str) -> Result<T> {
  from_reader(s.as_bytes())
}

pub struct Deserializer<'de, R> {
  reader: &'de R
}

impl<'de, 'a, R: Read> de::Deserializer<'de>
  for &'a mut Deserializer<'de, R> {
    type Error = Error;
    
  }
