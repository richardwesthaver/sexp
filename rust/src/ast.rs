//! ast.rs --- SEXP internal representation
use crate::tok::Num;
#[derive(Clone, Eq, PartialEq)]
pub enum Sexp {
  Nil,
  Number(Num),
  String(String),
  Symbol(String),
  List(Vec<Sexp>),
}
