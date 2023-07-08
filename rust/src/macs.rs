//! macs.rs --- SEXP Macro support
use crate::ast::Sexp;
use std::collections::HashMap;
pub type Macro = dyn FnMut(&mut Sexp);
pub type MacroObject = Box<Macro>;
pub struct ReadTable(HashMap<char, Box<Macro>>); // could use indexmap here..
pub struct WriteTable(Vec<(bool, Box<Macro>)>);
