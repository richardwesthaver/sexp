//! tests/de.rs --- DE
use serde_derive::Deserialize;
use sexp::from_str;

#[test]
fn de_struct() {
  #[derive(Deserialize, PartialEq, Debug)]
  struct Test {
    int: u32,
    seq: Vec<String>,
  }
  // external tagging is serde default behavior
  let j = r#"(test :int 1 :seq ("a" "b"))"#;
  let expected = Test {
    int: 1,
    seq: vec!["a".to_owned(), "b".to_owned()],
  };
  assert_eq!(expected, from_str(j).unwrap());
}

#[test]
fn de_enum() {
  #[derive(Deserialize, PartialEq, Debug)]
  enum E {
    Unit,
    Newtype(u32),
    Tuple(u32, u32),
    Struct { a: u32 },
  }

  let j = r#""unit""#;
  let expected = E::Unit;
  assert_eq!(expected, from_str(j).unwrap());

  let j = r#"(newtype 1)"#;
  let expected = E::Newtype(1);
  assert_eq!(expected, from_str(j).unwrap());

  let j = r#"(tuple 1 2)"#;
  let expected = E::Tuple(1, 2);
  assert_eq!(expected, from_str(j).unwrap());

  let j = r#"(struct :a 1)"#;
  let expected = E::Struct { a: 1 };
  assert_eq!(expected, from_str(j).unwrap());
}
