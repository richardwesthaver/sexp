/// SER
use sexp::to_string;
use serde_derive::Serialize;
#[test]
fn ser_struct() {
  #[derive(Serialize)]
  struct Test {
    int: u32,
    seq: Vec<&'static str>,
  }

  let test = Test {
    int: 1,
    seq: vec!["a", "b"],
  };
  let expected = r#"{"int":1,"seq":["a","b"]}"#;
  assert_eq!(to_string(&test).unwrap(), expected);
}

#[test]
fn ser_enum() {
  #[derive(Serialize)]
  enum E {
    Unit,
    Newtype(u32),
    Tuple(u32, u32),
    Struct { a: u32 },
  }

  let u = E::Unit;
  let expected = r#""Unit""#;
  assert_eq!(to_string(&u).unwrap(), expected);

  let n = E::Newtype(1);
  let expected = r#"{"Newtype":1}"#;
  assert_eq!(to_string(&n).unwrap(), expected);

  let t = E::Tuple(1, 2);
  let expected = r#"{"Tuple":[1,2]}"#;
  assert_eq!(to_string(&t).unwrap(), expected);

  let s = E::Struct { a: 1 };
  let expected = r#"{"Struct":{"a":1}}"#;
  assert_eq!(to_string(&s).unwrap(), expected);
}

/// DE
use sexp::from_str;
use serde_derive::Deserialize;

#[test]
fn de_struct() {
  #[derive(Deserialize, PartialEq, Debug)]
  struct Test {
    int: u32,
    seq: Vec<String>,
  }

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

  let j = r#""Unit""#;
  let expected = E::Unit;
  assert_eq!(expected, from_str(j).unwrap());

  let j = r#"{"Newtype":1}"#;
  let expected = E::Newtype(1);
  assert_eq!(expected, from_str(j).unwrap());

  let j = r#"{"Tuple":(1,2)}"#;
  let expected = E::Tuple(1, 2);
  assert_eq!(expected, from_str(j).unwrap());

  let j = r#"{"Struct":{"a":1}}"#;
  let expected = E::Struct { a: 1 };
  assert_eq!(expected, from_str(j).unwrap());
}
