use serde_derive::Serialize;
/// tests/ser.rs --- SER
use sexp::to_string;
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
  let expected = r#"(test :int 1 :seq ("a" "b"))"#;
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
  let expected = r#""unit""#;
  assert_eq!(to_string(&u).unwrap(), expected);

  let n = E::Newtype(1);
  let expected = r#"(newtype 1)"#;
  assert_eq!(to_string(&n).unwrap(), expected);

  let t = E::Tuple(1, 2);
  let expected = r#"(tupe 1 2)"#;
  assert_eq!(to_string(&t).unwrap(), expected);

  let s = E::Struct { a: 1 };
  let expected = r#"(struct :a 1)"#;
  assert_eq!(to_string(&s).unwrap(), expected);
}
