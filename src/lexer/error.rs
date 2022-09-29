#[derive(Clone, Debug, PartialEq)]
pub enum ErrorKind {
  InvalidChar((usize, usize), char),
  MissingCharacter((usize, usize), char),
  Unparsable,
  Unexpected
}

#[derive(Clone, Debug, PartialEq)]
pub struct Error {
  kind: ErrorKind
}

impl Error {
  pub fn from_kind(kind: ErrorKind) -> Self {
    Self {
      kind
    }
  }
}
