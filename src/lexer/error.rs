#[derive(Clone, Debug, PartialEq)]
pub enum ErrorKind {
  InvalidChar(usize, char),
  MissingCharacter(usize, char),
  Unparsable,
  Unexpected
}

#[derive(Clone, Debug, PartialEq)]
pub struct Error {
  kind: ErrorKind
}

impl Error {
  pub fn kind(kind: ErrorKind) -> Self {
    Self { kind }
  }
}
