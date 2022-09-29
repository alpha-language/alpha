use crate::lexer;

#[derive(Clone, Debug, PartialEq)]
pub enum ErrorKind<'i> {
  Lexer(lexer::error::Error),
  UnexpectedToken(lexer::token::Token<'i>),
  EOF
}

#[derive(Clone, Debug, PartialEq)]
pub struct Error<'i> {
  kind: ErrorKind<'i>
}

impl<'i> Error<'i> {
  pub fn from_kind(kind: ErrorKind<'i>) -> Self {
    Self {
      kind
    }
  }
}
