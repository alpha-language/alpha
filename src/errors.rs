use crate::lexer::token;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ErrorKind<'t> {
  /// LEXER
  InvalidChar((usize, usize), char),
  MissingCharacter((usize, usize), char),
  Unparsable,
  Unexpected,

  /// PARSER
  ExpectedToken(token::TokenKind<'t>),
  UnexpectedToken {
    received: token::Token<'t>,
    expected: token::TokenKind<'t>
  },
  EOF
}

pub type Result<'e, T> = core::result::Result<T, Error<'e>>;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Error<'e> {
  kind: ErrorKind<'e>
}

impl<'e> Error<'e> {
  pub fn from_kind(kind: ErrorKind<'e>) -> Self {
    Self {
      kind
    }
  }

  pub fn kind(&self) -> &ErrorKind {
    &self.kind
  }
}
