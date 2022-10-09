#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind<'v> {
  // Sigils
  OpenBrace,    // {
  CloseBrace,   // }
  OpenBracket,  // [
  CloseBracket, // ]
  OpenParen,    // (
  CloseParen,   // )
  Semicolon,    // ;
  Colon,        // ;
  Comma,        // ,
  Dot,          // .
  Plus,         // +
  Minus,        // -
  Star,         // *
  Slash,        // /
  Not,          // !
  NotEqual,     // !=
  Equal,        // =
  Equals,       // ==
  Greater,      // >
  GreaterEqual, // >=
  Less,         // <
  LessEqual,    // <=
  Amp,          // &
  And,          // &&
  Pipe,         // |
  Or,           // ||

  Arrow,   // ->
  NewLine, // \n

  // Literals
  ID(&'v str),
  Char(&'v str),
  String(&'v str),
  Int(i64),
  Float(f64),
  True,
  False,

  // Keywords
  Let,
  If,
  Else,
  While,
  For,
  In,
  Fn,
  Return
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Token<'i> {
  kind: TokenKind<'i>,
  pos: usize,
  len: usize
}

impl<'i> Token<'i> {
  pub fn new(kind: TokenKind<'i>, pos: usize, len: usize) -> Self {
    Self {
      kind,
      pos,
      len
    }
  }

  pub fn kind(&self) -> &TokenKind<'i> {
    &self.kind
  }

  pub fn pos(&self) -> usize {
    self.pos
  }

  pub fn len(&self) -> usize {
    self.len
  }
}
