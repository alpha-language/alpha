use core::mem;
use core::str::CharIndices;

use crate::errors::{Error, ErrorKind, Result};

use self::token::{Token, TokenKind};

mod tests;
pub mod token;

type CharIndice = Option<(usize, char)>;

pub struct Lexer<'i> {
  source: &'i str,

  cursor: usize,
  line: usize,
  col: usize,

  chars: CharIndices<'i>,
  previous: CharIndice,
  lookahead: CharIndice,
  lookahead2: CharIndice
}

impl<'i> Lexer<'i> {
  pub fn new(source: &'i str) -> Self {
    let mut chars = source.char_indices();
    let lookahead = chars.next();
    let lookahead2 = chars.next();

    Self {
      source,

      cursor: 0,
      line: 1,
      col: 1,

      chars,
      previous: None,
      lookahead,
      lookahead2
    }
  }

  fn bump(&mut self) -> CharIndice {
    let curr = mem::replace::<CharIndice>(
      &mut self.lookahead,
      mem::replace(&mut self.lookahead2, self.chars.next())
    );

    if let Some((_, ch)) = curr {
      self.previous = self.source.char_indices().nth(self.cursor);
      self.cursor += 1;
      self.col += 1;
      if ch == '\n' {
        self.col = 1;
        self.line += 1;
      }
    }

    curr
  }

  /// SKIP
  fn skip_until<F>(&mut self, mut predicate: F) -> Option<usize>
  where
    F: FnMut(char, CharIndice, CharIndice) -> bool
  {
    while let Some((i, ch)) = self.lookahead {
      if predicate(ch, self.previous, self.lookahead2) {
        return Some(i)
      } else {
        self.bump();
      }
    }

    Some(self.source.len())
  }

  fn skip_n(&mut self, n: usize) {
    for _ in 0..n {
      self.bump();
    }
  }

  fn skip_whitespace(&mut self) {
    self.skip_until(|ch, _, _| !ch.is_whitespace() && ch != '\n');
  }

  fn skip_to_endline(&mut self) {
    self.skip_until(|ch, _, _| ch == '\n');
  }

  fn skip_to_endcomment(&mut self) {
    self.skip_until(|ch, _, lookahead| match lookahead {
      Some((_, '/')) => ch == '*',
      _ => false
    });
    self.skip_n(2);
  }

  /// COLLECT
  fn collect_until<F>(&mut self, start: usize, predicate: F) -> Option<&'i str>
  where
    F: FnMut(char, CharIndice, CharIndice) -> bool
  {
    self
      .skip_until(predicate)
      .and_then(|i| self.source.get(start..i))
  }

  fn collect_number(&mut self, start: usize) -> Result<'i, Token<'i>> {
    let end = self.skip_until(|ch, _, _| !(ch == '_' || ch.is_ascii_digit()));

    // Check if it's a decimal or a field access after the . char
    if let (Some((_, '.')), Some((_, next_ch))) = (self.lookahead, self.lookahead2) {
      if next_ch.is_ascii_digit() {
        self.bump();
        return self
          .skip_until(|ch, _, _| !(ch == '_' || ch.is_ascii_digit()))
          .and_then(|i| self.source.get(start..i))
          .and_then(|str| Some((str.replace("_", "").parse::<f64>(), str.len())))
          .and_then(|(parsed_float, len)| match parsed_float {
            Ok(f) => Some(Token::new(TokenKind::Float(f), start, len)),
            Err(_) => None
          })
          .ok_or(Error::from_kind(ErrorKind::Unparsable))
      }
    }

    end
      .and_then(|i| self.source.get(start..i))
      .and_then(|str| Some((str.replace("_", "").parse::<i64>(), str.len())))
      .and_then(|(parsed_int, len)| match parsed_int {
        Ok(i) => Some(Token::new(TokenKind::Int(i), start, len)),
        Err(_) => None
      })
      .ok_or(Error::from_kind(ErrorKind::Unparsable))
  }
}

impl<'i> Iterator for Lexer<'i> {
  type Item = Result<'i, Token<'i>>;

  fn next(&mut self) -> Option<Self::Item> {
    self.skip_whitespace();

    match self.bump() {
      Some((i, '{')) => Some(Ok(Token::new(TokenKind::OpenBrace, i, 1))),
      Some((i, '}')) => Some(Ok(Token::new(TokenKind::CloseBrace, i, 1))),
      Some((i, '(')) => Some(Ok(Token::new(TokenKind::OpenParen, i, 1))),
      Some((i, ')')) => Some(Ok(Token::new(TokenKind::CloseParen, i, 1))),
      Some((i, '[')) => Some(Ok(Token::new(TokenKind::OpenBracket, i, 1))),
      Some((i, ']')) => Some(Ok(Token::new(TokenKind::CloseBracket, i, 1))),
      Some((i, ':')) => Some(Ok(Token::new(TokenKind::Colon, i, 1))),
      Some((i, ';')) => Some(Ok(Token::new(TokenKind::Semicolon, i, 1))),
      Some((i, ',')) => Some(Ok(Token::new(TokenKind::Comma, i, 1))),
      Some((i, '.')) => Some(Ok(Token::new(TokenKind::Dot, i, 1))),
      Some((i, '+')) => Some(Ok(Token::new(TokenKind::Plus, i, 1))),
      Some((i, '-')) =>
        if let Some((_, '>')) = self.lookahead {
          self.bump();
          Some(Ok(Token::new(TokenKind::Arrow, i, 2)))
        } else {
          Some(Ok(Token::new(TokenKind::Minus, i, 1)))
        },
      Some((i, '*')) => Some(Ok(Token::new(TokenKind::Star, i, 1))),
      Some((i, '/')) =>
        if let Some((_, '/')) = self.lookahead {
          self.skip_to_endline();
          self.next()
        } else if let Some((_, '*')) = self.lookahead {
          self.skip_to_endcomment();
          self.next()
        } else {
          Some(Ok(Token::new(TokenKind::Slash, i, 1)))
        },

      Some((i, '!')) =>
        if let Some((_, '=')) = self.lookahead {
          self.bump();
          Some(Ok(Token::new(TokenKind::NotEqual, i, 2)))
        } else {
          Some(Ok(Token::new(TokenKind::Not, i, 1)))
        },

      Some((i, '=')) =>
        if let Some((_, '=')) = self.lookahead {
          self.bump();
          Some(Ok(Token::new(TokenKind::Equals, i, 2)))
        } else {
          Some(Ok(Token::new(TokenKind::Equal, i, 1)))
        },

      Some((i, '>')) =>
        if let Some((_, '=')) = self.lookahead {
          self.bump();
          Some(Ok(Token::new(TokenKind::GreaterEqual, i, 2)))
        } else {
          Some(Ok(Token::new(TokenKind::Greater, i, 1)))
        },

      Some((i, '<')) =>
        if let Some((_, '=')) = self.lookahead {
          self.bump();
          Some(Ok(Token::new(TokenKind::LessEqual, i, 2)))
        } else {
          Some(Ok(Token::new(TokenKind::Less, i, 1)))
        },

      Some((i, '&')) =>
        if let Some((_, '&')) = self.lookahead {
          self.bump();
          Some(Ok(Token::new(TokenKind::And, i, 2)))
        } else {
          Some(Ok(Token::new(TokenKind::Amp, i, 1)))
        },

      Some((i, '|')) =>
        if let Some((_, '|')) = self.lookahead {
          self.bump();
          Some(Ok(Token::new(TokenKind::Or, i, 2)))
        } else {
          Some(Ok(Token::new(TokenKind::Pipe, i, 1)))
        },

      Some((i, '"')) => {
        let (line, col) = (self.line, self.col - 1);
        Some(
          self
            .collect_until(i, |ch, previous, _| {
              (ch == '"' || ch == '\n') && matches!(previous, Some((_, ch)) if ch != '\\')
            })
            .and_then(|str| match self.bump() {
              Some((_, '"')) => Some(Token::new(TokenKind::String(&str[1..]), i, str.len() + 1)),
              _ => None
            })
            .ok_or(Error::from_kind(ErrorKind::MissingCharacter(
              (line, col),
              '"'
            )))
        )
      },

      Some((i, '\'')) => {
        let (line, col) = (self.line, self.col - 1);
        Some(
          self
            .collect_until(i, |ch, previous, _| {
              (ch == '\'' && matches!(previous, Some((_, ch)) if ch != '\\')) || ch == '\n'
            })
            .and_then(|str| match self.bump() {
              Some((_, '\'')) => Some(Token::new(TokenKind::Char(&str[1..]), i, str.len() + 1)),
              _ => None
            })
            .ok_or(Error::from_kind(ErrorKind::MissingCharacter(
              (line, col),
              '\''
            )))
        )
      },

      Some((i, ch)) if ch == '_' || ch.is_alphabetic() => Some(
        self
          .collect_until(i, |ch, _, _| !(ch == '_' || ch.is_alphanumeric()))
          .and_then(|id| {
            Some((
              match id {
                "fn" => TokenKind::Fn,
                "let" => TokenKind::Let,
                "return" => TokenKind::Return,
                id => TokenKind::ID(id)
              },
              id.len()
            ))
          })
          .and_then(|(kind, len)| Some(Token::new(kind, i, len)))
          .ok_or(Error::from_kind(ErrorKind::Unexpected))
      ),

      Some((i, ch)) if ch.is_ascii_digit() => Some(self.collect_number(i)),
      Some((_, ch)) => Some(Err(Error::from_kind(ErrorKind::InvalidChar(
        (self.line, self.col - 1),
        ch
      )))),
      None => None
    }
  }
}
