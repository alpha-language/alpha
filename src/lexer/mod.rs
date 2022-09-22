use std::mem;
use std::str::CharIndices;

use self::error::{Error, ErrorKind};
use self::token::{Token, TokenKind};

mod error;
mod token;

type CharIndice = Option<(usize, char)>;

fn option_compare_with<T, F>(o: Option<T>, f: F) -> bool
where
  F: Fn(T) -> bool
{
  match o {
    Some(x) => f(x),
    None => false
  }
}

pub struct Lexer<'i> {
  source: &'i str,

  line: usize,
  col: usize,

  chars: CharIndices<'i>,
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

      line: 1,
      col: 1,

      chars,
      lookahead,
      lookahead2
    }
  }

  fn bump(&mut self) -> CharIndice {
    let curr = mem::replace::<CharIndice>(
      &mut self.lookahead, 
      mem::replace(
        &mut self.lookahead2, 
        self.chars.next()
      )
    );

    if let Some((_, ch)) = curr {
      self.col +=1;
      if ch == '\n' {
        self.col = 1;
        self.line += 1;
      }
    }

    curr
  }

  /// SKIP
  fn skip_until<F>(&mut self, mut predicate: F) -> Option<usize>
    where F: FnMut(char, CharIndice, CharIndice) -> bool
  {
    let mut prev_char = None;
    while let Some((i, ch)) = self.lookahead {
      if predicate(ch, prev_char, self.lookahead2) {
        return Some(i)
      }else{
        prev_char = self.bump();
      }
    }
    
    match self.lookahead {
      Some(_) => prev_char.and_then(|(i, ch)| {
        if predicate(ch, None, None) {
          Some(i + 1)
        } else {
          None
        }
      }),
      None => prev_char.map(|(i, ch)| i + ch.len_utf8())
    }
  }

  fn skip_n(&mut self, n: usize) {
    for _ in 0..n {
      self.bump();
    }
  }

  fn skip_whitespace(&mut self) {
    self.skip_until(|ch, _, _| !ch.is_whitespace());
  }
  
  fn skip_to_endline(&mut self) {
    self.skip_until(|ch, _, _| ch == '\n');
  }
  
  fn skip_to_endcomment(&mut self) {
    self.skip_until(|ch, _, lookahead| {
      match lookahead {
        Some((_, '/')) => ch == '*',
        _ => false
      }
    });
    self.skip_n(2);
  }

  /// COLLECT
  fn collect_until<F>(&mut self, start: usize, predicate: F) -> Option<&'i str>
  where
    F: FnMut(char, CharIndice, CharIndice) -> bool
  {
    self.skip_until(predicate)
      .and_then(|i| self.source.get(start..i))
  }

  fn collect_number(&mut self, start: usize) -> Result<Token<'i>, Error> {
    let end = self.skip_until(|ch, _, _| !(ch == '_' || ch.is_ascii_digit()));

    // Check if it's a decimal or a field access after the . char
    if let (Some((_, '.')), Some((_, next_ch))) = (self.lookahead, self.lookahead2) {
      if next_ch.is_ascii_digit() {
        self.bump();
        return self
          .skip_until(|ch, _, _| !(ch == '_' || ch.is_ascii_digit()))
          .and_then(|i| self.source.get(start..i))
          .and_then(|str| Some((str.replace("_", "").parse::<f64>(), str.len())))
          .and_then(|(parsed_float, len)| {
            match parsed_float {
              Ok(f) => Some(
                Token::new(
                  TokenKind::Float(f),
                  start,
                  len
                )
              ),
              Err(_) => None,
            }
            
          })
          .ok_or(Error::kind(ErrorKind::Unparsable))
      }
    }

    end.and_then(|i| self.source.get(start..i))
      .and_then(|str| Some((str.replace("_", "").parse::<i64>(), str.len())))
      .and_then(|(parsed_int, len)| {
        match parsed_int {
          Ok(i) => Some(
            Token::new(
              TokenKind::Int(i),
              start,
              len
            )
          ),
          Err(_) => None,
        }
        
      })
      .ok_or(Error::kind(ErrorKind::Unparsable))
  }
}

impl<'i> Iterator for Lexer<'i> {
  type Item = Result<Token<'i>, Error>;

  fn next(&mut self) -> Option<Self::Item> {
    self.skip_whitespace();

    match self.bump() {
      Some((i, '{')) => Some(Ok(Token::new(TokenKind::OpenBrace, i, 1))),
      Some((i, '}')) => Some(Ok(Token::new(TokenKind::CloseBrace, i, 1))),
      Some((i, '(')) => Some(Ok(Token::new(TokenKind::OpenParen, i, 1))),
      Some((i, ')')) => Some(Ok(Token::new(TokenKind::CloseParen, i, 1))),
      Some((i, '[')) => Some(Ok(Token::new(TokenKind::OpenBracket, i, 1))),
      Some((i, ']')) => Some(Ok(Token::new(TokenKind::CloseBracket, i, 1))),
      Some((i, ';')) => Some(Ok(Token::new(TokenKind::Semicolon, i, 1))),
      Some((i, ',')) => Some(Ok(Token::new(TokenKind::Comma, i, 1))),
      Some((i, '.')) => Some(Ok(Token::new(TokenKind::Dot, i, 1))),
      Some((i, '+')) => Some(Ok(Token::new(TokenKind::Plus, i, 1))),
      Some((i, '-')) => {
        if let Some((_, '>')) = self.lookahead {
          self.bump();
          Some(Ok(Token::new(TokenKind::Arrow, i, 2)))
        } else {
          Some(Ok(Token::new(TokenKind::Minus, i, 1)))
        }
      },
      Some((i, '*')) => Some(Ok(Token::new(TokenKind::Star, i, 1))),
      Some((i, '/')) => {
        if let Some((_, '/')) = self.lookahead {
          self.skip_to_endline();
          self.next()
        } else if let Some((_, '*')) = self.lookahead {
          self.skip_to_endcomment();
          self.next()
        } else {
          Some(Ok(Token::new(TokenKind::Slash, i, 1)))
        }
      }

      Some((i, '!')) => {
        if let Some((_, '=')) = self.lookahead {
          self.bump();
          Some(Ok(Token::new(TokenKind::NotEqual, i, 2)))
        } else {
          Some(Ok(Token::new(TokenKind::Not, i, 1)))
        }
      }

      Some((i, '=')) => {
        if let Some((_, '=')) = self.lookahead {
          self.bump();
          Some(Ok(Token::new(TokenKind::Equals, i, 2)))
        } else {
          Some(Ok(Token::new(TokenKind::Equal, i, 1)))
        }
      }

      Some((i, '>')) => {
        if let Some((_, '=')) = self.lookahead {
          self.bump();
          Some(Ok(Token::new(TokenKind::GreaterEqual, i, 2)))
        } else {
          Some(Ok(Token::new(TokenKind::Greater, i, 1)))
        }
      }

      Some((i, '<')) => {
        if let Some((_, '=')) = self.lookahead {
          self.bump();
          Some(Ok(Token::new(TokenKind::LessEqual, i, 2)))
        } else {
          Some(Ok(Token::new(TokenKind::Less, i, 1)))
        }
      }

      Some((i, '&')) => {
        if let Some((_, '&')) = self.lookahead {
          self.bump();
          Some(Ok(Token::new(TokenKind::And, i, 2)))
        } else {
          Some(Ok(Token::new(TokenKind::Amp, i, 1)))
        }
      }

      Some((i, '|')) => {
        if let Some((_, '|')) = self.lookahead {
          self.bump();
          Some(Ok(Token::new(TokenKind::Or, i, 2)))
        } else {
          Some(Ok(Token::new(TokenKind::Pipe, i, 1)))
        }
      }

      Some((i, '"')) =>{
        let (line, col) = (self.line, self.col);
        Some(
          self.collect_until(i, |ch, previous, _| 
            (ch == '"' || ch == '\n') && option_compare_with(previous, |(_, ch)| ch != '\\')
          ).and_then(|str| {
            match self.bump() {
              Some((_, '"')) => Some(
                Token::new(
                  TokenKind::String(&str[1..]),
                  i,
                  str.len() + 1
                )
              ),
              _ => None
            }
          })
          .ok_or(Error::kind(ErrorKind::MissingCharacter((line, col), '"')))
        )
      },
      
      Some((i, '\'')) => {
        let (line, col) = (self.line, self.col);
        Some(
          self.collect_until(i, |ch, previous, _|
            (ch == '\'' && option_compare_with(previous, |(_, ch)| ch != '\\')) || ch == '\n'
          )
          .and_then(|str| {
            match self.bump() {
              Some((_, '\'')) => Some(
                Token::new(
                  TokenKind::Char(&str[1..]),
                  i,
                  str.len() + 1
                )
              ),
              _ => None
            }
          })
          .ok_or(Error::kind(ErrorKind::MissingCharacter((line, col), '\'')))
        )
      },

      Some((i, ch)) if ch == '_' || ch.is_alphabetic() => {
        Some(self.collect_until(i, |ch, _, _| !(ch == '_' || ch.is_alphanumeric()))
          .and_then(|id| {
            Some((match id {
              "fn" => TokenKind::Fn,
              "return" => TokenKind::Return,
              id => TokenKind::ID(id)
            }, id.len()))
          })
          .and_then(|(kind, len)| Some(Token::new(kind, i, len)))
          .ok_or(Error::kind(ErrorKind::Unexpected)))
      },

      Some((i, ch)) if ch.is_ascii_digit() => Some(self.collect_number(i)),
      Some((_, ch)) => Some(Err(Error::kind(ErrorKind::InvalidChar((self.line, self.col - 1), ch)))),
      None => None
    }
  }
}
