use crate::lexer::{self, token};
use core::mem;
use std::collections::VecDeque;

use self::error::{Error, ErrorKind};

mod ast;
mod error;

pub struct Parser<'i> {
  lexer: lexer::Lexer<'i>,

  lookahead_token: Option<lexer::LexerToken<'i>>
}

impl<'i> Parser<'i> {
  pub fn new(lexer: lexer::Lexer<'i>) -> Self {
    Self {
      lexer,

      lookahead_token: None
    }
  }

  pub fn parse(&mut self) -> VecDeque<ast::Stmt> {
    self.collect_statements()
  }

  fn bump(&mut self) -> Option<lexer::LexerToken> {
    mem::replace(&mut self.lookahead_token, self.lexer.next())
  }

  fn eat(&mut self, token_kind: token::TokenKind) -> Result<(), error::Error> {
    match self.lexer.next() {
      Some(Ok(token)) =>
        if mem::discriminant(token.kind()) == mem::discriminant(&token_kind) {
          Ok(())
        } else {
          Err(Error::from_kind(ErrorKind::UnexpectedToken(token)))
        },

      Some(Err(e)) => return Err(Error::from_kind(ErrorKind::Lexer(e))),
      None => return Err(Error::from_kind(ErrorKind::EOF))
    }
  }

  /// COLLECT
  fn collect_statements(&mut self) -> VecDeque<ast::Stmt> {
    vec![].into()
  }

  // fn collect_statement(&mut self) -> ast::Stmt {}

  // fn collect_expr(&mut self) -> ast::Expr {}
}
