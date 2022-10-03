use crate::errors::{Error, ErrorKind, Result};
use crate::helpers;
use crate::lexer::{self, token};

use core::mem;

use std::collections::VecDeque;

mod ast;

pub struct Parser<'i> {
  lexer: lexer::Lexer<'i>,

  current_token: Option<Result<'i, token::Token<'i>>>,
  lookahead_token: Option<Result<'i, token::Token<'i>>>
}

impl<'i> Parser<'i> {
  pub fn new(mut lexer: lexer::Lexer<'i>) -> Self {
    let current_token = lexer.next();
    let lookahead_token = lexer.next();

    Self {
      lexer,

      current_token,
      lookahead_token
    }
  }

  pub fn parse(&mut self) -> Result<'i, VecDeque<ast::Stmt>> {
    self.collect_statements()
  }

  fn bump(&mut self) -> Option<Result<'i, token::Token<'i>>> {
    helpers::move_to(
      &mut self.current_token,
      mem::replace(&mut self.lookahead_token, self.lexer.next())
    )
  }

  fn eat(&mut self, token_kind: token::TokenKind<'i>) -> Result<'i, ()> {
    match self.current_token {
      Some(token) =>
        if mem::discriminant(token?.kind()) == mem::discriminant(&token_kind) {
          self.bump();
          Ok(())
        } else {
          self.generate_unexpected(token?, token_kind)
        },

      None => return Err(Error::from_kind(ErrorKind::EOF))
    }
  }

  /// COLLECT
  fn collect_statements(&mut self) -> Result<'i, VecDeque<ast::Stmt>> {
    let mut list = VecDeque::new();
    list.push_back(self.collect_statement()?);

    while let Ok(()) = self.eat(token::TokenKind::Semicolon) {
      list.push_back(self.collect_statement()?);
    }

    Ok(list)
  }

  fn collect_statement(&mut self) -> Result<'i, ast::Stmt> {
    if let Some(token) = self.current_token {
      return match token?.kind() {
        token::TokenKind::Let => self.collect_var_def(),
        token::TokenKind::Fn => self.collect_fn_decl(),
        token::TokenKind::Return => {
          self.bump();
          Ok(ast::Stmt::Return(self.collect_expr()?))
        },
        _ => Ok(ast::Stmt::ExprStmt(self.collect_expr()?))
      }
    }

    self.bump();
    Ok(ast::Stmt::None)
  }

  fn collect_var_def(&mut self) -> Result<'i, ast::Stmt> {
    self.eat(token::TokenKind::Let)?;

    let name = match self.current_token {
      Some(token) => match token?.kind() {
        &token::TokenKind::ID(name) => name,
        _ => return self.generate_unexpected(token?, token::TokenKind::ID("abc"))
      },
      None => return self.generate_expected(token::TokenKind::ID("abc"))
    };
    self.bump();

    self.eat(token::TokenKind::Equal)?;

    Ok(ast::Stmt::Declaration(
      name.to_string(),
      self.collect_expr()?
    ))
  }

  fn collect_fn_decl(&mut self) -> Result<'i, ast::Stmt> {
    self.eat(token::TokenKind::Fn)?;

    let name = match self.current_token {
      Some(token) => match token?.kind() {
        &token::TokenKind::ID(name) => name,
        _ => return self.generate_unexpected(token?, token::TokenKind::ID("abc"))
      },
      None => return self.generate_expected(token::TokenKind::ID("abc"))
    };
    self.bump();

    let mut args = VecDeque::new();

    self.eat(token::TokenKind::OpenParen)?;
    loop {
      match self.current_token {
        Some(token) => match token?.kind() {
          &token::TokenKind::CloseParen => break,
          &token::TokenKind::ID(name) => {
            self.bump();
            self.eat(token::TokenKind::Colon)?;
            let r#type = match self.current_token {
              Some(token) => match token?.kind() {
                &token::TokenKind::ID(t) => t,
                _ => return self.generate_unexpected(token?, token::TokenKind::ID("abc"))
              },
              None => return self.generate_expected(token::TokenKind::ID("abc"))
            };
            self.bump();

            args.push_back((name.to_string(), r#type.to_string()));
          },
          _ => self.eat(token::TokenKind::ID("abc"))?
        },
        None => return self.generate_expected(token::TokenKind::ID("abc"))
      };

      match self.current_token {
        Some(token) => match token?.kind() {
          &token::TokenKind::CloseParen => break,
          &token::TokenKind::Comma => continue,
          _ => ()
        },
        None => ()
      };

      self.eat(token::TokenKind::Comma)?
    }
    self.eat(token::TokenKind::CloseParen)?;

    let block = self.collect_block()?;

    Ok(ast::Stmt::Declaration(
      name.to_string(),
      ast::Expr::Closure(args, match block {
        ast::Expr::Block(stmts) => stmts,
        _ => return self.generate_expected(token::TokenKind::OpenBracket)
      })
    ))
  }

  fn collect_expr(&mut self) -> Result<'i, ast::Expr> {
    let expr = match self.current_token {
      Some(token) => match token?.kind() {
        &token::TokenKind::Char(v) => ast::Expr::CharLiteral(v.to_string()),
        &token::TokenKind::String(v) => ast::Expr::StringLiteral(v.to_string()),
        &token::TokenKind::Int(v) => ast::Expr::IntLiteral(v),
        &token::TokenKind::Float(v) => ast::Expr::FloatLiteral(v),
        &token::TokenKind::ID(name) => match self.lookahead_token {
          Some(Ok(token)) => ast::Expr::None,
          _ => ast::Expr::Identifier(name.to_string())
        },

        _ => ast::Expr::None
      },
      None => ast::Expr::None
    };

    self.bump();

    Ok(expr)
  }

  fn collect_block(&mut self) -> Result<'i, ast::Expr> {
    self.eat(token::TokenKind::OpenBrace)?;

    let mut list = VecDeque::new();

    loop {
      list.push_back(self.collect_statement()?);
      self.eat(token::TokenKind::Semicolon)?;

      match self.current_token {
        Some(token) =>
          if token?.kind() == &token::TokenKind::CloseBrace {
            break
          },

        _ => continue
      };
    }
    self.eat(token::TokenKind::CloseBrace)?;

    Ok(ast::Expr::Block(list))
  }

  /// ERRORS
  fn generate_unexpected<T>(
    &self,
    token: token::Token<'i>,
    expected: token::TokenKind<'i>
  ) -> Result<'i, T> {
    Err(Error::from_kind(ErrorKind::UnexpectedToken {
      received: token,
      expected
    }))
  }

  fn generate_expected<T>(&self, expected: token::TokenKind<'i>) -> Result<'i, T> {
    Err(Error::from_kind(ErrorKind::ExpectedToken(expected)))
  }
}
