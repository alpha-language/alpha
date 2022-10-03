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

    let value = self.collect_expr()?;
    self.bump();

    Ok(ast::Stmt::Declaration(name.to_string(), value))
  }

  fn collect_var_assignment(&mut self) -> Result<'i, ast::Expr> {
    let name = match self.current_token {
      Some(token) => match token?.kind() {
        &token::TokenKind::ID(name) => name,
        _ => return self.generate_unexpected(token?, token::TokenKind::ID("abc"))
      },
      None => return self.generate_expected(token::TokenKind::ID("abc"))
    };
    self.bump();

    self.eat(token::TokenKind::Equal)?;

    let value = self.collect_expr()?;
    self.bump();

    Ok(ast::Expr::Assign(name.to_string(), Box::new(value)))
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
    let left = match self.current_token {
      Some(token) => match token?.kind() {
        &token::TokenKind::Char(v) => ast::Expr::CharLiteral(v.to_string()),
        &token::TokenKind::String(v) => ast::Expr::StringLiteral(v.to_string()),
        &token::TokenKind::Int(v) => ast::Expr::IntLiteral(v),
        &token::TokenKind::Float(v) => ast::Expr::FloatLiteral(v),
        &token::TokenKind::ID(name) => match self.lookahead_token {
          Some(next_token) => match next_token?.kind() {
            &token::TokenKind::Equal => self.collect_var_assignment()?,
            _ => ast::Expr::Identifier(name.to_string())
          },
          _ => ast::Expr::Identifier(name.to_string())
        },
        &token::TokenKind::OpenParen => {
          self.bump();
          let expr = self.collect_expr()?;
          self.eat(token::TokenKind::CloseParen);
          self.bump();
          expr
        },

        _ => ast::Expr::None
      },
      None => ast::Expr::None
    };

    let right = match self.lookahead_token {
      Some(token) => match token?.kind() {
        &token::TokenKind::Plus
        | &token::TokenKind::Minus
        | &token::TokenKind::Star
        | &token::TokenKind::Slash
        | &token::TokenKind::NotEqual
        | &token::TokenKind::Equal
        | &token::TokenKind::Equals
        | &token::TokenKind::Greater
        | &token::TokenKind::GreaterEqual
        | &token::TokenKind::Less
        | &token::TokenKind::LessEqual
        | &token::TokenKind::Amp
        | &token::TokenKind::And
        | &token::TokenKind::Pipe
        | &token::TokenKind::Or => {
          self.bump();
          self.collect_binary_op()?
        },
        _ => None
      },
      None => None
    };

    match right {
      Some((op, right)) => Ok(ast::Expr::BinaryOp(op, Box::new(left), Box::new(right))),
      None => Ok(left)
    }
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

  fn collect_binary_op(&mut self) -> Result<'i, Option<(ast::BinaryOp, ast::Expr)>> {
    let op = self.current_token;
    self.bump();
    match op {
      Some(token) => match token?.kind() {
        &token::TokenKind::Plus => Ok(Some((ast::BinaryOp::Add, self.collect_expr()?))),
        &token::TokenKind::Minus => Ok(Some((ast::BinaryOp::Subtract, self.collect_expr()?))),
        &token::TokenKind::Star => Ok(Some((ast::BinaryOp::Multiply, self.collect_expr()?))),
        &token::TokenKind::Slash => Ok(Some((ast::BinaryOp::Divide, self.collect_expr()?))),
        &token::TokenKind::NotEqual => Ok(Some((ast::BinaryOp::NotEquals, self.collect_expr()?))),
        &token::TokenKind::Equals => Ok(Some((ast::BinaryOp::Equals, self.collect_expr()?))),
        &token::TokenKind::Greater => Ok(Some((ast::BinaryOp::GreaterThan, self.collect_expr()?))),
        &token::TokenKind::GreaterEqual =>
          Ok(Some((ast::BinaryOp::GreaterEquals, self.collect_expr()?))),
        &token::TokenKind::Less => Ok(Some((ast::BinaryOp::LessThan, self.collect_expr()?))),
        &token::TokenKind::LessEqual => Ok(Some((ast::BinaryOp::LessEquals, self.collect_expr()?))),
        &token::TokenKind::Amp => Ok(Some((ast::BinaryOp::BineryAnd, self.collect_expr()?))),
        &token::TokenKind::And => Ok(Some((ast::BinaryOp::And, self.collect_expr()?))),
        &token::TokenKind::Pipe => Ok(Some((ast::BinaryOp::BineryOr, self.collect_expr()?))),
        &token::TokenKind::Or => Ok(Some((ast::BinaryOp::Or, self.collect_expr()?))),
        _ => Ok(None)
      },
      None => Ok(None)
    }
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
