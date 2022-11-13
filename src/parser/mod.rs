use crate::errors::{Error, ErrorKind, Result};
use crate::helpers;
use crate::lexer::{self, token};

use core::mem;

use std::collections::VecDeque;

pub mod ast;
mod tests;
pub mod transpiler;

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
    self.see(token_kind)?;
    self.bump();
    Ok(())
  }

  fn see(&mut self, token_kind: token::TokenKind<'i>) -> Result<'i, ()> {
    match self.current_token {
      Some(token) =>
        if mem::discriminant(token?.kind()) == mem::discriminant(&token_kind) {
          Ok(())
        } else {
          self.generate_unexpected(token?, token_kind)
        },

      None => self.generate_expected(token_kind)
    }
  }

  /// COLLECT
  fn collect_statements(&mut self) -> Result<'i, VecDeque<ast::Stmt>> {
    let mut list = VecDeque::new();

    loop {
      if let None = self.current_token {
        break
      }

      let statement = match self.collect_statement() {
        Err(e) if e.kind() == &ErrorKind::EOF => break,
        stmt => stmt?
      };

      if statement != ast::Stmt::None && statement != ast::Stmt::ExprStmt(ast::Expr::None) {
        list.push_back(statement.clone());
      }

      match statement {
        ast::Stmt::Function(_, _, _) | ast::Stmt::While(_, _) | ast::Stmt::For(_, _, _) => continue,
        ast::Stmt::ExprStmt(expr) => match expr {
          ast::Expr::If(_) | ast::Expr::Block(_) => continue,
          _ => self.eat(token::TokenKind::Semicolon)?
        },
        _ => self.eat(token::TokenKind::Semicolon)?
      }
    }

    Ok(list)
  }

  fn collect_statement(&mut self) -> Result<'i, ast::Stmt> {
    if let Some(token) = self.current_token {
      return match token?.kind() {
        token::TokenKind::Let => self.collect_var_def(),
        token::TokenKind::Fn => self.collect_fn_decl(),
        token::TokenKind::While => self.collect_while(),
        token::TokenKind::For => self.collect_for(),
        token::TokenKind::Semicolon => Ok(ast::Stmt::None),
        token::TokenKind::Return => {
          self.eat(token::TokenKind::Return)?;
          Ok(ast::Stmt::Return(self.collect_expr()?))
        },
        _ => Ok(ast::Stmt::ExprStmt(self.collect_expr()?))
      }
    }

    Ok(ast::Stmt::None)
  }

  fn collect_var_def(&mut self) -> Result<'i, ast::Stmt> {
    self.eat(token::TokenKind::Let)?;

    let name = self.collect_id()?;

    self.eat(token::TokenKind::Equal)?;

    let value = self.collect_expr()?;

    Ok(ast::Stmt::Declaration(name.to_string(), value))
  }

  fn collect_var_assignment(&mut self) -> Result<'i, ast::Expr> {
    let name = self.collect_expr()?;

    self.eat(token::TokenKind::Equal)?;

    let value = self.collect_expr()?;

    Ok(ast::Expr::Assign(Box::new(name), Box::new(value)))
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

    let args = self.collect_args()?;

    let block = self.collect_block()?;

    Ok(ast::Stmt::Function(name.to_string(), args, match block {
      ast::Expr::Block(stmts) => stmts,
      _ => return self.generate_expected(token::TokenKind::OpenBracket)
    }))
  }

  fn collect_fn_call(&mut self) -> Result<'i, ast::Expr> {
    let name = self.collect_expr()?;

    let mut args = VecDeque::new();

    self.eat(token::TokenKind::OpenParen)?;
    loop {
      match self.current_token {
        Some(token) => match token?.kind() {
          &token::TokenKind::CloseParen => break,
          _ => {
            args.push_back(self.collect_expr()?);
          }
        },
        None => return self.generate_expected(token::TokenKind::ID("abc"))
      };

      match self.current_token {
        Some(token) => match token?.kind() {
          &token::TokenKind::CloseParen => break,
          &token::TokenKind::Comma => {
            self.eat(token::TokenKind::Comma)?;
            continue
          },
          _ => ()
        },
        None => ()
      };
    }
    self.eat(token::TokenKind::CloseParen)?;

    Ok(ast::Expr::Call(Box::new(name), args))
  }

  fn collect_if(&mut self) -> Result<'i, ast::Expr> {
    let mut conditions = VecDeque::new();
    let mut first = true;

    loop {
      if !mem::replace(&mut first, false) {
        self.eat(token::TokenKind::Else)?;
      }

      let condition = if let Ok(()) = self.see(token::TokenKind::If) {
        self.eat(token::TokenKind::If)?;
        Some(self.collect_expr()?)
      } else {
        None
      };

      let block = self.collect_block()?;
      conditions.push_back((helpers::copy(&condition), match block {
        ast::Expr::Block(stmts) => stmts,
        _ => return self.generate_expected(token::TokenKind::OpenBracket)
      }));

      match (condition, self.current_token) {
        (Some(_), Some(_)) => match self.see(token::TokenKind::Else) {
          Ok(_) => continue,
          Err(_) => break Ok(ast::Expr::If(conditions))
        },
        _ => break Ok(ast::Expr::If(conditions))
      }
    }
  }

  fn collect_while(&mut self) -> Result<'i, ast::Stmt> {
    self.eat(token::TokenKind::While)?;

    let codition = self.collect_expr()?;

    let block = self.collect_block()?;

    Ok(ast::Stmt::While(codition, match block {
      ast::Expr::Block(stmts) => stmts,
      _ => return self.generate_expected(token::TokenKind::OpenBracket)
    }))
  }

  fn collect_for(&mut self) -> Result<'i, ast::Stmt> {
    self.eat(token::TokenKind::For)?;

    let name = match self.current_token {
      Some(token) => match token?.kind() {
        &token::TokenKind::ID(name) => name,
        _ => return self.generate_unexpected(token?, token::TokenKind::ID("abc"))
      },
      None => return self.generate_expected(token::TokenKind::ID("abc"))
    };
    self.bump();
    self.eat(token::TokenKind::In)?;

    let codition = self.collect_expr()?;

    let block = self.collect_block()?;

    Ok(ast::Stmt::For(name.to_string(), codition, match block {
      ast::Expr::Block(stmts) => stmts,
      _ => return self.generate_expected(token::TokenKind::OpenBracket)
    }))
  }

  fn collect_expr(&mut self) -> Result<'i, ast::Expr> {
    self.collect_comparator()
  }

  fn collect_block(&mut self) -> Result<'i, ast::Expr> {
    self.eat(token::TokenKind::OpenBrace)?;

    let mut list = VecDeque::new();

    loop {
      let statement = match self.collect_statement() {
        Err(e) if e.kind() == &ErrorKind::EOF => break,
        stmt => stmt?
      };

      if statement != ast::Stmt::None && statement != ast::Stmt::ExprStmt(ast::Expr::None) {
        list.push_back(statement.clone());
      }

      match self.current_token {
        Some(token) =>
          if token?.kind() == &token::TokenKind::CloseBrace {
            break
          },

        None => break
      };

      match statement {
        ast::Stmt::Function(_, _, _) | ast::Stmt::While(_, _) | ast::Stmt::For(_, _, _) => continue,
        ast::Stmt::ExprStmt(ref expr) => match expr {
          ast::Expr::If(_) | ast::Expr::Block(_) => continue,
          _ => self.eat(token::TokenKind::Semicolon)?
        },
        _ => self.eat(token::TokenKind::Semicolon)?
      }
    }
    self.eat(token::TokenKind::CloseBrace)?;

    Ok(ast::Expr::Block(list))
  }

  fn collect_binary(&mut self) -> Result<'i, ast::Expr> {
    let mut expr = self.collect_litteral()?;

    while let Some(token) = self.current_token {
      expr = match token?.kind() {
        &token::TokenKind::Amp => {
          self.eat(token::TokenKind::Amp)?;
          ast::Expr::Op(
            ast::Op::BineryAnd,
            Box::new(expr),
            Box::new(self.collect_litteral()?)
          )
        },
        &token::TokenKind::Pipe => {
          self.eat(token::TokenKind::Pipe)?;
          ast::Expr::Op(
            ast::Op::BineryOr,
            Box::new(expr),
            Box::new(self.collect_litteral()?)
          )
        },
        &token::TokenKind::Dot => {
          self.eat(token::TokenKind::Dot)?;
          ast::Expr::Op(
            ast::Op::Dot,
            Box::new(expr),
            Box::new(self.collect_litteral()?)
          )
        },
        _ => break
      };
    }

    Ok(expr)
  }

  fn collect_factor(&mut self) -> Result<'i, ast::Expr> {
    let mut expr = self.collect_binary()?;

    while let Some(token) = self.current_token {
      expr = match token?.kind() {
        &token::TokenKind::Star => {
          self.eat(token::TokenKind::Star)?;
          ast::Expr::Op(
            ast::Op::Multiply,
            Box::new(expr),
            Box::new(self.collect_binary()?)
          )
        },
        &token::TokenKind::Slash => {
          self.eat(token::TokenKind::Slash)?;
          ast::Expr::Op(
            ast::Op::Divide,
            Box::new(expr),
            Box::new(self.collect_binary()?)
          )
        },
        &token::TokenKind::Percent => {
          self.eat(token::TokenKind::Percent)?;
          ast::Expr::Op(
            ast::Op::ModDiv,
            Box::new(expr),
            Box::new(self.collect_binary()?)
          )
        },
        _ => break
      };
    }

    Ok(expr)
  }

  fn collect_term(&mut self) -> Result<'i, ast::Expr> {
    let mut expr = self.collect_factor()?;

    while let Some(token) = self.current_token {
      expr = match token?.kind() {
        &token::TokenKind::Plus => {
          self.eat(token::TokenKind::Plus)?;
          ast::Expr::Op(
            ast::Op::Add,
            Box::new(expr),
            Box::new(self.collect_factor()?)
          )
        },
        &token::TokenKind::Minus => {
          self.eat(token::TokenKind::Minus)?;
          ast::Expr::Op(
            ast::Op::Subtract,
            Box::new(expr),
            Box::new(self.collect_factor()?)
          )
        },
        _ => break
      };
    }

    Ok(expr)
  }

  fn collect_comparator(&mut self) -> Result<'i, ast::Expr> {
    let mut expr = self.collect_term()?;

    while let Some(token) = self.current_token {
      expr = match token?.kind() {
        &token::TokenKind::In => {
          self.eat(token::TokenKind::In)?;
          ast::Expr::Op(ast::Op::In, Box::new(expr), Box::new(self.collect_term()?))
        },
        &token::TokenKind::Equals => {
          self.eat(token::TokenKind::Equals)?;
          ast::Expr::Op(
            ast::Op::Equals,
            Box::new(expr),
            Box::new(self.collect_term()?)
          )
        },
        &token::TokenKind::NotEqual => {
          self.eat(token::TokenKind::NotEqual)?;
          ast::Expr::Op(
            ast::Op::NotEquals,
            Box::new(expr),
            Box::new(self.collect_term()?)
          )
        },
        &token::TokenKind::Greater => {
          self.eat(token::TokenKind::Greater)?;
          ast::Expr::Op(
            ast::Op::GreaterThan,
            Box::new(expr),
            Box::new(self.collect_term()?)
          )
        },
        &token::TokenKind::GreaterEqual => {
          self.eat(token::TokenKind::GreaterEqual)?;
          ast::Expr::Op(
            ast::Op::GreaterEquals,
            Box::new(expr),
            Box::new(self.collect_term()?)
          )
        },
        &token::TokenKind::Less => {
          self.eat(token::TokenKind::Less)?;
          ast::Expr::Op(
            ast::Op::LessThan,
            Box::new(expr),
            Box::new(self.collect_term()?)
          )
        },
        &token::TokenKind::LessEqual => {
          self.eat(token::TokenKind::LessEqual)?;
          ast::Expr::Op(
            ast::Op::LessEquals,
            Box::new(expr),
            Box::new(self.collect_term()?)
          )
        },
        &token::TokenKind::And => {
          self.eat(token::TokenKind::And)?;
          ast::Expr::Op(ast::Op::And, Box::new(expr), Box::new(self.collect_term()?))
        },
        &token::TokenKind::Or => {
          self.eat(token::TokenKind::Or)?;
          ast::Expr::Op(ast::Op::Or, Box::new(expr), Box::new(self.collect_term()?))
        },
        _ => break
      };
    }

    Ok(expr)
  }

  fn collect_litteral(&mut self) -> Result<'i, ast::Expr> {
    Ok(match self.current_token {
      Some(token) => match token?.kind() {
        &token::TokenKind::Char(v) => {
          self.eat(token::TokenKind::Char(""))?;
          ast::Expr::CharLiteral(v.to_string())
        },
        &token::TokenKind::String(v) => {
          self.eat(token::TokenKind::String(""))?;
          ast::Expr::StringLiteral(v.to_string())
        },
        &token::TokenKind::Int(v) => {
          self.eat(token::TokenKind::Int(0))?;
          ast::Expr::IntLiteral(v)
        },
        &token::TokenKind::Float(v) => {
          self.eat(token::TokenKind::Float(0.))?;
          ast::Expr::FloatLiteral(v)
        },
        &token::TokenKind::ID(name) => match self.lookahead_token {
          Some(next_token) => match next_token?.kind() {
            &token::TokenKind::Equal => self.collect_var_assignment()?,
            &token::TokenKind::OpenParen => self.collect_fn_call()?,
            _ => {
              self.eat(token::TokenKind::ID(""))?;
              ast::Expr::Identifier(name.to_string())
            }
          },
          _ => {
            self.eat(token::TokenKind::ID(""))?;
            ast::Expr::Identifier(name.to_string())
          }
        },
        &token::TokenKind::True => {
          self.eat(token::TokenKind::True)?;
          ast::Expr::BooleanLiteral(true)
        },
        &token::TokenKind::False => {
          self.eat(token::TokenKind::False)?;
          ast::Expr::BooleanLiteral(false)
        },
        &token::TokenKind::If => self.collect_if()?,
        &token::TokenKind::Fn => self.collect_closure()?,
        &token::TokenKind::OpenBrace => self.collect_block()?,
        &token::TokenKind::OpenParen => {
          self.eat(token::TokenKind::OpenParen)?;
          let expr = self.collect_expr()?;
          self.eat(token::TokenKind::CloseParen)?;
          expr
        },
        &token::TokenKind::Minus => {
          self.eat(token::TokenKind::Minus)?;
          ast::Expr::UnaryOp(ast::UnaryOp::Negate, Box::new(self.collect_litteral()?))
        },
        &token::TokenKind::Not => {
          self.eat(token::TokenKind::Not)?;
          ast::Expr::UnaryOp(ast::UnaryOp::Not, Box::new(self.collect_litteral()?))
        },

        _ => ast::Expr::None
      },
      None => ast::Expr::None
    })
  }

  fn collect_id(&mut self) -> Result<'i, String> {
    let id = match self.current_token {
      Some(token) => match token?.kind() {
        &token::TokenKind::ID(name) => name.to_string(),
        _ => return self.generate_unexpected(token?, token::TokenKind::ID("abc"))
      },
      None => return self.generate_expected(token::TokenKind::ID("abc"))
    };

    self.bump();

    Ok(id)
  }

  fn collect_closure(&mut self) -> Result<'i, ast::Expr> {
    self.eat(token::TokenKind::Fn)?;

    let args = self.collect_args()?;

    let block = self.collect_block()?;

    Ok(ast::Expr::Closure(args, match block {
      ast::Expr::Block(stmts) => stmts,
      _ => return self.generate_expected(token::TokenKind::OpenBracket)
    }))
  }

  fn collect_args(&mut self) -> Result<'i, VecDeque<(String, Box<ast::Expr>)>> {
    let mut args = VecDeque::new();

    self.eat(token::TokenKind::OpenParen)?;
    loop {
      match self.current_token {
        Some(token) => match token?.kind() {
          &token::TokenKind::CloseParen => break,
          &token::TokenKind::ID(name) => {
            self.bump();
            self.eat(token::TokenKind::Colon)?;
            let r#type = self.collect_expr()?;

            args.push_back((name.to_string(), Box::new(r#type)));
          },
          _ => self.eat(token::TokenKind::ID("abc"))?
        },
        None => return self.generate_expected(token::TokenKind::ID("abc"))
      };

      match self.current_token {
        Some(token) => match token?.kind() {
          &token::TokenKind::CloseParen => break,
          &token::TokenKind::Comma => {
            self.eat(token::TokenKind::Comma)?;
            continue
          },
          _ => ()
        },
        None => ()
      };
    }
    self.eat(token::TokenKind::CloseParen)?;

    Ok(args)
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
