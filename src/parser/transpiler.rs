use std::collections::VecDeque;

use crate::errors::Result;

use super::ast;

pub trait Transpiler {
  fn transpile(&mut self) -> Result<'_, String>;
  fn transpile_statements(&mut self, statements: &VecDeque<ast::Stmt>) -> String;
  fn transpile_statement(&mut self, statement: &ast::Stmt) -> String;
  fn transpile_expression(&mut self, expression: &ast::Expr) -> String;
  fn transpile_operation(&mut self, operation: &ast::Op) -> String;
  fn transpile_unary(&mut self, unaryop: &ast::UnaryOp) -> String;
}
