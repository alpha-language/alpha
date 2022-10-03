use std::collections::VecDeque;

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
  And,
  Or,
  Equals,
  NotEquals,
  GreaterThan,
  GreaterEquals,
  LessThan,
  LessEquals,
  Add,
  Subtract,
  Multiply,
  Divide
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
  Not,
  Negate
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
  Identifier(String),
  FloatLiteral(f64),
  IntLiteral(i64),
  StringLiteral(String),
  CharLiteral(String),
  BooleanLiteral(bool),

  Assign(String, Box<Expr>),
  Call(Box<Expr>, VecDeque<Expr>),
  Closure(VecDeque<(String, String)>, VecDeque<Stmt>),
  Block(VecDeque<Stmt>),

  UnaryOp(UnaryOp, Box<Expr>),
  BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),

  None
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
  Return(Expr),
  ExprStmt(Expr),
  Declaration(String, Expr),
  If(Expr, VecDeque<Stmt>, VecDeque<Stmt>),
  While(Expr, VecDeque<Stmt>),

  None
}
