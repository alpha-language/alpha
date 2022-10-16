use std::collections::VecDeque;

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
  And,
  BineryAnd,
  Or,
  BineryOr,
  Equals,
  NotEquals,
  GreaterThan,
  GreaterEquals,
  LessThan,
  LessEquals,
  Add,
  Subtract,
  Multiply,
  Divide,
  ModDiv,
  In
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
  Call(String, VecDeque<Expr>),
  Closure(VecDeque<(String, String)>, VecDeque<Stmt>),
  Block(VecDeque<Stmt>),
  If(VecDeque<(Option<Expr>, VecDeque<Stmt>)>),

  UnaryOp(UnaryOp, Box<Expr>),
  Op(Op, Box<Expr>, Box<Expr>),

  None
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
  Return(Expr),
  ExprStmt(Expr),
  Function(String, VecDeque<(String, String)>, VecDeque<Stmt>),
  Declaration(String, Expr),
  While(Expr, VecDeque<Stmt>),
  For(String, Expr, VecDeque<Stmt>),

  None
}
