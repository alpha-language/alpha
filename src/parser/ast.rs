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
  In,
  Dot
}

impl Op {
  pub fn priority(&self) -> i8 {
    match self {
      &Self::BineryAnd | &Self::BineryOr | &Self::Dot => 1,

      &Self::Multiply | &Self::Divide | &Self::ModDiv => 2,

      &Self::Add | &Self::Subtract => 3,

      &Self::In
      | &Self::Equals
      | &Self::NotEquals
      | &Self::GreaterThan
      | &Self::GreaterEquals
      | &Self::LessThan
      | &Self::LessEquals
      | &Self::And
      | &Self::Or => 4
    }
  }
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

  Assign(Box<Expr>, Box<Expr>),
  Call(Box<Expr>, VecDeque<Expr>),
  Closure(VecDeque<(String, Box<Expr>)>, VecDeque<Stmt>),
  Block(VecDeque<Stmt>),
  If(VecDeque<(Option<Expr>, VecDeque<Stmt>)>),

  UnaryOp(UnaryOp, Box<Expr>),
  Op(Op, Box<Expr>, Box<Expr>),

  None
}

impl Expr {
  pub fn op_priority(&self) -> i8 {
    match self {
      Self::Op(op, _, _) => op.priority(),
      _ => -1
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
  Return(Expr),
  ExprStmt(Expr),
  Function(String, VecDeque<(String, Box<Expr>)>, VecDeque<Stmt>),
  Declaration(String, Expr),
  While(Expr, VecDeque<Stmt>),
  For(String, Expr, VecDeque<Stmt>),

  None
}
