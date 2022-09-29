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
  Nil,

  Identifier(String),
  FloatLiteral(f64),
  IntLiteral(i64),
  StringLiteral(String),
  CharLiteral(String),
  BooleanLiteral(bool),

  Assign(String, Box<Expr>),
  Function(Vec<String>, Vec<Stmt>),
  Call(Box<Expr>, Vec<Expr>),

  UnaryOp(UnaryOp, Box<Expr>),
  BinaryOp(BinaryOp, Box<Expr>, Box<Expr>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
  Return(Expr),
  ExprStmt(Expr),
  Declaration(String, Expr),
  If(Expr, Vec<Stmt>, Vec<Stmt>),
  While(Expr, Vec<Stmt>),
  Block(Vec<Stmt>)
}
