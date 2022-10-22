use std::collections::VecDeque;

use crate::lexer::Lexer;
use crate::parser::Parser;

#[allow(unused_imports)]
use super::ast::{Expr, Op, Stmt, UnaryOp};

#[allow(dead_code)]
fn parse(input: &str, expected: Vec<Stmt>) {
  let lexer = Lexer::new(input);
  let mut parser = Parser::new(lexer);
  assert_eq!(VecDeque::from(expected), parser.parse().unwrap());
}

#[test]
fn literals() {
  parse("123;", vec![Stmt::ExprStmt(Expr::IntLiteral(123))]);
  parse("123.45;", vec![Stmt::ExprStmt(Expr::FloatLiteral(123.45))]);
  parse("'p';", vec![Stmt::ExprStmt(Expr::CharLiteral(
    "p".to_string()
  ))]);
  parse("\"string\";", vec![Stmt::ExprStmt(Expr::StringLiteral(
    "string".to_string()
  ))]);
}

#[test]
fn identifiers() {
  parse("id;", vec![Stmt::ExprStmt(Expr::Identifier(
    "id".to_string()
  ))]);
  parse("_id;", vec![Stmt::ExprStmt(Expr::Identifier(
    "_id".to_string()
  ))]);
  parse("id123;", vec![Stmt::ExprStmt(Expr::Identifier(
    "id123".to_string()
  ))]);
}

#[test]
fn assignment() {
  parse("x = 10;", vec![Stmt::ExprStmt(Expr::Assign(
    "x".to_string(),
    Box::new(Expr::IntLiteral(10))
  ))]);

  parse("x = y = 10;", vec![Stmt::ExprStmt(Expr::Assign(
    "x".to_string(),
    Box::new(Expr::Assign(
      "y".to_string(),
      Box::new(Expr::IntLiteral(10))
    ))
  ))]);
}

#[test]
fn logic() {
  parse("1 && 1;", vec![Stmt::ExprStmt(Expr::Op(
    Op::And,
    Box::new(Expr::IntLiteral(1)),
    Box::new(Expr::IntLiteral(1))
  ))]);

  parse("1 || 1;", vec![Stmt::ExprStmt(Expr::Op(
    Op::Or,
    Box::new(Expr::IntLiteral(1)),
    Box::new(Expr::IntLiteral(1))
  ))]);
}

#[test]
fn equality() {
  parse("1 == 1;", vec![Stmt::ExprStmt(Expr::Op(
    Op::Equals,
    Box::new(Expr::IntLiteral(1)),
    Box::new(Expr::IntLiteral(1))
  ))]);

  parse("1 != 1;", vec![Stmt::ExprStmt(Expr::Op(
    Op::NotEquals,
    Box::new(Expr::IntLiteral(1)),
    Box::new(Expr::IntLiteral(1))
  ))]);
}

#[test]
fn comparison() {
  parse("10 > 5;", vec![Stmt::ExprStmt(Expr::Op(
    Op::GreaterThan,
    Box::new(Expr::IntLiteral(10)),
    Box::new(Expr::IntLiteral(5))
  ))]);

  parse("10 >= 5;", vec![Stmt::ExprStmt(Expr::Op(
    Op::GreaterEquals,
    Box::new(Expr::IntLiteral(10)),
    Box::new(Expr::IntLiteral(5))
  ))]);

  parse("10 < 5;", vec![Stmt::ExprStmt(Expr::Op(
    Op::LessThan,
    Box::new(Expr::IntLiteral(10)),
    Box::new(Expr::IntLiteral(5))
  ))]);

  parse("10 <= 5;", vec![Stmt::ExprStmt(Expr::Op(
    Op::LessEquals,
    Box::new(Expr::IntLiteral(10)),
    Box::new(Expr::IntLiteral(5))
  ))]);
}

#[test]
fn addition() {
  parse("1 + 1;", vec![Stmt::ExprStmt(Expr::Op(
    Op::Add,
    Box::new(Expr::IntLiteral(1)),
    Box::new(Expr::IntLiteral(1))
  ))]);
}

#[test]
fn subtraction() {
  parse("1 - 1;", vec![Stmt::ExprStmt(Expr::Op(
    Op::Subtract,
    Box::new(Expr::IntLiteral(1)),
    Box::new(Expr::IntLiteral(1))
  ))]);
}

#[test]
fn multiplication() {
  parse("1 * 1;", vec![Stmt::ExprStmt(Expr::Op(
    Op::Multiply,
    Box::new(Expr::IntLiteral(1)),
    Box::new(Expr::IntLiteral(1))
  ))]);
}

#[test]
fn division() {
  parse("1 / 1;", vec![Stmt::ExprStmt(Expr::Op(
    Op::Divide,
    Box::new(Expr::IntLiteral(1)),
    Box::new(Expr::IntLiteral(1))
  ))]);

  parse("1 % 1;", vec![Stmt::ExprStmt(Expr::Op(
    Op::ModDiv,
    Box::new(Expr::IntLiteral(1)),
    Box::new(Expr::IntLiteral(1))
  ))]);
}

#[test]
fn unary() {
  parse("!1;", vec![Stmt::ExprStmt(Expr::UnaryOp(
    UnaryOp::Not,
    Box::new(Expr::IntLiteral(1))
  ))]);

  parse("-1;", vec![Stmt::ExprStmt(Expr::UnaryOp(
    UnaryOp::Negate,
    Box::new(Expr::IntLiteral(1))
  ))]);

  parse("-(1);", vec![Stmt::ExprStmt(Expr::UnaryOp(
    UnaryOp::Negate,
    Box::new(Expr::IntLiteral(1))
  ))]);
}

#[test]
fn precedence() {
  parse("x = -1 * 2 + 3 > 4 != 5;", vec![Stmt::ExprStmt(
    Expr::Assign(
      "x".to_string(),
      Box::new(Expr::Op(
        Op::NotEquals,
        Box::new(Expr::Op(
          Op::GreaterThan,
          Box::new(Expr::Op(
            Op::Add,
            Box::new(Expr::Op(
              Op::Multiply,
              Box::new(Expr::UnaryOp(
                UnaryOp::Negate,
                Box::new(Expr::IntLiteral(1))
              )),
              Box::new(Expr::IntLiteral(2))
            )),
            Box::new(Expr::IntLiteral(3))
          )),
          Box::new(Expr::IntLiteral(4))
        )),
        Box::new(Expr::IntLiteral(5))
      ))
    )
  )]);

  parse("1 && 1 || 1;", vec![Stmt::ExprStmt(Expr::Op(
    Op::Or,
    Box::new(Expr::Op(
      Op::And,
      Box::new(Expr::IntLiteral(1)),
      Box::new(Expr::IntLiteral(1))
    )),
    Box::new(Expr::IntLiteral(1))
  ))]);
}

#[test]
fn grouping() {
  parse("2 * (1 + 2) * 3;", vec![Stmt::ExprStmt(Expr::Op(
    Op::Multiply,
    Box::new(Expr::Op(
      Op::Multiply,
      Box::new(Expr::IntLiteral(2)),
      Box::new(Expr::Op(
        Op::Add,
        Box::new(Expr::IntLiteral(1)),
        Box::new(Expr::IntLiteral(2))
      ))
    )),
    Box::new(Expr::IntLiteral(3))
  ))]);
}

#[test]
fn call() {
  parse("a();", vec![Stmt::ExprStmt(Expr::Call(
    "a".to_string(),
    VecDeque::new()
  ))]);

  parse("a(1);", vec![Stmt::ExprStmt(Expr::Call(
    "a".to_string(),
    VecDeque::from([Expr::IntLiteral(1)])
  ))]);

  parse("a(1, 1,);", vec![Stmt::ExprStmt(Expr::Call(
    "a".to_string(),
    VecDeque::from([Expr::IntLiteral(1), Expr::IntLiteral(1)])
  ))]);
}

#[test]
fn declaration() {
  parse("let x = 10;", vec![Stmt::Declaration(
    "x".to_string(),
    Expr::IntLiteral(10)
  )]);

  parse("let x = {10;};", vec![Stmt::Declaration(
    "x".to_string(),
    Expr::Block(VecDeque::from([Stmt::ExprStmt(Expr::IntLiteral(10))]))
  )]);

  parse("let x = if true { 1 + 1; };", vec![Stmt::Declaration(
    "x".to_string(),
    Expr::If(VecDeque::from([(
      Some(Expr::BooleanLiteral(true)),
      VecDeque::from([Stmt::ExprStmt(Expr::Op(
        Op::Add,
        Box::new(Expr::IntLiteral(1)),
        Box::new(Expr::IntLiteral(1))
      ))])
    )]))
  )]);
}

#[test]
fn function_declaration_and_return() {
  parse("fn test(a: i8) {\nreturn a;\n}", vec![Stmt::Function(
    "test".to_string(),
    VecDeque::from([("a".to_string(), "i8".to_string())]),
    VecDeque::from([Stmt::Return(Expr::Identifier("a".to_string()))])
  )]);

  parse("fn test(a: i8, b: i8,) {\nreturn a + b;\n}", vec![
    Stmt::Function(
      "test".to_string(),
      VecDeque::from([
        ("a".to_string(), "i8".to_string()),
        ("b".to_string(), "i8".to_string())
      ]),
      VecDeque::from([Stmt::Return(Expr::Op(
        Op::Add,
        Box::new(Expr::Identifier("a".to_string())),
        Box::new(Expr::Identifier("b".to_string()))
      ))])
    ),
  ]);
}

#[test]
fn closure() {
  parse("let main = fn (a: i8) {\nreturn a;\n};", vec![
    Stmt::Declaration(
      "main".to_string(),
      Expr::Closure(
        VecDeque::from([("a".to_string(), "i8".to_string())]),
        VecDeque::from([Stmt::Return(Expr::Identifier("a".to_string()))])
      )
    ),
  ])
}

#[test]
fn if_parse() {
  parse("if true { 1 + 1; };", vec![Stmt::ExprStmt(Expr::If(
    VecDeque::from([(
      Some(Expr::BooleanLiteral(true)),
      VecDeque::from([Stmt::ExprStmt(Expr::Op(
        Op::Add,
        Box::new(Expr::IntLiteral(1)),
        Box::new(Expr::IntLiteral(1))
      ))])
    )])
  ))]);

  parse("if true { 1 + 1; } else if true { 1 + 1; }", vec![
    Stmt::ExprStmt(Expr::If(VecDeque::from([
      (
        Some(Expr::BooleanLiteral(true)),
        VecDeque::from([Stmt::ExprStmt(Expr::Op(
          Op::Add,
          Box::new(Expr::IntLiteral(1)),
          Box::new(Expr::IntLiteral(1))
        ))])
      ),
      (
        Some(Expr::BooleanLiteral(true)),
        VecDeque::from([Stmt::ExprStmt(Expr::Op(
          Op::Add,
          Box::new(Expr::IntLiteral(1)),
          Box::new(Expr::IntLiteral(1))
        ))])
      )
    ]))),
  ]);

  parse("if true { 1 + 1; } else { 1 + 1; }", vec![Stmt::ExprStmt(
    Expr::If(VecDeque::from([
      (
        Some(Expr::BooleanLiteral(true)),
        VecDeque::from([Stmt::ExprStmt(Expr::Op(
          Op::Add,
          Box::new(Expr::IntLiteral(1)),
          Box::new(Expr::IntLiteral(1))
        ))])
      ),
      (
        None,
        VecDeque::from([Stmt::ExprStmt(Expr::Op(
          Op::Add,
          Box::new(Expr::IntLiteral(1)),
          Box::new(Expr::IntLiteral(1))
        ))])
      )
    ]))
  )]);
}

#[test]
fn loops_parse() {
  parse("while true { 1 + 1; }", vec![Stmt::While(
    Expr::BooleanLiteral(true),
    VecDeque::from([Stmt::ExprStmt(Expr::Op(
      Op::Add,
      Box::new(Expr::IntLiteral(1)),
      Box::new(Expr::IntLiteral(1))
    ))])
  )]);

  parse("for i in 1 { 1 + 1; }", vec![Stmt::For(
    "i".to_string(),
    Expr::IntLiteral(1),
    VecDeque::from([Stmt::ExprStmt(Expr::Op(
      Op::Add,
      Box::new(Expr::IntLiteral(1)),
      Box::new(Expr::IntLiteral(1))
    ))])
  )]);
}

#[test]
fn block_parse() {
  parse("{ 1 + 1; return 123; }", vec![Stmt::ExprStmt(Expr::Block(
    VecDeque::from([
      Stmt::ExprStmt(Expr::Op(
        Op::Add,
        Box::new(Expr::IntLiteral(1)),
        Box::new(Expr::IntLiteral(1))
      )),
      Stmt::Return(Expr::IntLiteral(123))
    ])
  ))])
}

#[test]
fn program() {
  parse("\nlet x = 1;\nlet y = 2;\n\nlet z = 3;\n", vec![
    Stmt::Declaration("x".to_string(), Expr::IntLiteral(1)),
    Stmt::Declaration("y".to_string(), Expr::IntLiteral(2)),
    Stmt::Declaration("z".to_string(), Expr::IntLiteral(3)),
  ]);
}
