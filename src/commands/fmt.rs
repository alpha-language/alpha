use std::collections::VecDeque;
use std::fs;

use crate::errors::Result;
use crate::lexer::Lexer;
use crate::parser::transpiler::Transpiler;
use crate::parser::{ast, Parser};

struct Formator<'p>(Parser<'p>);

impl<'f> Transpiler for Formator<'f> {
  fn transpile(&mut self) -> Result<'f, String> {
    let statements = self.0.parse()?;
    Ok(self.transpile_statements(&statements))
  }

  fn transpile_statements(&mut self, statements: &VecDeque<ast::Stmt>) -> String {
    let mut result = String::new();

    for statement in statements.iter() {
      result.push_str(self.transpile_statement(statement).as_str())
    }

    result
  }

  fn transpile_statement(&mut self, statement: &ast::Stmt) -> String {
    match statement {
      ast::Stmt::Function(name, args, body) => format!(
        "fn {name}({}){{\n{}}}\n",
        args
          .iter()
          .map(|(arg, r#type)| format!("{}: {}", arg, r#type))
          .collect::<Vec<_>>()
          .join(", "),
        self.transpile_statements(body)
      ),
      ast::Stmt::Declaration(name, expr) =>
        format!("let {name} = {};\n", self.transpile_expression(expr)),
      ast::Stmt::ExprStmt(expr) => self.transpile_expression(expr) + ";",
      ast::Stmt::For(var, list, body) => format!(
        "for {var} in {} {{\n{}\n}}\n",
        self.transpile_expression(list),
        self.transpile_statements(body)
      ),
      ast::Stmt::While(cond, body) => format!(
        "while {} {{\n{}\n}}\n",
        self.transpile_expression(cond),
        self.transpile_statements(body)
      ),
      ast::Stmt::Return(expr) => format!("return {};\n", self.transpile_expression(expr)),
      _ => String::new()
    }
  }

  fn transpile_expression(&mut self, expression: &ast::Expr) -> String {
    match expression {
      ast::Expr::BooleanLiteral(b) => if *b { "true" } else { "false" }.into(),
      ast::Expr::CharLiteral(c) => format!("'{c}'"),
      ast::Expr::StringLiteral(s) => format!("\"{s}\""),
      ast::Expr::IntLiteral(i) => format!("{i}"),
      ast::Expr::FloatLiteral(f) => format!("{f}"),
      ast::Expr::Identifier(name) => format!("{name}"),

      ast::Expr::Assign(name, expr) =>
        format!("{name} = {}", self.transpile_expression(expr.as_ref())),
      ast::Expr::Block(body) => format!("{{\n{}\n}}", self.transpile_statements(body)),
      ast::Expr::Call(func, args) => format!(
        "{func}({})",
        args
          .iter()
          .map(|expr| self.transpile_expression(expr))
          .collect::<Vec<_>>()
          .join(", ")
      ),
      ast::Expr::Closure(args, body) => format!(
        "fn ({}){{\n{}}}\n",
        args
          .iter()
          .map(|(arg, r#type)| format!("{}: {}", arg, r#type))
          .collect::<Vec<_>>()
          .join(", "),
        self.transpile_statements(body)
      ),
      ast::Expr::If(conditions) => {
        let mut conditions = conditions.iter();
        let (cond, body) = conditions.next().unwrap();
        let cond = &cond.clone().unwrap();
        let mut result = format!(
          "if {} {{\n{}\n}}",
          self.transpile_expression(cond),
          self.transpile_statements(body)
        );

        while let Some(otherwise) = conditions.next() {
          result += &if let Some(cond) = &otherwise.0 {
            format!(
              "else if {} {{\n{}\n}}",
              self.transpile_expression(cond),
              self.transpile_statements(&otherwise.1)
            )
          } else {
            format!("else {{\n{}\n}}", self.transpile_statements(&otherwise.1))
          };
        }

        result
      },
      ast::Expr::Op(op, left, right) => format!(
        "{} {} {}",
        self.transpile_expression(left.as_ref()),
        self.transpile_operation(op),
        self.transpile_expression(right.as_ref())
      ),
      ast::Expr::UnaryOp(unaryop, right) => format!(
        "{}{}",
        self.transpile_unary(unaryop),
        self.transpile_expression(right.as_ref())
      ),
      _ => String::new()
    }
  }

  fn transpile_operation(&mut self, operation: &ast::Op) -> String {
    match operation {
      ast::Op::And => "&&".into(),
      ast::Op::BineryAnd => "&".into(),
      ast::Op::Or => "||".into(),
      ast::Op::BineryOr => "|".into(),
      ast::Op::Equals => "==".into(),
      ast::Op::NotEquals => "!=".into(),
      ast::Op::GreaterThan => ">".into(),
      ast::Op::GreaterEquals => ">=".into(),
      ast::Op::LessThan => "<".into(),
      ast::Op::LessEquals => "<=".into(),
      ast::Op::Add => "+".into(),
      ast::Op::Subtract => "-".into(),
      ast::Op::Multiply => "*".into(),
      ast::Op::Divide => "/".into(),
      ast::Op::ModDiv => "%".into(),
      ast::Op::In => "in".into()
    }
  }

  fn transpile_unary(&mut self, unaryop: &ast::UnaryOp) -> String {
    match unaryop {
      ast::UnaryOp::Not => "!".into(),
      ast::UnaryOp::Negate => "-".into()
    }
  }
}

pub fn handler(file: String) {
  let file = fs::read_to_string(file).unwrap();

  let lexer = Lexer::new(&file);
  let mut formator = Formator(Parser::new(lexer));

  println!("{}", formator.transpile().unwrap());
}
