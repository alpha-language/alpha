use std::collections::VecDeque;
use std::fs;

use crate::errors::Result;
use crate::lexer::Lexer;
use crate::parser::transpiler::Transpiler;
use crate::parser::{ast, Parser};

struct Formator<'p> {
  parser: Parser<'p>,
  indent: usize
}

impl<'f> Formator<'f> {
  fn new(parser: Parser<'f>) -> Self {
    Self {
      parser,
      indent: 0
    }
  }
}

impl<'f> Transpiler for Formator<'f> {
  fn transpile(&mut self) -> Result<'f, String> {
    let statements = self.parser.parse();
    Ok(self.transpile_statements(&statements?))
  }

  fn transpile_statements(&mut self, statements: &VecDeque<ast::Stmt>) -> String {
    let mut result = String::new();

    for statement in statements.iter() {
      result = format!(
        "{result}{}{}",
        "\t".repeat(self.indent),
        self.transpile_statement(statement).as_str()
      );
    }

    result
  }

  fn transpile_statement(&mut self, statement: &ast::Stmt) -> String {
    match statement {
      ast::Stmt::Function(name, args, body) => {
        self.indent += 1;
        let r = if body.len() > 0 {
          format!(
            "fn {name}({}) {{\n{}}}\n",
            args
              .iter()
              .map(|(arg, r#type)| format!("{}: {}", arg, r#type))
              .collect::<Vec<_>>()
              .join(", "),
            self.transpile_statements(body)
          )
        } else {
          format!(
            "fn {name}({}) {{}}\n",
            args
              .iter()
              .map(|(arg, r#type)| format!("{}: {}", arg, r#type))
              .collect::<Vec<_>>()
              .join(", "),
          )
        };
        self.indent -= 1;
        r
      },
      ast::Stmt::Declaration(name, expr) =>
        format!("let {name} = {};\n", self.transpile_expression(expr)),
      ast::Stmt::ExprStmt(expr) => format!("{}{}\n", self.transpile_expression(expr), match expr {
        ast::Expr::If(_) | ast::Expr::Block(_) => "",
        _ => ";"
      }),
      ast::Stmt::For(var, list, body) => {
        self.indent += 1;
        let r = format!(
          "for {var} in {} {{\n{}{}}}\n",
          self.transpile_expression(list),
          self.transpile_statements(body),
          "\t".repeat(self.indent - 1)
        );
        self.indent -= 1;
        r
      },
      ast::Stmt::While(cond, body) => {
        self.indent += 1;
        let r = format!(
          "while {} {{\n{}{}}}\n",
          self.transpile_expression(cond),
          self.transpile_statements(body),
          "\t".repeat(self.indent - 1)
        );
        self.indent -= 1;
        r
      },
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
      ast::Expr::Block(body) => {
        self.indent += 1;
        let r = format!(
          "{{\n{}{}}}",
          self.transpile_statements(body),
          "\t".repeat(self.indent - 1)
        );
        self.indent -= 1;
        r
      },
      ast::Expr::Call(func, args) => format!(
        "{func}({})",
        args
          .iter()
          .map(|expr| self.transpile_expression(expr))
          .collect::<Vec<_>>()
          .join(", ")
      ),
      ast::Expr::Closure(args, body) => {
        self.indent += 1;
        let r = if body.len() > 0 {
          format!(
            "fn ({}){{\n{}{}}}\n",
            args
              .iter()
              .map(|(arg, r#type)| format!("{}: {}", arg, r#type))
              .collect::<Vec<_>>()
              .join(", "),
            self.transpile_statements(body),
            "\t".repeat(self.indent - 1)
          )
        } else {
          format!(
            "fn ({}){{}}\n",
            args
              .iter()
              .map(|(arg, r#type)| format!("{}: {}", arg, r#type))
              .collect::<Vec<_>>()
              .join(", "),
          )
        };
        self.indent -= 1;
        r
      },
      ast::Expr::If(conditions) => {
        let mut conditions = conditions.iter();
        let (cond, body) = conditions.next().unwrap();
        let cond = &cond.clone().unwrap();
        self.indent += 1;
        let mut result = format!(
          "if {} {{\n{}{}}}",
          self.transpile_expression(cond),
          self.transpile_statements(body),
          "\t".repeat(self.indent - 1)
        );
        self.indent -= 1;

        while let Some(otherwise) = conditions.next() {
          self.indent += 1;
          result += &if let Some(cond) = &otherwise.0 {
            format!(
              "else if {} {{\n{}{}}}",
              self.transpile_expression(cond),
              self.transpile_statements(&otherwise.1),
              "\t".repeat(self.indent - 1)
            )
          } else {
            format!(
              "else {{\n{}{}}}",
              self.transpile_statements(&otherwise.1),
              "\t".repeat(self.indent - 1)
            )
          };
          self.indent -= 1;
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
  let mut formator = Formator::new(Parser::new(lexer));

  println!("{}", formator.transpile().unwrap());
}
