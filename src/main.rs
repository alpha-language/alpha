use lexer::Lexer;
use parser::Parser;

mod errors;
mod helpers;
mod lexer;
mod parser;

fn main() {
  let lexer = Lexer::new(
    "
fn main () {
  let a = 4;
  let b = (5 + a) * 8;
  c = 7;
}
  "
  );
  let mut p = Parser::new(lexer);

  println!("{:#?}", p.parse());
}
