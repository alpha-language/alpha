use lexer::Lexer;
use parser::Parser;

mod errors;
mod helpers;
mod lexer;
mod parser;

fn main() {
  let lexer = Lexer::new(
    "/* */
  fn main() {
    return 0;
  }
    "
  );
  let mut p = Parser::new(lexer);

  println!("{:#?}", p.parse());
}
