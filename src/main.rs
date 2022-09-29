use lexer::Lexer;
use parser::Parser;

mod lexer;
mod parser;

fn main() {
  let lexer = Lexer::new(
    "/* */
fn main() -> i8 {
  return 0
}
  "
  );
  let mut p = Parser::new(lexer);

  p.parse();
}
