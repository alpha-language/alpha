use lexer::Lexer;

mod lexer;

fn main() {
  let l = Lexer::new("
fn main() -> i8 {
  let a = 5
  println(\"Hello World!\", 'p')
  return 7_000_4.7_7
}
  ");

  for token in l {
    println!("{:?}", token);
  }
}
