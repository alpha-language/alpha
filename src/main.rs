use clap::{Parser as CliParser, Subcommand};
use lexer::Lexer;
use parser::Parser;

mod commands;
mod errors;
mod helpers;
mod lexer;
mod parser;

#[derive(CliParser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
  /// Output file
  #[arg(short, long, default_value_t = String::from("a.out"))]
  output: String,

  #[command(subcommand)]
  command: Option<Commands>
}

#[derive(Subcommand, Debug)]
enum Commands {
  /// Formating input file
  Fmt { file: String }
}

fn main() {
  let args = Args::parse();

  match args.command {
    Some(Commands::Fmt {
      file
    }) => commands::fmt::handler(file),
    None => todo!()
  };
}
