#[allow(unused_imports)]
use super::token::{Token, TokenKind};
use super::Lexer;

#[allow(dead_code)]
fn lex(source: &str, expected: Vec<Token>) {
  let mut lexer = Lexer::new(source);

  let mut actual_len = 0;
  let expected_len = expected.len();

  for (expected, actual) in expected.into_iter().zip(lexer.by_ref()) {
    actual_len += 1;
    let actual = actual.unwrap();
    assert_eq!(expected, actual);
  }

  assert_eq!(expected_len, actual_len);
  assert_eq!(None, lexer.next());
}

#[test]
fn delimiters() {
  lex("{} [] ()", vec![
    Token::new(TokenKind::OpenBrace, 0, 1),
    Token::new(TokenKind::CloseBrace, 1, 1),
    Token::new(TokenKind::OpenBracket, 3, 1),
    Token::new(TokenKind::CloseBracket, 4, 1),
    Token::new(TokenKind::OpenParen, 6, 1),
    Token::new(TokenKind::CloseParen, 7, 1),
  ]);
}

#[test]
fn operators() {
  lex(", . + - * / = == ! != > >= < <= & && | ||", vec![
    Token::new(TokenKind::Comma, 0, 1),
    Token::new(TokenKind::Dot, 2, 1),
    Token::new(TokenKind::Plus, 4, 1),
    Token::new(TokenKind::Minus, 6, 1),
    Token::new(TokenKind::Star, 8, 1),
    Token::new(TokenKind::Slash, 10, 1),
    Token::new(TokenKind::Equal, 12, 1),
    Token::new(TokenKind::Equals, 14, 2),
    Token::new(TokenKind::Not, 17, 1),
    Token::new(TokenKind::NotEqual, 19, 2),
    Token::new(TokenKind::Greater, 22, 1),
    Token::new(TokenKind::GreaterEqual, 24, 2),
    Token::new(TokenKind::Less, 27, 1),
    Token::new(TokenKind::LessEqual, 29, 2),
    Token::new(TokenKind::Amp, 32, 1),
    Token::new(TokenKind::And, 34, 2),
    Token::new(TokenKind::Pipe, 37, 1),
    Token::new(TokenKind::Or, 39, 2),
  ]);
}

#[test]
fn line_comment() {
  lex("123; // comment\n 123", vec![
    Token::new(TokenKind::Int(123), 0, 3),
    Token::new(TokenKind::Semicolon, 3, 1),
    Token::new(TokenKind::Int(123), 17, 3),
  ]);
}

#[test]
fn string() {
  lex("\"hello, world\"", vec![Token::new(
    TokenKind::String("hello, world"),
    0,
    14
  )]);
}

#[test]
fn integer() {
  lex("123", vec![Token::new(TokenKind::Int(123), 0, 3)]);
}

#[test]
fn decimal() {
  lex("123.45", vec![Token::new(TokenKind::Float(123.45), 0, 6)]);
}

#[test]
fn number_field_access() {
  lex("123.prop", vec![
    Token::new(TokenKind::Int(123), 0, 3),
    Token::new(TokenKind::Dot, 3, 1),
    Token::new(TokenKind::ID("prop"), 4, 4),
  ]);
}

#[test]
fn identifiers() {
  lex("id", vec![Token::new(TokenKind::ID("id"), 0, 2)]);
  lex("_id", vec![Token::new(TokenKind::ID("_id"), 0, 3)]);
  lex("id123", vec![Token::new(TokenKind::ID("id123"), 0, 5)]);
}

#[test]
fn keywords() {
  lex("let", vec![Token::new(TokenKind::Let, 0, 3)]);
  lex("fn", vec![Token::new(TokenKind::Fn, 0, 2)]);
  lex("return", vec![Token::new(TokenKind::Return, 0, 6)]);
  lex("true", vec![Token::new(TokenKind::True, 0, 4)]);
  lex("false", vec![Token::new(TokenKind::False, 0, 5)]);
  lex("if", vec![Token::new(TokenKind::If, 0, 2)]);
  lex("else", vec![Token::new(TokenKind::Else, 0, 4)]);
  lex("while", vec![Token::new(TokenKind::While, 0, 5)]);
  lex("for", vec![Token::new(TokenKind::For, 0, 3)]);
  lex("in", vec![Token::new(TokenKind::In, 0, 2)]);
}
