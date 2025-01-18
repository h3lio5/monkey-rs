use super::Lexer;
use crate::token::Token;

#[test]
fn test_lexer_simple_symbols() {
    let mut lexer = Lexer::new("=+(){},[];");
    assert_eq!(lexer.next_token(), Token::Assign);
    assert_eq!(lexer.next_token(), Token::Plus);
    assert_eq!(lexer.next_token(), Token::LParen);
    assert_eq!(lexer.next_token(), Token::RParen);
    assert_eq!(lexer.next_token(), Token::LBrace);
    assert_eq!(lexer.next_token(), Token::RBrace);
    assert_eq!(lexer.next_token(), Token::Comma);
    assert_eq!(lexer.next_token(), Token::LBracket);
    assert_eq!(lexer.next_token(), Token::RBracket);
    assert_eq!(lexer.next_token(), Token::Semicolon);
    assert_eq!(lexer.next_token(), Token::Eof);
}

#[test]
fn test_lexer_let_statements() {
    let mut lexer = Lexer::new(
        "let five = 5;
	let ten = 10;
	let add = fn(x, y) {
	x + y;
	};
	let result = add(five, ten);",
    );
    assert_eq!(lexer.next_token(), Token::Let);
    assert_eq!(lexer.next_token(), Token::Identifier("five".to_owned()));
    assert_eq!(lexer.next_token(), Token::Assign);
    assert_eq!(lexer.next_token(), Token::Int(5));
    assert_eq!(lexer.next_token(), Token::Semicolon);
    assert_eq!(lexer.next_token(), Token::Let);
    assert_eq!(lexer.next_token(), Token::Identifier("ten".to_owned()));
    assert_eq!(lexer.next_token(), Token::Assign);
    assert_eq!(lexer.next_token(), Token::Int(10));
    assert_eq!(lexer.next_token(), Token::Semicolon);
    assert_eq!(lexer.next_token(), Token::Let);
    assert_eq!(lexer.next_token(), Token::Identifier("add".to_owned()));
    assert_eq!(lexer.next_token(), Token::Assign);
    assert_eq!(lexer.next_token(), Token::Func);
    assert_eq!(lexer.next_token(), Token::LParen);
    assert_eq!(lexer.next_token(), Token::Identifier("x".to_owned()));
    assert_eq!(lexer.next_token(), Token::Comma);
    assert_eq!(lexer.next_token(), Token::Identifier("y".to_owned()));
    assert_eq!(lexer.next_token(), Token::RParen);
    assert_eq!(lexer.next_token(), Token::LBrace);
    assert_eq!(lexer.next_token(), Token::Identifier("x".to_owned()));
    assert_eq!(lexer.next_token(), Token::Plus);
    assert_eq!(lexer.next_token(), Token::Identifier("y".to_owned()));
    assert_eq!(lexer.next_token(), Token::Semicolon);
    assert_eq!(lexer.next_token(), Token::RBrace);
    assert_eq!(lexer.next_token(), Token::Semicolon);
    assert_eq!(lexer.next_token(), Token::Let);
    assert_eq!(lexer.next_token(), Token::Identifier("result".to_owned()));
    assert_eq!(lexer.next_token(), Token::Assign);
    assert_eq!(lexer.next_token(), Token::Identifier("add".to_owned()));
    assert_eq!(lexer.next_token(), Token::LParen);
    assert_eq!(lexer.next_token(), Token::Identifier("five".to_owned()));
    assert_eq!(lexer.next_token(), Token::Comma);
    assert_eq!(lexer.next_token(), Token::Identifier("ten".to_owned()));
    assert_eq!(lexer.next_token(), Token::RParen);
    assert_eq!(lexer.next_token(), Token::Semicolon);
    assert_eq!(lexer.next_token(), Token::Eof);
}

#[test]
fn test_lexer_operators() {
    let mut lexer = Lexer::new(
        "!-/*5;
	5 < 10 >= 5;
    1 <= 2;",
    );
    assert_eq!(lexer.next_token(), Token::Bang);
    assert_eq!(lexer.next_token(), Token::Minus);
    assert_eq!(lexer.next_token(), Token::Slash);
    assert_eq!(lexer.next_token(), Token::Asterisk);
    assert_eq!(lexer.next_token(), Token::Int(5));
    assert_eq!(lexer.next_token(), Token::Semicolon);
    assert_eq!(lexer.next_token(), Token::Int(5));
    assert_eq!(lexer.next_token(), Token::LessThan);
    assert_eq!(lexer.next_token(), Token::Int(10));
    assert_eq!(lexer.next_token(), Token::GreaterThanEqual);
    assert_eq!(lexer.next_token(), Token::Int(5));
    assert_eq!(lexer.next_token(), Token::Semicolon);
    assert_eq!(lexer.next_token(), Token::Int(1));
    assert_eq!(lexer.next_token(), Token::LessThanEqual);
    assert_eq!(lexer.next_token(), Token::Int(2));
    assert_eq!(lexer.next_token(), Token::Semicolon);
    assert_eq!(lexer.next_token(), Token::Eof);
}

#[test]
fn test_lexer_if_boolean() {
    let mut lexer = Lexer::new(
        "if (a == 5) {
    1 != 2;
	return true;
	} else {
	return false;
	}",
    );

    assert_eq!(lexer.next_token(), Token::If);
    assert_eq!(lexer.next_token(), Token::LParen);
    assert_eq!(lexer.next_token(), Token::Identifier("a".to_owned()));
    assert_eq!(lexer.next_token(), Token::Equal);
    assert_eq!(lexer.next_token(), Token::Int(5));
    assert_eq!(lexer.next_token(), Token::RParen);
    assert_eq!(lexer.next_token(), Token::LBrace);
    assert_eq!(lexer.next_token(), Token::Int(1));
    assert_eq!(lexer.next_token(), Token::NotEqual);
    assert_eq!(lexer.next_token(), Token::Int(2));
    assert_eq!(lexer.next_token(), Token::Semicolon);
    assert_eq!(lexer.next_token(), Token::Return);
    assert_eq!(lexer.next_token(), Token::True);
    assert_eq!(lexer.next_token(), Token::Semicolon);
    assert_eq!(lexer.next_token(), Token::RBrace);
    assert_eq!(lexer.next_token(), Token::Else);
    assert_eq!(lexer.next_token(), Token::LBrace);
    assert_eq!(lexer.next_token(), Token::Return);
    assert_eq!(lexer.next_token(), Token::False);
    assert_eq!(lexer.next_token(), Token::Semicolon);
    assert_eq!(lexer.next_token(), Token::RBrace);
    assert_eq!(lexer.next_token(), Token::Eof);
}

#[test]
fn test_lexer_string_literal() {
    let mut lexer = Lexer::new("\"Hello World!\"");
    assert_eq!(lexer.next_token(), Token::String("Hello World!".to_owned()));
    assert_eq!(lexer.next_token(), Token::Eof);
}
