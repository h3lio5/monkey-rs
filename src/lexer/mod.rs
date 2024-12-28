use std::{iter::Peekable, str::Chars};

use super::token::Token;

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: &'a str,
    cursor: Peekable<Chars<'a>>,
    line_number: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            cursor: input.chars().peekable(),
            line_number: 0,
        };

        lexer
    }

    pub fn next_token(&mut self) -> Token {

        self.skip_whitespace();

        let ch = self.cursor.next().unwrap_or('\0');
        let token = match ch {
            '=' => self.handle_equality(Token::Assign, Token::Equal),
            '!' => self.handle_equality(Token::Bang, Token::NotEqual),
            '+' => Token::Plus,
            '-' => self.handle_minus_or_negative_number(),
            '/' => Token::Slash,
            '*' => Token::Asterisk,
            '<' => self.handle_equality(Token::LessThan, Token::LessThanEqual),
            '>' => self.handle_equality(Token::GreaterThan, Token::GreaterThanEqual),
            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            c if c.is_ascii_digit() => self.read_number(c, 1),
            c if c.is_ascii_alphabetic() || c == '_' => self.read_identifier(c),
            '\0' => Token::Eof,
            _ => Token::Illegal
            };

        token
    }

    // Utility Functions
    fn skip_whitespace(&mut self) {
        while self.cursor.peek().unwrap().is_ascii_whitespace() {
            self.cursor.next();
        }
    }

    fn handle_equality(&mut self, token1: Token, token2: Token) -> Token {
        if self.cursor.peek() == Some(&'=') {
            token2
        } else {
            token1
        }
    }

    fn read_number(&mut self, first_digit: char, sign: i64) -> Token {
        let mut number = String::from(first_digit);

        while let Some(&ch) = self.cursor.peek() {
            if !ch.is_ascii_digit() {
                break;
            }
            number.push(ch);
            self.cursor.next();
        }

        let n = number.parse::<i64>().expect(format!("Error parsing {} into i64 type", number).as_str());
        Token::Int(sign * n) 
    }

    fn handle_minus_or_negative_number(&mut self) -> Token {
        match self.cursor.peek() {
            Some(&ch) if ch.is_ascii_digit() => {
                let first_digit = self.cursor.next().unwrap(); 
                self.read_number(first_digit, -1)
            }
            _ => Token::Minus,
        }
    }

    fn read_identifier(&mut self, first_letter: char) -> Token {
        let mut identifier = String::from(first_letter);

        while let Some(&ch) = self.cursor.peek() {
            if !ch.is_alphanumeric() {
                break;
            }
            identifier.push(ch);
            self.cursor.next();
        }

        Token::get_keyword_or_identifier(&identifier)
    }

}

