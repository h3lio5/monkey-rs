use super::ast::*;
use super::lexer::Lexer;
use super::token::Token;

mod errors;
use errors::ParseError;

// Operator Precedence
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // <, <=, >, >=
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // my_function(X)
}

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        let mut parser = Parser {
            lexer,
            current_token: Token::Eof,
            peek_token: Token::Eof,
        };

        parser.advance();
        parser.advance();

        parser
    }

    fn advance(&mut self) {
        self.current_token = self.peek_token.take();
        std::mem::swap(&mut self.current_token, &mut self.peek_token);
        self.peek_token = Some(self.lexer.next_token());
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program: Program;

        while !self.is_current_token(Token::Eof) {}

        todo!()
    }

    fn parse_statement(&mut self) -> Result<Statement, errors::ParseError> {
        match self.current_token {
            Some(Token::Let) => self.parse_let_statement(),
            Some(Token::Return) => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, errors::ParseError> {
        let token = Token::Let;

        if !self.expect_peek(&Token::Identifier("".to_string())) {
            return Err(ParseError::MismatchedToken {
                expected: Token::Identifier("(..)".to_string()),
                found: self.peek_token.clone().unwrap_or(Token::Illegal),
            });
        }

        let name = self.current_token.clone().unwrap();

        if !self.expect_peek(&Token::Assign) {
            return Err(ParseError::MismatchedToken {
                expected: Token::Assign,
                found: self.peek_token.clone().unwrap_or(Token::Illegal),
            });
        }

        self.advance();

        let value = self.parse_expression(0)?;

        if !self.expect_peek(&Token::Semicolon) {
            return Err(ParseError::MismatchedToken {
                expected: Token::Semicolon,
                found: self.peek_token.clone().unwrap_or(Token::Illegal),
            });
        }

        let let_statement = LetStatement { token, name, value };

        Ok(Statement::Let(let_statement))
    }

    ///////////////////////// Parse Expression Functions /////////////////////////

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        // parse the prefix
        let left = match self.current_token.as_ref().unwrap() {
            Token::Identifier(_) => self.parse_identifier_expression()?,
            Token::Int(_) => self.parse_integer_literal_expression()?,
            Token::True => self.parse_boolean_literal_expression()?,
            Token::False => self.parse_boolean_literal_expression()?,
            Token::If => self.parse_if_expression()?,
            Token::Func => self.parse_function_literal_expression()?,
            Token::Minus | Token::Bang => self.parse_prefix_expression()?,
            Token::LParen => self.parse_grouped_expression()?,
            _ => {
                return Err(ParseError::NoPrefixParseFunction {
                    token: self.current_token.clone().unwrap(),
                })
            }
        };

        while !self.is_peek_token(&Token::Semicolon) && precedence < self.peek_token_precedence() {}
        todo!()
    }

    fn parse_integer_literal_expression(&self) -> Result<Expression, ParseError> {
        let integer_literal_token = self
            .current_token
            .as_ref()
            .ok_or_else(|| ParseError::ParseExpressionError {
                expression: String::from("Integer Literal"),
            })?
            .clone();
        Ok(Expression::Literal(integer_literal_token))
    }

    fn parse_boolean_literal_expression(&self) -> Result<Expression, ParseError> {
        let boolean_literal_token = self
            .current_token
            .as_ref()
            .ok_or_else(|| ParseError::ParseExpressionError {
                expression: String::from("Boolean Literal"),
            })?
            .clone();
        Ok(Expression::Boolean(boolean_literal_token))
    }

    fn parse_identifier_expression(&self) -> Result<Expression, ParseError> {
        let identifier_token = self
            .current_token
            .as_ref()
            .ok_or_else(|| ParseError::ParseExpressionError {
                expression: String::from("Identifier Expression"),
            })?
            .clone();
        Ok(Expression::Identifier(identifier_token))
    }
    ///////////////////////// Utilities /////////////////////////
    fn is_current_token(&self, token: &Token) -> bool {
        self.current_token.as_ref() == Some(token)
    }

    fn is_peek_token(&self, token: &Token) -> bool {
        self.peek_token.as_ref() == Some(token)
    }

    fn expect_peek(&mut self, token: &Token) -> bool {
        if self.is_peek_token(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn peek_token_precedence(&self) -> Precedence {
        match self.peek_token.as_ref().unwrap() {
            Token::Equal => Precedence::Equals,
            Token::NotEqual => Precedence::Equals,
            Token::LessThan => Precedence::LessGreater,
            Token::LessThanEqual => Precedence::LessGreater,
            Token::GreaterThan => Precedence::LessGreater,
            Token::GreaterThanEqual => Precedence::LessGreater,
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Slash => Precedence::Product,
            Token::Asterisk => Precedence::Product,
            Token::LParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}
