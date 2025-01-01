use std::process::id;

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
        std::mem::swap(&mut self.current_token, &mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program: Program;

        while !self.is_current_token(Token::Eof) {}

        todo!()
    }

    fn parse_statement(&mut self) -> Result<Statement, errors::ParseError> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, errors::ParseError> {
        let token = Token::Let;

        if !self.expect_peek(Token::Identifier("".to_string())) {
            return Err(ParseError::MismatchedToken {
                expected: Token::Identifier("(..)".to_string()),
                found: self.peek_token.clone(),
            });
        }

        let name = self.current_token.clone();

        if !self.expect_peek(Token::Assign) {
            return Err(ParseError::MismatchedToken {
                expected: Token::Assign,
                found: self.peek_token.clone(),
            });
        }

        self.advance();

        let value = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(Token::Semicolon) {
            return Err(ParseError::MismatchedToken {
                expected: Token::Semicolon,
                found: self.peek_token.clone(),
            });
        }

        let let_statement = LetStatement { token, name, value };

        Ok(Statement::Let(let_statement))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        let token = self.current_token.clone();
        self.advance();

        let value = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(Token::Semicolon) {
            self.raise_token_mismatch_error(Token::Semicolon, self.peek_token.clone())?
        }
        Ok(Statement::Return(ReturnStatement { token, value }))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let token = self.current_token.clone();
        let value = self.parse_expression(Precedence::Lowest)?;
        if self.is_peek_token(Token::Semicolon) {
            self.advance();
        }
        Ok(Statement::Expression(ExpressionStatement { token, value }))
    }

    ///////////////////////// Parse Expression Functions /////////////////////////

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        // parse the prefix
        let mut left = match self.current_token {
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
                    token: self.current_token.clone(),
                })
            }
        };

        while !self.is_peek_token(Token::Semicolon) && precedence < self.peek_token_precedence() {
            if !self.has_infix_parse_function(&self.peek_token) {
                return Ok(left);
            }
            self.advance();
            left = self.parse_infix_expression(left)?;
        }
        return Ok(left);
    }

    fn parse_integer_literal_expression(&self) -> Result<Expression, ParseError> {
        let integer_literal_token = self.current_token.clone();
        Ok(Expression::Literal(integer_literal_token))
    }

    fn parse_boolean_literal_expression(&self) -> Result<Expression, ParseError> {
        let boolean_literal_token = self.current_token.clone();
        Ok(Expression::Boolean(boolean_literal_token))
    }

    fn parse_identifier_expression(&self) -> Result<Expression, ParseError> {
        let identifier_token = self.current_token.clone();
        Ok(Expression::Identifier(identifier_token))
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParseError> {
        let operator = self.current_token.clone();
        self.advance();
        Ok(Expression::Prefix(PrefixExpression {
            operator,
            right: Box::new(self.parse_expression(Precedence::Prefix)?),
        }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        let operator = self.current_token.clone();
        let precedence = self.peek_token_precedence();
        self.advance();
        Ok(Expression::Infix(InfixExpression {
            operator,
            left: Box::new(left),
            right: Box::new(self.parse_expression(precedence)?),
        }))
    }

    fn parse_function_literal_expression(&mut self) -> Result<Expression, ParseError> {
        let token = self.current_token.clone();

        if !self.expect_peek(Token::LParen) {
            self.raise_token_mismatch_error(Token::LParen, self.peek_token.clone())?
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(Token::LBrace) {
            self.raise_token_mismatch_error(Token::LBrace, self.peek_token.clone())?
        }

        let body = self.parse_block_statement()?;

        let function_literal = Expression::Func(FuncLiteral {
            token,
            parameters,
            body,
        });

        Ok(function_literal)
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Token>, ParseError> {
        let mut identifiers = Vec::new();
        if self.is_peek_token(Token::RParen) {
            self.advance();
            return Ok(identifiers);
        }

        self.advance();
        self.collect_identifier(&mut identifiers)?;

        while self.is_peek_token(Token::Comma) {
            self.advance();
            self.advance();
            self.collect_identifier(&mut identifiers)?;
        }

        if !self.expect_peek(Token::RParen) {
            self.raise_token_mismatch_error(Token::RParen, self.peek_token.clone())?
        }

        Ok(identifiers)
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParseError> {
        let mut statements = Vec::new();
        let token = self.current_token.clone();

        self.advance();

        while !self.is_current_token(Token::RBrace) && !self.is_current_token(Token::Eof) {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.advance();
        }

        let block_statement = BlockStatement { token, statements };
        Ok(block_statement)
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParseError> {
        self.advance();
        let expression = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(Token::RParen) {
            self.raise_token_mismatch_error(Token::RParen, self.peek_token.clone())?
        }
        Ok(expression)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParseError> {
        let token = self.current_token.clone();
        if !self.expect_peek(Token::LParen) {
            self.raise_token_mismatch_error(Token::LParen, self.peek_token.clone())?
        }
        self.advance();

        let condition = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(Token::RParen) {
            self.raise_token_mismatch_error(Token::RParen, self.peek_token.clone())?
        }
        if !self.expect_peek(Token::LBrace) {
            self.raise_token_mismatch_error(Token::RBrace, self.peek_token.clone())?
        }
        let consequence = self.parse_block_statement()?;
        let mut alternative = None;

        if self.is_peek_token(Token::Else) {
            self.advance();
            if !self.expect_peek(Token::LBrace) {
                self.raise_token_mismatch_error(Token::LBrace, self.peek_token.clone())?
            }
            alternative = Some(self.parse_block_statement()?);
        }
        let if_expression = Expression::If(IfExpression {
            token,
            condition: Box::new(condition),
            consequence,
            alternative,
        });
        Ok(if_expression)
    }

    ///////////////////////// Utilities /////////////////////////
    #[inline]
    fn is_current_token(&self, token: Token) -> bool {
        self.current_token == token
    }

    #[inline]
    fn is_peek_token(&self, token: Token) -> bool {
        self.peek_token == token
    }

    #[inline]
    fn is_ident_token(&self) -> bool {
        match self.current_token {
            Token::Identifier(_) => true,
            _ => false,
        }
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if self.is_peek_token(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn raise_token_mismatch_error(&self, expected: Token, found: Token) -> Result<(), ParseError> {
        Err(ParseError::MismatchedToken { expected, found })
    }

    fn collect_identifier(&mut self, identifiers: &mut Vec<Token>) -> Result<(), ParseError> {
        if !self.is_ident_token() {
            return Err(ParseError::MismatchedToken {
                expected: Token::Identifier(String::from("<IDENT>")),
                found: self.current_token.clone(),
            });
        }
        identifiers.push(self.current_token.clone());
        Ok(())
    }

    fn peek_token_precedence(&self) -> Precedence {
        match self.peek_token {
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

    #[inline]
    fn has_infix_parse_function(&self, token: &Token) -> bool {
        matches!(
            token,
            Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::Equal
                | Token::NotEqual
                | Token::GreaterThan
                | Token::GreaterThanEqual
                | Token::LessThan
                | Token::LessThanEqual
                | Token::LParen
        )
    }
}
