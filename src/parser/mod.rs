use super::ast::*;
use super::lexer::Lexer;
use super::token::Token;

mod errors;
use errors::ParseError;

#[cfg(test)]
mod test;

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
/// Parser for the monkey programming language
#[derive(Debug, Clone)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    /// Creates a new Parser instance
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Self {
            lexer,
            current_token: Token::Eof,
            peek_token: Token::Eof,
        };
        parser.advance();
        parser.advance();
        parser
    }

    /// Parses the entire program
    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut program = Program::new();
        while !self.is_current_token(Token::Eof) {
            program.push(self.parse_statement()?);
            self.advance();
        }
        Ok(program)
    }

    // Statement Parsing Methods
    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        let token = Token::Let;
        self.expect_next(Token::Identifier(String::new()))?;
        let name = self.current_token.clone();

        self.expect_next(Token::Assign)?;
        self.advance();

        let value = self.parse_expression(Precedence::Lowest)?;
        self.expect_next(Token::Semicolon)?;

        Ok(Statement::Let(LetStatement { token, name, value }))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        let token = self.current_token.clone();
        self.advance();
        let value = self.parse_expression(Precedence::Lowest)?;
        self.expect_next(Token::Semicolon)?;
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

    // Expression Parsing Methods
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        let mut left = self.parse_prefix_expression()?;

        while !self.is_peek_token(Token::Semicolon) && precedence < self.peek_token_precedence() {
            left = match self.has_infix_parse_function(&self.peek_token) {
                InfixType::Regular => {
                    self.advance();
                    self.parse_infix_expression(left)?
                }
                InfixType::Call => {
                    self.advance();
                    self.parse_call_expression(left)?
                }
                InfixType::Noop => break,
            };
        }
        Ok(left)
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParseError> {
        match self.current_token {
            Token::Identifier(_) => self.parse_identifier_expression(),
            Token::Int(_) => self.parse_integer_literal_expression(),
            Token::True | Token::False => self.parse_boolean_literal_expression(),
            Token::If => self.parse_if_expression(),
            Token::Func => self.parse_function_literal_expression(),
            Token::Minus | Token::Bang => {
                let operator = self.current_token.clone();
                self.advance();
                Ok(Expression::Prefix(PrefixExpression {
                    operator,
                    right: Box::new(self.parse_expression(Precedence::Prefix)?),
                }))
            }
            Token::LParen => self.parse_grouped_expression(),
            _ => Err(ParseError::NoPrefixParseFunction {
                token: self.current_token.clone(),
            }),
        }
    }

    fn parse_integer_literal_expression(&self) -> Result<Expression, ParseError> {
        let integer_literal_token = self.current_token.clone();
        Ok(Expression::IntegerLiteral(integer_literal_token))
    }

    fn parse_boolean_literal_expression(&self) -> Result<Expression, ParseError> {
        let boolean_literal_token = self.current_token.clone();
        Ok(Expression::Boolean(boolean_literal_token))
    }

    fn parse_identifier_expression(&self) -> Result<Expression, ParseError> {
        let identifier_token = self.current_token.clone();
        Ok(Expression::Identifier(identifier_token))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        let operator = self.current_token.clone();
        let precedence = self.current_token_precedence();
        self.advance();

        let right = self.parse_expression(precedence)?;

        Ok(Expression::Infix(InfixExpression {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        }))
    }

    fn parse_function_literal_expression(&mut self) -> Result<Expression, ParseError> {
        let token = self.current_token.clone();

        self.expect_next(Token::LParen)?;
        let parameters = self.parse_function_parameters()?;

        self.expect_next(Token::LBrace)?;
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

        self.expect_next(Token::RParen)?;

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

        // Closing brace missing
        self.expect_now(Token::RBrace)?;

        let block_statement = BlockStatement { token, statements };
        Ok(block_statement)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ParseError> {
        let token = self.current_token.clone();
        let arguments = self.parse_call_arguments()?;
        let call_expression = Expression::Call(CallExpression {
            token,
            function: Box::new(function),
            arguments,
        });
        Ok(call_expression)
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut arguments = Vec::new();
        if self.is_peek_token(Token::RParen) {
            self.advance();
            return Ok(arguments);
        }
        self.advance();
        let mut expr = self.parse_expression(Precedence::Lowest)?;
        arguments.push(expr);
        while self.is_peek_token(Token::Comma) {
            self.advance();
            self.advance();
            expr = self.parse_expression(Precedence::Lowest)?;
            arguments.push(expr);
        }

        self.expect_next(Token::RParen)?;

        Ok(arguments)
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParseError> {
        self.advance();
        let expression = self.parse_expression(Precedence::Lowest)?;
        self.expect_next(Token::RParen)?;
        Ok(expression)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParseError> {
        let token = self.current_token.clone();
        self.expect_next(Token::LParen)?;
        self.advance();

        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_next(Token::RParen)?;
        self.expect_next(Token::LBrace)?;
        let consequence = self.parse_block_statement()?;
        let mut alternative = None;

        if self.is_peek_token(Token::Else) {
            self.advance();
            self.expect_next(Token::LBrace)?;
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
    fn advance(&mut self) {
        std::mem::swap(&mut self.current_token, &mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    fn expect_next(&mut self, token: Token) -> Result<(), ParseError> {
        if self.expect_peek(token.clone()) {
            Ok(())
        } else {
            self.raise_token_mismatch_error(token, self.peek_token.clone())
        }
    }

    fn expect_now(&self, token: Token) -> Result<(), ParseError> {
        if self.is_current_token(token.clone()) {
            Ok(())
        } else {
            self.raise_token_mismatch_error(token, self.current_token.clone())
        }
    }

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
        matches!(self.current_token, Token::Identifier(_))
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        let is_match = match token {
            Token::Identifier(_) => matches!(self.peek_token, Token::Identifier(_)),
            _ => self.peek_token == token,
        };
        if is_match {
            self.advance();
            true
        } else {
            false
        }
    }

    fn raise_token_mismatch_error(&self, expected: Token, found: Token) -> Result<(), ParseError> {
        Err(ParseError::MismatchedToken { expected, found })
    }

    // Token Precedence Methods
    fn token_precedence(&self, token: &Token) -> Precedence {
        match token {
            Token::Equal | Token::NotEqual => Precedence::Equals,
            Token::LessThan
            | Token::LessThanEqual
            | Token::GreaterThan
            | Token::GreaterThanEqual => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            Token::LParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    #[inline]
    fn peek_token_precedence(&self) -> Precedence {
        self.token_precedence(&self.peek_token)
    }

    #[inline]
    fn current_token_precedence(&self) -> Precedence {
        self.token_precedence(&self.current_token)
    }

    fn has_infix_parse_function(&self, token: &Token) -> InfixType {
        match token {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::Equal
            | Token::NotEqual
            | Token::GreaterThan
            | Token::GreaterThanEqual
            | Token::LessThan
            | Token::LessThanEqual => InfixType::Regular,
            Token::LParen => InfixType::Call,
            _ => InfixType::Noop,
        }
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
}
