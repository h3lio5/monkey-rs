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

    pub fn parse_program(&mut self) -> Result<Program, errors::ParseError> {
        let mut program = Program::new();

        while !self.is_current_token(Token::Eof) {
            let statement = self.parse_statement()?;
            program.push(statement);
            self.advance();
        }

        Ok(program)
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

        if !self.expect_peek(Token::Identifier(String::new())) {
            self.raise_peek_token_mismatch_error(Token::Identifier(String::from("(..)")))?
        }

        let name = self.current_token.clone();

        if !self.expect_peek(Token::Assign) {
            self.raise_peek_token_mismatch_error(Token::Assign)?
        }

        self.advance();

        let value = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(Token::Semicolon) {
            self.raise_peek_token_mismatch_error(Token::Semicolon)?
        }

        let let_statement = LetStatement { token, name, value };

        Ok(Statement::Let(let_statement))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        let token = self.current_token.clone();
        self.advance();

        let value = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(Token::Semicolon) {
            self.raise_peek_token_mismatch_error(Token::Semicolon)?
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
            Token::True | Token::False => self.parse_boolean_literal_expression()?,
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
            match self.has_infix_parse_function(&self.peek_token) {
                InfixType::Regular => {
                    self.advance();
                    left = self.parse_infix_expression(left)?;
                }
                InfixType::Call => {
                    self.advance();
                    left = self.parse_call_expression(left)?;
                }
                InfixType::Noop => return Ok(left),
            }
        }
        Ok(left)
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
        let precedence = self.current_token_precedence();
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
            self.raise_peek_token_mismatch_error(Token::LParen)?
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(Token::LBrace) {
            self.raise_peek_token_mismatch_error(Token::LBrace)?
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
            self.raise_peek_token_mismatch_error(Token::RParen)?
        }

        Ok(identifiers)
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParseError> {
        let mut statements = Vec::new();
        let token = self.current_token.clone();

        self.advance();

        while !self.is_current_token(Token::RBrace) && !self.is_current_token(Token::Eof) {
            // DEBUG
            // println!("[parse_block_statement]: {:?}", self.current_token.clone());
            let stmt = self.parse_statement()?;
            // println!("[parse_block_statement]: After parsing statement {:?}", stmt);
            // println!("[parse_block_statement]: current_token after parsing {:?}", self.current_token.clone());
            statements.push(stmt);
            self.advance();
        }

        // Closing brace missing
        if self.is_current_token(Token::Eof) {
            self.raise_token_mismatch_error(Token::RBrace, Token::Eof)?;
        }

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

        if self.expect_peek(Token::RParen) {
            self.raise_peek_token_mismatch_error(Token::RParen)?
        }

        Ok(arguments)
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParseError> {
        self.advance();
        let expression = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(Token::RParen) {
            self.raise_peek_token_mismatch_error(Token::RParen)?
        }
        Ok(expression)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParseError> {
        let token = self.current_token.clone();
        if !self.expect_peek(Token::LParen) {
            self.raise_peek_token_mismatch_error(Token::LParen)?
        }
        self.advance();

        let condition = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(Token::RParen) {
            self.raise_peek_token_mismatch_error(Token::RParen)?
        }
        if !self.expect_peek(Token::LBrace) {
            self.raise_peek_token_mismatch_error(Token::RBrace)?
        }
        let consequence = self.parse_block_statement()?;
        let mut alternative = None;

        if self.is_peek_token(Token::Else) {
            self.advance();
            if !self.expect_peek(Token::LBrace) {
                self.raise_peek_token_mismatch_error(Token::LBrace)?
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
        match token {
            Token::Identifier(_) => {
                if let Token::Identifier(_) = self.peek_token {
                    self.advance();
                    true
                } else {
                    false
                }
            }
            _ => self
                .is_peek_token(token)
                .then(|| {
                    self.advance();
                })
                .is_some(),
        }
    }

    #[inline]
    fn raise_peek_token_mismatch_error(&self, expected: Token) -> Result<(), ParseError> {
        self.raise_token_mismatch_error(expected, self.peek_token.clone())
    }

    #[inline]
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

    fn token_precedence(&self, token: &Token) -> Precedence {
        match token {
            Token::Equal | Token::NotEqual=> Precedence::Equals,
            Token::LessThan | Token::LessThanEqual => Precedence::LessGreater,
            Token::GreaterThan | Token::GreaterThanEqual => Precedence::LessGreater,
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

    #[inline]
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
}
