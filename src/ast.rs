use std::collections::HashMap;

use super::token::Token;

pub type Program = Vec<Statement>;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStatement {
    pub token: Token,
    pub name: Token, // Identifier
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    pub token: Token,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub token: Token,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    IntegerLiteral(Token),
    Identifier(Token),
    StringLiteral(Token),
    Array(ArrayLiteral),
    Hash(HashLiteral),
    Index(IndexExpression),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(Token),
    If(IfExpression),
    Func(FuncLiteral),
    Call(CallExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrefixExpression {
    pub operator: Token,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InfixExpression {
    pub operator: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub token: Token, // the { token
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Box<Expression>,  // array literal or identifier
    pub index: Box<Expression>, // should be integer literal
}

#[derive(Debug, Clone, PartialEq)]
pub struct HashLiteral {
    pub token: Token,
    pub pairs: Vec<(Expression, Expression)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncLiteral {
    pub token: Token,           // the fn token
    pub parameters: Vec<Token>, // Token::Ident("<name>")
    pub body: BlockStatement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub token: Token,              // the ( token
    pub function: Box<Expression>, // Identifier or FunctionLiteral
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InfixType {
    Regular,
    Call,
    Index,
    Noop,
}
