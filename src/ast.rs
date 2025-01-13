use super::token::Token;

pub type Program = Vec<Statement>;

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub token: Token,
    pub name: Token, // Identifier
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub enum Expression {
    IntegerLiteral(Token),
    Identifier(Token),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(Token),
    If(IfExpression),
    Func(FuncLiteral),
    Call(CallExpression),
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub operator: Token,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub operator: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub token: Token, // the { token
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct FuncLiteral {
    pub token: Token,           // the fn token
    pub parameters: Vec<Token>, // Token::Ident("<name>")
    pub body: BlockStatement,
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub token: Token,              // the ( token
    pub function: Box<Expression>, // Identifier or FunctionLiteral
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub enum InfixType {
    Regular,
    Call,
    Noop,
}

#[derive(Debug, Clone)]
pub struct Identifier(String);
