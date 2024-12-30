use super::token::Token;

pub type Program = Vec<Statement>;

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(Expression),
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub token: Token,
    pub name: Token,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    token: Token,
    value: Expression,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    token: Token,
    value: Expression,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Token),
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
    token: Token,
    right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    token: Token,
    left: Box<Expression>,
    right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    token: Token,
    condition: Box<Expression>,
    consequence: BlockStatement,
    alternative: Option<BlockStatement>,
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    token: Token, // the { token
    statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct FuncLiteral {
    token: Token,           // the fn token
    parameters: Vec<Token>, // Token::Ident("<name>")
    body: BlockStatement,
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    token: Token,              // the ( token
    function: Box<Expression>, // Identifier or FunctionLiteral
    arguments: Vec<Expression>,
}
