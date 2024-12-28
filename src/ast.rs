use super::token::Token;

pub type Program = Vec<Statement>;

#[derive(Debug, Clone)]
pub enum Statement {
    LetStatement(LetStatementInner),
    ReturnStatement(ReturnStatementInner),
    ExpressionStatement(Expression)
}

#[derive(Debug, Clone)]
pub struct LetStatementInner {
    token: Token,
    value: Expression,
}

#[derive(Debug, Clone)]
pub struct ReturnStatementInner {
    token: Token,
    value: Expression,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatementInner {
    token: Token,
    value: Expression
}

#[derive(Debug, Clone)]
pub enum Expression {
    IntegerLiteral(Token),
    PrefixExpression(PrefixExpressionInner),
    InfixExpression(InfixExpressionInner),
    BooleanLiteral(Token),
    IfExpression(IfExpressionInner),
}

#[derive(Debug, Clone)]
pub struct PrefixExpressionInner {
    token: Token,
    right: Box<Expression>
}

#[derive(Debug, Clone)]
pub struct InfixExpressionInner {
    token: Token,
    left: Box<Expression>,
    right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct IfExpressionInner {
    token: Token,
    condition: Box<Expression>,
    consequence: BlockStatement,
    alternative: Option<BlockStatement>
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    token: Token, // the { token
    statements: Vec<Statement>
}