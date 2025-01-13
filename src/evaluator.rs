use std::cell::RefCell;
use std::rc::Rc;
use std::mem::discriminant;

use crate::token::Token;

use super::ast::*;
use super::object::{environment::Environment, FuncObject, Object};

#[derive(Debug, Clone)]
pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

impl Evaluator {
    fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    fn eval_program(self, program: Program) -> Result<Option<Object>, String> {
        let mut result = None;
        let mut env = Rc::new(RefCell::new(Environment::new()));

        for statement in program {
            result = self.eval_statement(statement)?;
        }

        Ok(result)
    }

    fn eval_statement(&self, statement: Statement) -> Result<Option<Object>, String> {
        match statement {
            Statement::Let(LetStatement {
                token: _,
                name,
                value,
            }) => {
                let val = self.eval_expression(value)?;

                let Token::Identifier(ident) = name else {
                    unreachable!()
                };
                self.env.borrow_mut().set(&ident, val);
                Ok(None)
            }
            Statement::Return(ReturnStatement { token: _, value }) => {
                let val = self.eval_expression(value)?;
                Ok(Some(Object::Return(Box::new(val))))
            }
            Statement::Expression(ExpressionStatement { token: _, value }) => {
                let val = self.eval_expression(value)?;
                Ok(Some(val))
            }
        }
    }

    fn eval_expression(&self, expression: Expression) -> Result<Object, String> {
        match expression {
            Expression::Boolean(token) => self.eval_boolean(token),
            Expression::IntegerLiteral(token) => self.eval_integer(token),
            Expression::Identifier(token) => self.eval_identifier(token),
            Expression::Prefix(PrefixExpression { operator, right }) => {
                self.eval_prefix_expression(operator, right)
            }
            Expression::Infix(InfixExpression {
                operator,
                left,
                right,
            }) => self.eval_infix_expression(operator, left, right),
            Expression::If(IfExpression {
                token: _,
                condition,
                consequence,
                alternative,
            }) => self.eval_if_expression(condition, consequence, alternative),
            Expression::Func(FuncLiteral {
                token: _,
                parameters,
                body,
            }) => self.eval_func_literal(parameters, body),
            Expression::Call(CallExpression {
                token: _,
                function,
                arguments,
            }) => self.eval_call_expression(function, arguments),
        }
    }

    fn eval_boolean(&self, token: Token) -> Result<Object, String> {
        let Token::Boolean(value) = token else {
            unreachable!()
        };
        Ok(Object::Boolean(value))
    }

    fn eval_integer(&self, token: Token) -> Result<Object, String> {
        let Token::Int(value) = token else {
            unreachable!()
        };
        Ok(Object::Int(value))
    }

    fn eval_identifier(&self, token: Token) -> Result<Object, String> {
        let Token::Identifier(ident) = token else {
            unreachable!()
        };
        self.env
            .borrow()
            .get(&ident)
            .map_err(|_| format!("key not found: {}", ident))
    }

    fn eval_prefix_expression(
        &self,
        operator: Token,
        right: Box<Expression>,
    ) -> Result<Object, String> {
        let right = self.eval_expression(*right)?;
        match operator {
            Token::Bang => self.eval_bang_prefix_expression(right),
            Token::Minus => self.eval_minus_prefix_expression(right),
            _ => Err(format!("unsupported prefix operator")),
        }
    }

    fn eval_bang_prefix_expression(&self, right: Object) -> Result<Object, String> {
        match right {
            Object::Boolean(value) => Ok(Object::Boolean(!value)),
            Object::Null => Ok(Object::Boolean(true)),
            _ => Ok(Object::Boolean(false)),
        }
    }

    fn eval_minus_prefix_expression(&self, right: Object) -> Result<Object, String> {
        match right {
            Object::Int(value) => Ok(Object::Int(-1 * value)),
            _ => Err(format!("prefix operator '-' only supported for Int values")),
        }
    }

    fn eval_infix_expression(
        &self,
        operator: Token,
        left: Box<Expression>,
        right: Box<Expression>,
    ) -> Result<Object, String> {
        match (*left, *right) {
            (Expression::IntegerLiteral(left), Expression::IntegerLiteral(right)) => self.eval_integer_infix_expression(operator, left, right),
            (Expression::Boolean(left), Expression::Boolean(right)) => self.eval_boolean_infix_expression(operator, left, right),
            (left, right) if discriminant(&left) != discriminant(&right) => Err(format!("Mismatched types cannot be compared"))
            _ => Err(format!("Infix operation unsupported for the given operands"))
        }
    }

    fn eval_integer_infix_expression(&self, operator: Token, left: Token, right: Token) -> Result<Object, String> {
        let Token::Int(left_val) = left else {unreachable!()};
        let Token::Int(right_val) = right else {unreachable!()};
        match operator {
            Token::Plus => Ok(Object::Int(left_val + right_val)),
            Token::Minus => Ok(Object::Int(left_val - right_val)),
            Token::Asterisk => Ok(Object::Int(left_val * right_val)),
            Token::Slash => Ok(Object::Int(left_val / right_val)),
            Token::GreaterThan => Ok(Object::Boolean(left_val > right_val)),
            Token::GreaterThanEqual => Ok(Object::Boolean(left_val >= right_val)),
            Token::LessThan => Ok(Object::Boolean(left_val < right_val)),
            Token::LessThanEqual => Ok(Object::Boolean(left_val <= right_val)),
            Token::Equal => Ok(Object::Boolean(left_val == right_val)),
            Token::NotEqual => Ok(Object::Boolean(left_val != right_val)),
            _ => Err(format!("unsupported binary operation between integer operands"))
        }
    }

    fn eval_boolean_infix_expression(&self, operator: Token, left: Token, right: Token) -> Result<Object, String> {
        let Token::Boolean(left_val) = left else {unreachable!()};
        let Token::Boolean(right_val) = right else {unreachable!()};
        match operator {
            Token::Equal => Ok(Object::Boolean(left_val == right_val)),
            Token::NotEqual => Ok(Object::Boolean(left_val != right_val)),
            _ => Err(format!("unsupported binary operation between boolean operands"))
        }
    }

    fn eval_if_expression(
        &self,
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    ) -> Result<Object, String> {
        todo!()
    }

    fn eval_func_literal(
        &self,
        parameters: Vec<Token>,
        body: BlockStatement,
    ) -> Result<Object, String> {
        todo!()
    }

    fn eval_call_expression(
        &self,
        function: Box<Expression>,
        arguments: Vec<Expression>,
    ) -> Result<Object, String> {
        todo!()
    }
}
