use std::cell::RefCell;
use std::collections::HashMap;
use std::mem::discriminant;
use std::rc::Rc;

use crate::{
    ast::*,
    object::{environment::Environment, FuncObject, Object},
    token::Token,
};

mod errors;
use errors::*;
#[cfg(test)]
mod tests;

type Env = Rc<RefCell<Environment>>;

/// The evaluator for the Monkey programming language
#[derive(Debug, Clone)]
pub struct Evaluator {
    env: Env,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn eval_program(&mut self, program: Program) -> EvalResult<Object> {
        let mut result = Object::Null;
        for statement in program {
            result = self.eval_statement(statement)?;
            if let Object::Return(value) = result {
                return Ok(*value);
            }
        }
        Ok(result)
    }

    fn eval_statement(&mut self, statement: Statement) -> EvalResult<Object> {
        match statement {
            Statement::Let(stmt) => self.eval_let_statement(stmt),
            Statement::Return(stmt) => self.eval_return_statement(stmt),
            Statement::Expression(stmt) => self.eval_expression(stmt.value),
        }
    }

    fn eval_let_statement(&mut self, stmt: LetStatement) -> EvalResult<Object> {
        let Token::Identifier(ident) = stmt.name else {
            return Err(EvalError::UnknownIdentifier(
                "invalid identifier".to_string(),
            ));
        };
        let value = self.eval_expression(stmt.value)?;
        self.env.borrow_mut().set(&ident, value);
        Ok(Object::Null)
    }

    fn eval_return_statement(&mut self, stmt: ReturnStatement) -> EvalResult<Object> {
        let val = self.eval_expression(stmt.value)?;
        Ok(Object::Return(Box::new(val)))
    }

    fn eval_expression(&mut self, expression: Expression) -> EvalResult<Object> {
        match expression {
            Expression::Boolean(token) => self.eval_boolean(token),
            Expression::IntegerLiteral(token) => self.eval_integer(token),
            Expression::StringLiteral(token) => self.eval_string(token),
            Expression::Identifier(token) => self.eval_identifier(token),
            Expression::Prefix(expr) => self.eval_prefix_expression(expr),
            Expression::Infix(expr) => self.eval_infix_expression(expr),
            Expression::If(expr) => self.eval_if_expression(expr),
            Expression::Func(expr) => self.eval_func_literal(expr),
            Expression::Call(expr) => self.eval_call_expression(expr),
            Expression::Array(expr) => self.eval_array_expression(expr),
            Expression::Index(expr) => self.eval_index_expression(expr),
            Expression::Hash(expr) => self.eval_hash_expression(expr),
        }
    }

    fn eval_boolean(&self, token: Token) -> EvalResult<Object> {
        match token {
            Token::True => Ok(Object::Boolean(true)),
            Token::False => Ok(Object::Boolean(false)),
            _ => Err(EvalError::TypeMismatch(
                "expected boolean token".to_string(),
            )),
        }
    }

    fn eval_integer(&self, token: Token) -> EvalResult<Object> {
        match token {
            Token::Int(value) => Ok(Object::Int(value)),
            _ => Err(EvalError::TypeMismatch(
                "expected integer token".to_string(),
            )),
        }
    }

    fn eval_string(&self, token: Token) -> EvalResult<Object> {
        match token {
            Token::String(value) => Ok(Object::String(value)),
            _ => Err(EvalError::TypeMismatch("expected string token".to_string())),
        }
    }

    fn eval_identifier(&self, token: Token) -> EvalResult<Object> {
        match token {
            Token::Identifier(ident) => self
                .env
                .borrow()
                .get(&ident)
                .map_err(|_| EvalError::UnknownIdentifier(ident)),
            _ => Err(EvalError::TypeMismatch(
                "expected identifier token".to_string(),
            )),
        }
    }

    fn eval_prefix_expression(&mut self, expr: PrefixExpression) -> EvalResult<Object> {
        let right = self.eval_expression(*expr.right)?;
        match expr.operator {
            Token::Bang => self.eval_bang_prefix_expression(right),
            Token::Minus => self.eval_minus_prefix_expression(right),
            _ => Err(EvalError::UnsupportedOperation(
                "invalid prefix operator".to_string(),
            )),
        }
    }

    fn eval_bang_prefix_expression(&self, right: Object) -> EvalResult<Object> {
        Ok(Object::Boolean(match right {
            Object::Boolean(value) => !value,
            Object::Null => true,
            _ => false,
        }))
    }

    fn eval_minus_prefix_expression(&self, right: Object) -> EvalResult<Object> {
        match right {
            Object::Int(value) => Ok(Object::Int(-value)),
            _ => Err(EvalError::TypeMismatch(
                "prefix operator '-' only supported for integers".to_string(),
            )),
        }
    }

    fn eval_infix_expression(&mut self, expr: InfixExpression) -> EvalResult<Object> {
        let left = self.eval_expression(*expr.left)?;
        let right = self.eval_expression(*expr.right)?;

        match (&left, &right) {
            (Object::Int(l), Object::Int(r)) => {
                self.eval_integer_infix_expression(expr.operator, *l, *r)
            }
            (Object::Boolean(l), Object::Boolean(r)) => {
                self.eval_boolean_infix_expression(expr.operator, *l, *r)
            }
            (Object::String(l), Object::String(r)) => {
                self.eval_string_infix_expression(expr.operator, l, r)
            }
            _ if discriminant(&left) != discriminant(&right) => {
                Err(EvalError::TypeMismatch("mismatched types".to_string()))
            }
            _ => Err(EvalError::UnsupportedOperation(
                "unsupported infix operation".to_string(),
            )),
        }
    }

    fn eval_integer_infix_expression(
        &self,
        operator: Token,
        left: i64,
        right: i64,
    ) -> EvalResult<Object> {
        Ok(match operator {
            Token::Plus => Object::Int(left + right),
            Token::Minus => Object::Int(left - right),
            Token::Asterisk => Object::Int(left * right),
            Token::Slash => Object::Int(left / right),
            Token::GreaterThan => Object::Boolean(left > right),
            Token::GreaterThanEqual => Object::Boolean(left >= right),
            Token::LessThan => Object::Boolean(left < right),
            Token::LessThanEqual => Object::Boolean(left <= right),
            Token::Equal => Object::Boolean(left == right),
            Token::NotEqual => Object::Boolean(left != right),
            _ => {
                return Err(EvalError::UnsupportedOperation(
                    "unsupported integer operation".to_string(),
                ))
            }
        })
    }

    fn eval_string_infix_expression(
        &self,
        operator: Token,
        left: &String,
        right: &String,
    ) -> EvalResult<Object> {
        if let Token::Plus = operator {
            Ok(Object::String(format!("{}{}", left, right)))
        } else {
            Err(EvalError::UnsupportedOperation(
                "unsupported string operation".to_string(),
            ))
        }
    }

    fn eval_boolean_infix_expression(
        &self,
        operator: Token,
        left: bool,
        right: bool,
    ) -> EvalResult<Object> {
        Ok(match operator {
            Token::Equal => Object::Boolean(left == right),
            Token::NotEqual => Object::Boolean(left != right),
            _ => {
                return Err(EvalError::UnsupportedOperation(
                    "unsupported boolean operation".to_string(),
                ))
            }
        })
    }

    fn eval_if_expression(&mut self, expr: IfExpression) -> EvalResult<Object> {
        let condition = self.eval_expression(*expr.condition)?;

        if self.is_truthy(&condition) {
            self.eval_block_statement(expr.consequence)
        } else if let Some(alt) = expr.alternative {
            self.eval_block_statement(alt)
        } else {
            Ok(Object::Null)
        }
    }

    fn eval_block_statement(&mut self, block: BlockStatement) -> EvalResult<Object> {
        let mut result = Object::Null;
        for stmt in block.statements {
            result = self.eval_statement(stmt)?;
            if matches!(result, Object::Return(_)) {
                break;
            }
        }
        Ok(result)
    }

    fn eval_func_literal(&self, expr: FuncLiteral) -> EvalResult<Object> {
        Ok(Object::Func(FuncObject {
            parameters: expr.parameters,
            body: expr.body,
            env: Rc::clone(&self.env),
        }))
    }

    fn eval_call_expression(&mut self, expr: CallExpression) -> EvalResult<Object> {
        let Object::Func(func) = self.eval_expression(*expr.function)? else {
            return Err(EvalError::InvalidFunction);
        };

        let args = self.eval_arguments(expr.arguments)?;
        let env_cache = Rc::clone(&self.env);

        let result = self.execute_function(func, args)?;
        self.env = env_cache;

        Ok(result)
    }

    fn eval_arguments(&mut self, arguments: Vec<Expression>) -> EvalResult<Vec<Object>> {
        arguments
            .into_iter()
            .map(|arg| self.eval_expression(arg))
            .collect()
    }

    fn execute_function(&mut self, func: FuncObject, args: Vec<Object>) -> EvalResult<Object> {
        let mut func_env = Environment::new_enclosed(func.env);

        // Bind parameters to arguments
        for (param, arg) in func.parameters.into_iter().zip(args) {
            if let Token::Identifier(name) = param {
                func_env.set(&name, arg);
            }
        }

        self.env = Rc::new(RefCell::new(func_env));
        self.eval_block_statement(func.body)
    }

    fn eval_array_expression(&mut self, expr: ArrayLiteral) -> EvalResult<Object> {
        let array = expr
            .elements
            .into_iter()
            .map(|arg| self.eval_expression(arg))
            .collect::<EvalResult<Vec<_>>>()?;

        Ok(Object::Array(array))
    }

    fn eval_index_expression(&mut self, expr: IndexExpression) -> EvalResult<Object> {
        let left = self.eval_expression(*expr.left)?;
        let index = self.eval_expression(*expr.index)?;

        match (&left, &index) {
            (Object::Array(array), Object::Int(index)) => {
                self.eval_array_index_expression(array, index)
            }
            (Object::Hash(hash), Object::Int(_) | Object::Boolean(_) | Object::String(_)) => {
                Ok(hash.get(&index).cloned().unwrap_or(Object::Null))
            }
            (Object::Array(_), non_int) => Err(EvalError::TypeMismatch(format!(
                "array index must be INTEGER, got {non_int:?}"
            ))),
            (Object::Hash(_), non_hashable) => Err(EvalError::TypeMismatch(format!(
                "hash key must be INTEGER, BOOLEAN, or STRING, got {non_hashable:?}"
            ))),
            (non_indexable, _) => Err(EvalError::UnsupportedOperation(format!(
                "index operator not supported for type {non_indexable:?}"
            ))),
        }
    }

    fn eval_array_index_expression(&mut self, array: &[Object], index: &i64) -> EvalResult<Object> {
        match array.get(*index as usize) {
            Some(value) => Ok(value.clone()),
            None => Ok(Object::Null), // Return Null if the index is out of bounds
        }
    }

    fn eval_hash_expression(&mut self, expr: HashLiteral) -> EvalResult<Object> {
        let mut hash = HashMap::new();
        for (key_expr, value_expr) in expr.pairs {
            let key = self.eval_expression(key_expr)?;
            let value = self.eval_expression(value_expr)?;
            hash.insert(key, value);
        }
        Ok(Object::Hash(hash))
    }

    #[inline]
    fn is_truthy(&self, obj: &Object) -> bool {
        !matches!(obj, Object::Boolean(false) | Object::Null)
    }
}
