use std::cell::RefCell;
use std::mem::discriminant;
use std::rc::Rc;

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

    fn eval_program(&mut self, program: Program) -> Result<Object, String> {
        program
            .into_iter()
            .try_fold(Object::Null, |_, statement| self.eval_statement(statement))
    }

    fn eval_statement(&mut self, statement: Statement) -> Result<Object, String> {
        match statement {
            Statement::Let(LetStatement {
                token: _,
                name,
                value,
            }) => {
                let val = self.eval_expression(value)?;
                if let Token::Identifier(ident) = name {
                    self.env.borrow_mut().set(&ident, val);
                }
                Ok(Object::Null)
            }
            Statement::Return(ReturnStatement { token: _, value }) => {
                let val = self.eval_expression(value)?;
                Ok(Object::Return(Box::new(val)))
            }
            Statement::Expression(ExpressionStatement { token: _, value }) => {
                self.eval_expression(value)
            }
        }
    }

    fn eval_expression(&mut self, expression: Expression) -> Result<Object, String> {
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
        &mut self,
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
        if let Object::Int(value) = right {
            Ok(Object::Int(-value))
        } else {
            Err(format!("prefix operator '-' only supported for Int values"))
        }
    }

    fn eval_infix_expression(
        &mut self,
        operator: Token,
        left: Box<Expression>,
        right: Box<Expression>,
    ) -> Result<Object, String> {
        let left = self.eval_expression(*left)?;
        let right = self.eval_expression(*right)?;
        match (&left, &right) {
            (Object::Int(left_val), Object::Int(right_val)) => {
                self.eval_integer_infix_expression(operator, *left_val, *right_val)
            }
            (Object::Boolean(left_val), Object::Boolean(right_val)) => {
                self.eval_boolean_infix_expression(operator, *left_val, *right_val)
            }
            _ if discriminant(&left) != discriminant(&right) => {
                Err(format!("Mismatched types cannot be compared"))
            }
            _ => Err(format!(
                "Infix operation unsupported for the given operands"
            )),
        }
    }

    fn eval_integer_infix_expression(
        &self,
        operator: Token,
        left_val: i64,
        right_val: i64,
    ) -> Result<Object, String> {
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
            _ => Err(format!(
                "unsupported binary operation between integer operands"
            )),
        }
    }

    fn eval_boolean_infix_expression(
        &self,
        operator: Token,
        left_val: bool,
        right_val: bool,
    ) -> Result<Object, String> {
        match operator {
            Token::Equal => Ok(Object::Boolean(left_val == right_val)),
            Token::NotEqual => Ok(Object::Boolean(left_val != right_val)),
            _ => Err(format!(
                "unsupported binary operation between boolean operands"
            )),
        }
    }

    fn eval_if_expression(
        &mut self,
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    ) -> Result<Object, String> {
        let condition_outcome = self.eval_expression(*condition)?;
        if self.is_truthy(condition_outcome) {
            self.eval_block_statement(consequence)
        } else if let Some(alternative) = alternative {
            self.eval_block_statement(alternative)
        } else {
            Ok(Object::Null)
        }
    }

    fn eval_block_statement(&mut self, block_statement: BlockStatement) -> Result<Object, String> {
        block_statement
            .statements
            .into_iter()
            .try_fold(Object::Null, |_, statement| {
                let result = self.eval_statement(statement)?;
                if let Object::Return(_) = result {
                    return Ok(result);
                }
                Ok(result)
            })
    }

    fn eval_func_literal(
        &self,
        parameters: Vec<Token>,
        body: BlockStatement,
    ) -> Result<Object, String> {
        let val = Object::Func(FuncObject {
            parameters,
            body,
            env: Rc::clone(&self.env),
        });
        Ok(val)
    }

    fn eval_call_expression(
        &mut self,
        function: Box<Expression>,
        arguments: Vec<Expression>,
    ) -> Result<Object, String> {
        // evaluate the argument expressions and store the values in the new environment
        let args_evaluated = arguments
            .into_iter()
            .map(|arg| self.eval_expression(arg))
            .collect::<Result<Vec<Object>, String>>()?;

        let env_cache = Rc::clone(&self.env);

        let Object::Func(FuncObject {
            parameters,
            body,
            env,
        }) = self.eval_expression(*function)?
        else {
            unreachable!()
        };

        // store the arguments in the func scoped environment
        let mut func_scope_env = Environment::new_enclosed(env);
        parameters
            .into_iter()
            .zip(args_evaluated)
            .for_each(|(p, val)| {
                if let Token::Identifier(ident) = p {
                    func_scope_env.set(&ident, val);
                }
            });
        // apply the updated func scoped env to the main env
        self.env = Rc::new(RefCell::new(func_scope_env));

        let result = self.eval_block_statement(body)?;

        // Restore the main env from the cache
        self.env = env_cache;

        Ok(result)
    }

    fn is_truthy(&self, condition: Object) -> bool {
        match condition {
            Object::Boolean(val) => val,
            Object::Null => false,
            _ => true,
        }
    }
}
