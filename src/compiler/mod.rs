use crate::ast::{Expression, InfixExpression, LetStatement, Program, ReturnStatement, Statement};
use crate::object::Object;
use crate::token::Token;
use crate::vm::opcode::{opcode_encode, OpCode};

#[derive(Debug, Clone)]
pub struct ByteCode {
    pub instructions: Vec<u8>,
    pub constants: Vec<Object>,
}

#[derive(Debug, Clone)]
pub struct Compiler {
    instructions: Vec<u8>,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
            constants: vec![],
        }
    }

    pub fn compile_program(&mut self, program: Program) -> Result<(), String> {
        for statement in program {
            self.compile_statement(statement);
        }

        Ok(())
    }

    fn compile_statement(&mut self, statement: Statement) {
        match statement {
            Statement::Let(smtm) => self.compile_let_statement(smtm),
            Statement::Return(smtm) => self.compile_return_statement(smtm),
            Statement::Expression(smtm) => {
                self.compile_expr_statement(smtm.value);
                // pop off the expression result from the stack to prevent it from blowing up
                self.emit(OpCode::POP, vec![]);
            }
        }
    }

    fn compile_let_statement(&mut self, statement: LetStatement) {
        todo!()
    }

    fn compile_return_statement(&mut self, statement: ReturnStatement) {
        todo!()
    }

    fn compile_expr_statement(&mut self, expr: Expression) {
        match expr {
            Expression::IntegerLiteral(token) => self.compile_integer_literal(token),
            Expression::Infix(expr) => self.compile_infix_expr(expr),
            Expression::Boolean(token) => self.compile_boolean_literal(token),
            _ => {
                println!("{expr:?}")
            }
        }
    }

    fn compile_integer_literal(&mut self, token: Token) {
        match token {
            Token::Int(val) => {
                let obj = Object::Int(val);
                let constant_index = self.add_constant(obj);
                self.emit(OpCode::CONSTANT, vec![constant_index]);
            }
            _ => {
                panic!("Expected an integer token, but got: {:?}", token);
            }
        }
    }

    fn compile_boolean_literal(&mut self, token: Token) {
        match token {
            Token::True => self.emit(OpCode::TRUE, vec![]),
            Token::False => self.emit(OpCode::FALSE, vec![]),
            _ => panic!("Expected a boolean token, but got: {:?}", token),
        };
    }

    fn compile_infix_expr(&mut self, expr: InfixExpression) {
        match expr.operator {
            // Handle less-than operations by reversing operands and using greater-than opcodes
            Token::LessThan | Token::LessThanEqual => {
                self.compile_expr_statement(*expr.right);
                self.compile_expr_statement(*expr.left);
                let opcode = match expr.operator {
                    Token::LessThan => OpCode::GREATERTHAN,
                    Token::LessThanEqual => OpCode::GREATERTHANEQUAL,
                    _ => unreachable!(),
                };
                self.emit(opcode, vec![]);
            }
            // Handle all other operations normally
            op => {
                self.compile_expr_statement(*expr.left);
                self.compile_expr_statement(*expr.right);
                let opcode = match op {
                    Token::Plus => OpCode::ADD,
                    Token::Minus => OpCode::SUB,
                    Token::Asterisk => OpCode::MUL,
                    Token::Slash => OpCode::DIV,
                    Token::Equal => OpCode::EQUAL,
                    Token::NotEqual => OpCode::NOTEQUAL,
                    Token::GreaterThan => OpCode::GREATERTHAN,
                    Token::GreaterThanEqual => OpCode::GREATERTHANEQUAL,
                    _ => todo!("Unsupported operator: {:?}", op),
                };
                self.emit(opcode, vec![]);
            }
        }
    }

    fn add_constant(&mut self, obj: Object) -> i16 {
        self.constants.push(obj);
        self.constants.len() as i16 - 1
    }

    fn emit(&mut self, opcode: OpCode, operands: Vec<i16>) -> i16 {
        let ins = opcode_encode(opcode, operands);
        self.instructions.extend(ins);
        self.instructions.len() as i16 - 1
    }

    pub fn bytecode(&self) -> ByteCode {
        ByteCode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }
}
