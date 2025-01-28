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
            Statement::Expression(smtm) => self.compile_expr_statement(smtm.value),
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
            _ => todo!(),
        }
    }

    fn compile_integer_literal(&mut self, token: Token) {
        match token {
            Token::Int(val) => {
                let obj = Object::Int(val);
                let constant_index = self.add_constant(obj);
                self.emit(OpCode::CONSTANT, vec![constant_index]);
            }
            _ => {}
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
