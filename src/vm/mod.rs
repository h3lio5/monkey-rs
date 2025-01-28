use errors::{VmError, VmResult};
use opcode::OpCode;

use crate::compiler::ByteCode;
use crate::object::Object;

pub mod errors;
pub mod opcode;
#[cfg(test)]
mod tests;

pub const STACK_SIZE: usize = 2048;

#[derive(Debug, Clone)]
pub struct Vm {
    constants: Vec<Object>,
    instructions: Vec<u8>,
    stack: Vec<Object>,
    sp: usize,
}

impl Vm {
    fn new(bytecode: ByteCode) -> Self {
        let mut stack = Vec::with_capacity(STACK_SIZE);
        stack.resize(STACK_SIZE, Object::Null);

        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            sp: 0,
            stack,
        }
    }

    fn run(&mut self) -> VmResult<()> {
        let mut ip = 0;
        while ip < self.instructions.len() {
            let opcode: OpCode = self.instructions[ip].into();

            match opcode {
                OpCode::CONSTANT => self.handle_constant_opcode(&mut ip)?,
                OpCode::ADD | OpCode::SUB | OpCode::MUL | OpCode::DIV => {
                    self.handle_binary_opcode(opcode, &mut ip)?
                }
                OpCode::POP => self.handle_pop_opcode(&mut ip)?,
                OpCode::TRUE | OpCode::FALSE => self.handle_bool_opcode(opcode, &mut ip)?,
                OpCode::EQUAL
                | OpCode::NOTEQUAL
                | OpCode::GREATERTHAN
                | OpCode::GREATERTHANEQUAL => self.handle_comparision_opcode(opcode, &mut ip)?,
                op => return Err(VmError::OpcodeDecodingError { opcode: op }),
            }
        }
        Ok(())
    }

    ////////////////////////// OpCode Handlers ///////////////////////////
    fn handle_constant_opcode(&mut self, ip: &mut usize) -> VmResult<()> {
        // move to the next instruction, i.e., the operands
        *ip += 1;
        let bytes: [u8; 2] = self.instructions[*ip..*ip + 2].try_into().map_err(|_| {
            errors::VmError::OpcodeDecodingError {
                opcode: OpCode::CONSTANT,
            }
        })?;
        let operand = i16::from_be_bytes(bytes);
        // jump to the next opcode
        *ip += 2;
        // get the constant from the constants pool
        let constant = self.constants[operand as usize].clone();
        // push the object to the stack
        self.stack_push(constant)?;
        Ok(())
    }

    fn handle_binary_opcode(&mut self, opcode: OpCode, ip: &mut usize) -> VmResult<()> {
        // move to the next instruction
        *ip += 1;
        // pop off the top two stack elements which are the operands
        let right = self.stack_pop()?;
        let left = self.stack_pop()?;

        let result = match (left, right) {
            (Object::Int(left_val), Object::Int(right_val)) => {
                self.execute_binary_integer_op(opcode, left_val, right_val)?
            }
            _ => return Err(VmError::TypeError),
        };
        // push the result on to the stack
        self.stack_push(result)?;
        Ok(())
    }

    fn handle_comparision_opcode(&mut self, opcode: OpCode, ip: &mut usize) -> VmResult<()> {
        // move to the next instruction
        *ip += 1;
        let right = self.stack_pop()?;
        let left = self.stack_pop()?;

        let result = match (left, right) {
            (Object::Int(left_val), Object::Int(right_val)) => {
                self.execute_integer_comparision_op(opcode, left_val, right_val)?
            }
            (Object::Boolean(left_val), Object::Boolean(right_val)) => match opcode {
                OpCode::EQUAL => Object::Boolean(left_val == right_val),
                OpCode::NOTEQUAL => Object::Boolean(left_val != right_val),
                _ => return Err(VmError::UnsupportedBinaryOperation),
            },
            _ => return Err(VmError::TypeError),
        };
        // push the result on to the stack
        self.stack_push(result)?;
        Ok(())
    }

    fn execute_integer_comparision_op(
        &mut self,
        opcode: OpCode,
        left: i64,
        right: i64,
    ) -> VmResult<Object> {
        match opcode {
            OpCode::EQUAL => Ok(Object::Boolean(left == right)),
            OpCode::NOTEQUAL => Ok(Object::Boolean(left != right)),
            OpCode::GREATERTHAN => Ok(Object::Boolean(left > right)),
            OpCode::GREATERTHANEQUAL => Ok(Object::Boolean(left >= right)),
            _ => unreachable!(),
        }
    }

    fn execute_binary_integer_op(
        &mut self,
        opcode: OpCode,
        left: i64,
        right: i64,
    ) -> VmResult<Object> {
        match opcode {
            OpCode::ADD => Ok(Object::Int(left + right)),
            OpCode::SUB => Ok(Object::Int(left - right)),
            OpCode::MUL => Ok(Object::Int(left * right)),
            OpCode::DIV if right == 0 => Err(VmError::DivByZeroError),
            OpCode::DIV => Ok(Object::Int(left / right)),
            _ => Err(VmError::UnsupportedBinaryOperation),
        }
    }

    #[inline]
    fn handle_pop_opcode(&mut self, ip: &mut usize) -> VmResult<()> {
        // move to the next instruction
        *ip += 1;
        let _ = self.stack_pop()?;
        Ok(())
    }

    #[inline]
    fn handle_bool_opcode(&mut self, opcode: OpCode, ip: &mut usize) -> VmResult<()> {
        // move to the next instruction
        *ip += 1;
        match opcode {
            OpCode::TRUE => self.stack_push(Object::Boolean(true)),
            OpCode::FALSE => self.stack_push(Object::Boolean(false)),
            _ => unreachable!(),
        }
    }
    /////////////////////// Stack Operations //////////////////////
    /// Pushes an object onto the stack.
    /// Returns `StackOverflowError` if the stack is full.
    #[inline]
    fn stack_push(&mut self, obj: Object) -> VmResult<()> {
        match self.sp {
            sp if sp >= STACK_SIZE => Err(VmError::StackOverflowError),
            sp => {
                self.stack[sp] = obj;
                self.sp += 1;
                Ok(())
            }
        }
    }

    /// Pops and returns the top object from the stack.
    /// Returns `StackEmptyError` if the stack is empty.
    #[inline]
    fn stack_pop(&mut self) -> VmResult<Object> {
        match self.sp {
            0 => Err(VmError::StackEmptyError),
            sp => {
                self.sp -= 1;
                Ok(self.stack[sp - 1].clone())
            }
        }
    }

    /// Returns a reference to the top object on the stack without removing it.
    /// Returns `None` if the stack is empty.
    #[inline]
    fn last_popped_stack_element(&self) -> Option<Object> {
        // Using direct indexing for O(1) access instead of iter().last() which is O(n)
        Some(self.stack[self.sp].clone())
    }
}
