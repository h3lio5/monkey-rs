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
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            sp: 0,
            stack: Vec::with_capacity(STACK_SIZE),
        }
    }

    fn run(&mut self) -> VmResult<()> {
        let mut ip = 0;
        while ip < self.instructions.len() {
            let opcode: OpCode = self.instructions[ip].into();

            match opcode {
                OpCode::CONSTANT => self.handle_constant_opcode(&mut ip)?,
                OpCode::ADD => self.handle_add_opcode(&mut ip)?,
                _ => todo!(),
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

    fn handle_add_opcode(&mut self, ip: &mut usize) -> VmResult<()> {
        // move to the next instruction
        *ip += 1;
        // pop off the top two stack elements which are the operands
        let right = self.stack_pop().ok_or(VmError::StackEmptyError)?;
        let left = self.stack_pop().ok_or(VmError::StackEmptyError)?;

        
        let result = match (left, right) {
            (Object::Int(left_val), Object::Int(right_val)) => Object::Int(left_val + right_val),
            _ => todo!()
        };

        // push the result on to the stack
        self.stack_push(result)?;
        Ok(())
    }


    /////////////////////// Stack Operations //////////////////////
    fn stack_push(&mut self, obj: Object) -> VmResult<()> {
        // Check for stack overflow before pushing the object
        if self.sp >= STACK_SIZE {
            return Err(errors::VmError::StackOverflowError);
        }
        // Push the object onto the stack
        self.stack.push(obj);
        self.sp += 1; // Increment the stack pointer
    
        Ok(())
    }

    fn stack_pop(&mut self) -> Option<Object> {
        if let Some(value) = self.stack.pop() {
            self.sp -= 1;
            Some(value)
        } else {
            None
        }
    }

    fn stack_top(&self) -> Option<Object> {
        if self.sp == 0 {
            None
        } else {
            let top = self.stack[self.sp - 1].clone();
            Some(top)
        }
    }
}
