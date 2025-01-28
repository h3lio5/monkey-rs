use std::fs::OpenOptions;

#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    CONSTANT = 0x00,
    ADD = 0x01,
    SUB = 0x02,
    MUL = 0x03,
    DIV = 0x04,
    POP = 0x05,
    TRUE = 0x06,
    FALSE = 0x07,
    EQUAL = 0x08,
    NOTEQUAL = 0x09,
    GREATERTHAN = 0x10,
    GREATERTHANEQUAL = 0x11,
    INVALID = 0xFF,
}

impl From<u8> for OpCode {
    fn from(byte: u8) -> OpCode {
        match byte {
            0x00 => OpCode::CONSTANT,
            0x01 => OpCode::ADD,
            0x02 => OpCode::SUB,
            0x03 => OpCode::MUL,
            0x04 => OpCode::DIV,
            0x05 => OpCode::POP,
            0x06 => OpCode::TRUE,
            0x07 => OpCode::FALSE,
            0x08 => OpCode::EQUAL,
            0x09 => OpCode::NOTEQUAL,
            0x10 => OpCode::GREATERTHAN,
            0x11 => OpCode::GREATERTHANEQUAL,
            _ => OpCode::INVALID,
        }
    }
}

impl From<OpCode> for u8 {
    #[allow(clippy::as_conversions)]
    fn from(opcode: OpCode) -> u8 {
        opcode as u8
    }
}

impl From<OpCode> for usize {
    fn from(opcode: OpCode) -> usize {
        opcode as usize
    }
}

pub fn opcode_encode(opcode: OpCode, operands: Vec<i16>) -> Vec<u8> {
    let mut encoded = vec![opcode as u8];
    for operand in operands {
        let b = operand.to_be_bytes();
        encoded.extend_from_slice(&b);
    }
    encoded
}
