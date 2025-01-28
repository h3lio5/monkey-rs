use std::fs::OpenOptions;

#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    CONSTANT = 0x00,
    ADD = 0x01,
    INVALID = 0x03,
}

impl From<u8> for OpCode {
    fn from(byte: u8) -> OpCode {
        match byte {
            0x00 => OpCode::CONSTANT,
            0x01 => OpCode::ADD,
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
