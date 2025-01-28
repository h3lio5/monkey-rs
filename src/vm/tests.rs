use super::*;
use crate::{ast::*, compiler::*, lexer::*, parser::*};

fn test_helper_parse_input(input: &str) -> ByteCode {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().expect("Parsing failed!");
    let mut compiler = Compiler::new();
    compiler.compile_program(program);
    compiler.bytecode()
}

#[test]
fn test_vm_constant_op() {
    let bytecode = test_helper_parse_input("1");
    let mut vm = Vm::new(bytecode);
    vm.run();

    assert_eq!(vm.stack_top(), Some(Object::Int(1)));

    let bytecode = test_helper_parse_input("1; 2;");
    let mut vm = Vm::new(bytecode);
    vm.run();

    assert_eq!(vm.stack_top(), Some(Object::Int(2)));
}

#[test]
fn test_vm_add_op() {
    let bytecode = test_helper_parse_input("1 + 2");
    let mut vm = Vm::new(bytecode);
    vm.run();

    assert_eq!(vm.stack_top(), Some(Object::Int(3)));

    let bytecode = test_helper_parse_input("1 + 2 + 3;");
    let mut vm = Vm::new(bytecode);
    vm.run();

    assert_eq!(vm.stack_top(), Some(Object::Int(6)));
}