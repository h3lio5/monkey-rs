use super::*;
use crate::{ast::*, compiler::*, lexer::*, parser::*};

fn test_helper_parse_input(input: &str) -> (Vm, VmResult<()>) {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().expect("Parsing failed!");
    let mut compiler = Compiler::new();
    compiler.compile_program(program);
    let bytecode = compiler.bytecode();
    let mut vm = Vm::new(bytecode);
    let vm_run_result = vm.run();
    (vm, vm_run_result)
}

#[test]
fn test_vm_constant_op() {
    let (vm, _) = test_helper_parse_input("1");
    assert_eq!(vm.last_popped_stack_element(), Some(Object::Int(1)));

    let (vm, _) = test_helper_parse_input("1; 2;");
    assert_eq!(vm.last_popped_stack_element(), Some(Object::Int(2)));
}

#[test]
fn test_vm_add_op() {
    let (vm, _) = test_helper_parse_input("1 + 2");
    assert_eq!(vm.last_popped_stack_element(), Some(Object::Int(3)));

    let (vm, _) = test_helper_parse_input("1 + 2 + 3;");
    assert_eq!(vm.last_popped_stack_element(), Some(Object::Int(6)));
}

#[test]
fn test_vm_sub_op() {
    let (vm, _) = test_helper_parse_input("1 - 2");
    assert_eq!(vm.last_popped_stack_element(), Some(Object::Int(-1)));

    let (vm, _) = test_helper_parse_input("1 + 2 - 3;");
    assert_eq!(vm.last_popped_stack_element(), Some(Object::Int(0)));
}

#[test]
fn test_vm_mul_op() {
    let (vm, _) = test_helper_parse_input("1 * 2");
    assert_eq!(vm.last_popped_stack_element(), Some(Object::Int(2)));

    let (vm, _) = test_helper_parse_input("1 * 2 * 3;");
    assert_eq!(vm.last_popped_stack_element(), Some(Object::Int(6)));
}

#[test]
fn test_vm_div_op() {
    let (vm, _) = test_helper_parse_input("1 / 2");
    assert_eq!(vm.last_popped_stack_element(), Some(Object::Int(0)));

    let (vm, _) = test_helper_parse_input("1 * 2 + 3 / 5;");
    assert_eq!(vm.last_popped_stack_element(), Some(Object::Int(2)));

    let (vm, run_result) = test_helper_parse_input("1 / 0");
    assert_eq!(run_result, Err(VmError::DivByZeroError));
}

#[test]
fn test_vm_boolean_op() {
    let (vm, _) = test_helper_parse_input("true;");
    assert_eq!(vm.last_popped_stack_element(), Some(Object::Boolean(true)));

    let (vm, _) = test_helper_parse_input("false;");
    assert_eq!(vm.last_popped_stack_element(), Some(Object::Boolean(false)));
}

#[test]
fn test_vm_comparisions() {
    let (vm, _) = test_helper_parse_input("1 < 2");
    assert_eq!(vm.last_popped_stack_element(), Some(Object::Boolean(true)));

    let (vm, _) = test_helper_parse_input("1 < 2 == true");
    assert_eq!(vm.last_popped_stack_element(), Some(Object::Boolean(true))); 

    let (vm, _) = test_helper_parse_input("true != false");
    assert_eq!(vm.last_popped_stack_element(), Some(Object::Boolean(true)));

    let (vm, vm_run_result) = test_helper_parse_input("true < false");
    assert_eq!(vm_run_result, Err(VmError::UnsupportedBinaryOperation));

    let (vm, vm_run_result) = test_helper_parse_input("true < 2");
    assert_eq!(vm_run_result, Err(VmError::TypeError));
}
