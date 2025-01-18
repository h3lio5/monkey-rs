use super::{EvalError, Evaluator};
use crate::ast::*;
use crate::lexer::Lexer;
use crate::object::Object;
use crate::parser::Parser;
use crate::token::Token;

#[test]
fn test_evaluator_integer_expr() {
    let mut lexer = Lexer::new("5;");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap();
    let mut evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::Int(5));

    lexer = Lexer::new("3 + 6;");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::Int(9));

    lexer = Lexer::new("3 * 7 + 6;");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::Int(27));

    lexer = Lexer::new("let a = 3 * 7 + 6;");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::Null);

    lexer = Lexer::new("let a = 3 * 7 + 6; a + 3;");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::Int(30));
}

#[test]
fn test_evaluator_integer_expr_incorrect() {
    let mut lexer = Lexer::new("3 * 7 + 6;");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap();
    let mut evaluator = Evaluator::new();
    assert_ne!(evaluator.eval_program(program).unwrap(), Object::Int(29));

    lexer = Lexer::new("3 * 7 * true;");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert!(matches!(
        evaluator.eval_program(program).unwrap_err(),
        EvalError::TypeMismatch(_)
    ));
}

#[test]
fn test_evaluator_string_expr() {
    let mut lexer = Lexer::new("\"heyy\";");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap();
    let mut evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::String("heyy".to_string()));

    lexer = Lexer::new("\"baby girl\";");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::String("baby girl".to_string()));

    lexer = Lexer::new("\"baby\" + \" \" + \"girl\";");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::String("baby girl".to_string()));

}

#[test]
fn test_evaluator_boolean_expr() {
    let mut lexer = Lexer::new("true;");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap();
    let mut evaluator = Evaluator::new();
    assert_eq!(
        evaluator.eval_program(program).unwrap(),
        Object::Boolean(true)
    );

    lexer = Lexer::new("let a = true; !a;");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(
        evaluator.eval_program(program).unwrap(),
        Object::Boolean(false)
    );

    lexer = Lexer::new("let a = true; a == false");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(
        evaluator.eval_program(program).unwrap(),
        Object::Boolean(false)
    );

    lexer = Lexer::new("let a = true; if (a) {!!a}");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(
        evaluator.eval_program(program).unwrap(),
        Object::Boolean(true)
    );
}

#[test]
fn test_evaluator_boolean_expr_incorrect() {
    let mut lexer = Lexer::new("true;");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap();
    let mut evaluator = Evaluator::new();
    assert_ne!(
        evaluator.eval_program(program).unwrap(),
        Object::Boolean(false)
    );

    lexer = Lexer::new("let a = true; a + true;");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(
        evaluator.eval_program(program).unwrap_err(),
        EvalError::UnsupportedOperation("unsupported boolean operation".to_owned())
    );

    lexer = Lexer::new("let a = true; -a;");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(
        evaluator.eval_program(program).unwrap_err(),
        EvalError::TypeMismatch("prefix operator '-' only supported for integers".to_owned())
    );
}

#[test]
fn test_evaluator_bang_operator() {
    let mut lexer = Lexer::new("!true;");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap();
    let mut evaluator = Evaluator::new();
    assert_eq!(
        evaluator.eval_program(program).unwrap(),
        Object::Boolean(false)
    );

    lexer = Lexer::new("let a = true; !!!!true");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(
        evaluator.eval_program(program).unwrap(),
        Object::Boolean(true)
    );

    lexer = Lexer::new("!5");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(
        evaluator.eval_program(program).unwrap(),
        Object::Boolean(false)
    );

    lexer = Lexer::new("!!!!5");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(
        evaluator.eval_program(program).unwrap(),
        Object::Boolean(true)
    );
}

#[test]
fn test_evaluator_if_expr() {
    let mut lexer = Lexer::new("if (true) { 10 }");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap();
    let mut evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::Int(10));

    lexer = Lexer::new("if (false) { 10 }");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::Null);

    lexer = Lexer::new("if (1) { 10 }");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::Int(10));

    lexer = Lexer::new("if (1 < 2) { 10 }");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::Int(10));

    lexer = Lexer::new("if (1 > 2) { 10 }");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::Null);

    lexer = Lexer::new("if (1 > 2) { 10 } else { 20 }");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::Int(20));

    lexer = Lexer::new("if (1 < 2) { 10 } else { 20 }");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::Int(10));

    lexer = Lexer::new("let a = if (1 > 2) { 10 } else { 20 }; a;");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::Int(20));
}

#[test]
fn test_evaluator_return_statement() {
    let mut lexer = Lexer::new("return 10;");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap();
    let mut evaluator = Evaluator::new();
    assert_eq!(
        evaluator.eval_program(program).unwrap(),
        Object::Return(Box::new(Object::Int(10)))
    );

    lexer = Lexer::new("return 10; 11;");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(
        evaluator.eval_program(program).unwrap(),
        Object::Return(Box::new(Object::Int(10)))
    );

    lexer = Lexer::new("9; return 2 * 5; 11;");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(
        evaluator.eval_program(program).unwrap(),
        Object::Return(Box::new(Object::Int(10)))
    );

    lexer = Lexer::new(
        "let a = 2; 
    if (a > 1) { return true; }; false;",
    );
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(
        evaluator.eval_program(program).unwrap(),
        Object::Return(Box::new(Object::Boolean(true)))
    );

    lexer = Lexer::new(
        "let a = 1; 
    if (a > 1) { return true; }; false;",
    );
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(
        evaluator.eval_program(program).unwrap(),
        Object::Boolean(false)
    );
}

#[test]
fn test_evaluator_function_object() {
    let mut lexer = Lexer::new("fn (x) {x}");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap();
    let mut evaluator = Evaluator::new();
    assert!(matches!(
        evaluator.eval_program(program).unwrap(),
        Object::Func(_)
    ));

    lexer = Lexer::new("let a = fn(x) {x}; return a;");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert!(matches!(
        evaluator.eval_program(program).unwrap(),
        Object::Return(_)
    ));

    lexer = Lexer::new("let a = fn(x) {x}; a;");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert!(matches!(
        evaluator.eval_program(program).unwrap(),
        Object::Func(_)
    ));
}

#[test]
fn test_evaluator_function_call() {
    let mut lexer = Lexer::new("let add = fn(x, y) {x + y}; add(1,2);");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap();
    let mut evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::Int(3));

    lexer = Lexer::new("fn (x) {x * 2;}(3)");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::Int(6));

    lexer = Lexer::new("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5))");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::Int(20));

    lexer = Lexer::new("let giveMeHello = fn() { \"Hello mah man\"; }; giveMeHello();");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::String("Hello mah man".to_string()));

    lexer = Lexer::new("let sayHello = fn(name) { \"Hello mah man\" + \" \" + name + \"!\"; }; sayHello(\"donny\");");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap();
    evaluator = Evaluator::new();
    assert_eq!(evaluator.eval_program(program).unwrap(), Object::String("Hello mah man donny!".to_string()));
}
