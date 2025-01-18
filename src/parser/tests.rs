use crate::lexer::Lexer;
use crate::parser::*;
use crate::{ast::*, lexer};

#[test]
fn test_parser_let_statements() {
    let lexer = Lexer::new(
        "let x = 5;
        let y = true;
        let foobar = y;
    ",
    );
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap().into_iter();

    assert_eq!(
        program.next(),
        Some(Statement::Let(LetStatement {
            token: Token::Let,
            name: Token::Identifier("x".to_owned()),
            value: Expression::IntegerLiteral(Token::Int(5))
        }))
    );
    assert_eq!(
        program.next(),
        Some(Statement::Let(LetStatement {
            token: Token::Let,
            name: Token::Identifier("y".to_owned()),
            value: Expression::Boolean(Token::True)
        }))
    );
    assert_eq!(
        program.next(),
        Some(Statement::Let(LetStatement {
            token: Token::Let,
            name: Token::Identifier("foobar".to_owned()),
            value: Expression::Identifier(Token::Identifier("y".to_owned()))
        }))
    );
}

#[test]
fn test_parser_let_simple_let_statement_peek_error() {
    let lexer = Lexer::new("let x 5;");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program();
    assert_eq!(
        program.unwrap_err(),
        ParseError::MismatchedToken {
            expected: Token::Assign,
            found: Token::Int(5)
        }
    );
}

#[test]
fn test_parser_let_func_let_statement_peek_error() {
    let lexer = Lexer::new(
        "let mul = fn(x, y) {
    let a = 1;
    x * y;",
    );
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program();
    assert_eq!(
        program.unwrap_err(),
        ParseError::MismatchedToken {
            expected: Token::RBrace,
            found: Token::Eof
        }
    );
}

#[test]
fn test_parser_let_func_let_statement() {
    let lexer = Lexer::new(
        "let mul = fn(x, y) {
    let a = 1;
    x * y};
    ",
    );
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap().into_iter();

    assert_eq!(
        program.next(),
        Some(Statement::Let(LetStatement {
            token: Token::Let,
            name: Token::Identifier("mul".to_owned()),
            value: Expression::Func(FuncLiteral {
                token: Token::Func,
                parameters: vec![
                    Token::Identifier("x".to_owned()),
                    Token::Identifier("y".to_owned())
                ],
                body: BlockStatement {
                    token: Token::LBrace,
                    statements: vec![
                        Statement::Let(LetStatement {
                            token: Token::Let,
                            name: Token::Identifier("a".to_owned()),
                            value: Expression::IntegerLiteral(Token::Int(1))
                        }),
                        Statement::Expression(ExpressionStatement {
                            token: Token::Identifier("x".to_owned()),
                            value: Expression::Infix(InfixExpression {
                                operator: Token::Asterisk,
                                left: Box::new(Expression::Identifier(Token::Identifier(
                                    "x".to_owned()
                                ))),
                                right: Box::new(Expression::Identifier(Token::Identifier(
                                    "y".to_owned()
                                )))
                            })
                        })
                    ]
                }
            })
        }))
    );
}

#[test]
fn test_parser_return_statement() {
    let lexer = Lexer::new("return 5;");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap().into_iter();

    assert_eq!(
        program.next(),
        Some(Statement::Return(ReturnStatement {
            token: Token::Return,
            value: Expression::IntegerLiteral(Token::Int(5))
        }))
    );
}

#[test]
fn test_parser_identifier_expression() {
    let lexer = Lexer::new("cielo;");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap().into_iter();

    assert_eq!(
        program.next(),
        Some(Statement::Expression(ExpressionStatement {
            token: Token::Identifier("cielo".to_owned()),
            value: Expression::Identifier(Token::Identifier("cielo".to_owned()))
        }))
    );
}

#[test]
fn test_parser_operator_precedence() {
    let mut lexer = Lexer::new("!-a");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap().into_iter();

    assert_eq!(
        program.next(),
        Some(Statement::Expression(ExpressionStatement {
            token: Token::Bang,
            value: Expression::Prefix(PrefixExpression {
                operator: Token::Bang,
                right: Box::new(Expression::Prefix(PrefixExpression {
                    operator: Token::Minus,
                    right: Box::new(Expression::Identifier(Token::Identifier("a".to_owned())))
                }))
            })
        }))
    );
    assert_eq!(program.next(), None);

    lexer = Lexer::new("a + b * c + d / e - f");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap().into_iter();

    assert_eq!(
        program.next(),
        Some(Statement::Expression(ExpressionStatement {
            token: Token::Identifier("a".to_owned()),
            value: Expression::Infix(InfixExpression {
                operator: Token::Minus,
                left: Box::new(Expression::Infix(InfixExpression {
                    operator: Token::Plus,
                    left: Box::new(Expression::Infix(InfixExpression {
                        operator: Token::Plus,
                        left: Box::new(Expression::Identifier(Token::Identifier("a".to_owned()))),
                        right: Box::new(Expression::Infix(InfixExpression {
                            operator: Token::Asterisk,
                            left: Box::new(Expression::Identifier(Token::Identifier(
                                "b".to_owned()
                            ))),
                            right: Box::new(Expression::Identifier(Token::Identifier(
                                "c".to_owned()
                            )))
                        }))
                    })),
                    right: Box::new(Expression::Infix(InfixExpression {
                        operator: Token::Slash,
                        left: Box::new(Expression::Identifier(Token::Identifier("d".to_owned()))),
                        right: Box::new(Expression::Identifier(Token::Identifier("e".to_owned())))
                    }))
                })),
                right: Box::new(Expression::Identifier(Token::Identifier("f".to_owned())))
            })
        }))
    );
    assert_eq!(program.next(), None);

    // ((3 + (4 * 5)) != ((3 * 2) + (4 * 5)))
    lexer = Lexer::new("3 + 4 * 5 != 3 * 2 + 4 * 5");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap().into_iter();

    assert_eq!(
        program.next(),
        Some(Statement::Expression(ExpressionStatement {
            token: Token::Int(3),
            value: Expression::Infix(InfixExpression {
                operator: Token::NotEqual,
                left: Box::new(Expression::Infix(InfixExpression {
                    operator: Token::Plus,
                    left: Box::new(Expression::IntegerLiteral(Token::Int(3))),
                    right: Box::new(Expression::Infix(InfixExpression {
                        operator: Token::Asterisk,
                        left: Box::new(Expression::IntegerLiteral(Token::Int(4))),
                        right: Box::new(Expression::IntegerLiteral(Token::Int(5)))
                    }))
                })),
                right: Box::new(Expression::Infix(InfixExpression {
                    operator: Token::Plus,
                    left: Box::new(Expression::Infix(InfixExpression {
                        operator: Token::Asterisk,
                        left: Box::new(Expression::IntegerLiteral(Token::Int(3))),
                        right: Box::new(Expression::IntegerLiteral(Token::Int(2)))
                    })),
                    right: Box::new(Expression::Infix(InfixExpression {
                        operator: Token::Asterisk,
                        left: Box::new(Expression::IntegerLiteral(Token::Int(4))),
                        right: Box::new(Expression::IntegerLiteral(Token::Int(5)))
                    }))
                }))
            })
        }))
    );
    assert_eq!(program.next(), None);
}

#[test]
fn test_parser_integer_literal() {
    let lexer = Lexer::new("1; 2; 3;");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap().into_iter();

    assert_eq!(
        program.next(),
        Some(Statement::Expression(ExpressionStatement {
            token: Token::Int(1),
            value: Expression::IntegerLiteral(Token::Int(1))
        }))
    );
    assert_eq!(
        program.next(),
        Some(Statement::Expression(ExpressionStatement {
            token: Token::Int(2),
            value: Expression::IntegerLiteral(Token::Int(2))
        }))
    );
    assert_eq!(
        program.next(),
        Some(Statement::Expression(ExpressionStatement {
            token: Token::Int(3),
            value: Expression::IntegerLiteral(Token::Int(3))
        }))
    );
    assert_eq!(program.next(), None);
}

#[test]
fn test_parser_boolean_literal() {
    let lexer = Lexer::new("true; false; true;");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap().into_iter();

    assert_eq!(
        program.next(),
        Some(Statement::Expression(ExpressionStatement {
            token: Token::True,
            value: Expression::Boolean(Token::True)
        }))
    );
    assert_eq!(
        program.next(),
        Some(Statement::Expression(ExpressionStatement {
            token: Token::False,
            value: Expression::Boolean(Token::False)
        }))
    );
    assert_eq!(
        program.next(),
        Some(Statement::Expression(ExpressionStatement {
            token: Token::True,
            value: Expression::Boolean(Token::True)
        }))
    );
    assert_eq!(program.next(), None);
}

#[test]
fn test_parser_string_literal() {
    let lexer = Lexer::new("\"yoo\"; \"hoo\"; \"baby\";");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap().into_iter();

    assert_eq!(
        program.next(),
        Some(Statement::Expression(ExpressionStatement {
            token: Token::String("yoo".to_string()),
            value: Expression::StringLiteral(Token::String("yoo".to_string()))
        }))
    );
    assert_eq!(
        program.next(),
        Some(Statement::Expression(ExpressionStatement {
            token: Token::String("hoo".to_string()),
            value: Expression::StringLiteral(Token::String("hoo".to_string()))
        }))
    );
    assert_eq!(
        program.next(),
        Some(Statement::Expression(ExpressionStatement {
            token: Token::String("baby".to_string()),
            value: Expression::StringLiteral(Token::String("baby".to_string()))
        }))
    );
    assert_eq!(program.next(), None);
}

#[test]
fn test_parser_if_expression() {
    let mut lexer = Lexer::new("if (x > y) {x + y}");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap().into_iter();

    assert_eq!(
        program.next(),
        Some(Statement::Expression(ExpressionStatement {
            token: Token::If,
            value: Expression::If(IfExpression {
                token: Token::If,
                condition: Box::new(Expression::Infix(InfixExpression {
                    operator: Token::GreaterThan,
                    left: Box::new(Expression::Identifier(Token::Identifier("x".to_owned()))),
                    right: Box::new(Expression::Identifier(Token::Identifier("y".to_owned()))),
                })),
                consequence: BlockStatement {
                    token: Token::LBrace,
                    statements: vec![Statement::Expression(ExpressionStatement {
                        token: Token::Identifier("x".to_owned()),
                        value: Expression::Infix(InfixExpression {
                            operator: Token::Plus,
                            left: Box::new(Expression::Identifier(Token::Identifier(
                                "x".to_owned()
                            ))),
                            right: Box::new(Expression::Identifier(Token::Identifier(
                                "y".to_owned()
                            )))
                        })
                    })]
                },
                alternative: None
            })
        }))
    );
    assert_eq!(program.next(), None);

    lexer = Lexer::new("if (x > y) {x + y} else {x}");
    parser = Parser::new(lexer);
    program = parser.parse_program().unwrap().into_iter();

    assert_eq!(
        program.next(),
        Some(Statement::Expression(ExpressionStatement {
            token: Token::If,
            value: Expression::If(IfExpression {
                token: Token::If,
                condition: Box::new(Expression::Infix(InfixExpression {
                    operator: Token::GreaterThan,
                    left: Box::new(Expression::Identifier(Token::Identifier("x".to_owned()))),
                    right: Box::new(Expression::Identifier(Token::Identifier("y".to_owned()))),
                })),
                consequence: BlockStatement {
                    token: Token::LBrace,
                    statements: vec![Statement::Expression(ExpressionStatement {
                        token: Token::Identifier("x".to_owned()),
                        value: Expression::Infix(InfixExpression {
                            operator: Token::Plus,
                            left: Box::new(Expression::Identifier(Token::Identifier(
                                "x".to_owned()
                            ))),
                            right: Box::new(Expression::Identifier(Token::Identifier(
                                "y".to_owned()
                            )))
                        })
                    })]
                },
                alternative: Some(BlockStatement {
                    token: Token::LBrace,
                    statements: vec![Statement::Expression(ExpressionStatement {
                        token: Token::Identifier("x".to_owned()),
                        value: Expression::Identifier(Token::Identifier("x".to_owned()))
                    })]
                })
            })
        }))
    );
    assert_eq!(program.next(), None);
}

#[test]
fn test_parser_function_literal() {
    let lexer = Lexer::new("fn(x, y) { x + y;}");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap().into_iter();

    assert_eq!(
        program.next(),
        Some(Statement::Expression(ExpressionStatement {
            token: Token::Func,
            value: Expression::Func(FuncLiteral {
                token: Token::Func,
                parameters: vec![
                    Token::Identifier("x".to_owned()),
                    Token::Identifier("y".to_owned())
                ],
                body: BlockStatement {
                    token: Token::LBrace,
                    statements: vec![Statement::Expression(ExpressionStatement {
                        token: Token::Identifier("x".to_owned()),
                        value: Expression::Infix(InfixExpression {
                            operator: Token::Plus,
                            left: Box::new(Expression::Identifier(Token::Identifier(
                                "x".to_owned()
                            ))),
                            right: Box::new(Expression::Identifier(Token::Identifier(
                                "y".to_owned()
                            )))
                        })
                    })]
                }
            })
        }))
    );
    assert_eq!(program.next(), None);
}

#[test]
fn test_parser_function_literal_incorrect() {
    let lexer = Lexer::new("fn(x, y { x + y;}");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program();
    assert_eq!(
        program.unwrap_err(),
        ParseError::MismatchedToken {
            expected: Token::RParen,
            found: Token::LBrace
        }
    );
}

#[test]
fn test_parser_call_expression() {
    let lexer = Lexer::new("add(1, 2 * 3, 4 + 5);");
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program().unwrap().into_iter();

    assert_eq!(
        program.next(),
        Some(Statement::Expression(ExpressionStatement {
            token: Token::Identifier("add".to_owned()),
            value: Expression::Call(CallExpression {
                token: Token::LParen,
                function: Box::new(Expression::Identifier(Token::Identifier("add".to_owned()))),
                arguments: vec![
                    Expression::IntegerLiteral(Token::Int(1)),
                    Expression::Infix(InfixExpression {
                        operator: Token::Asterisk,
                        left: Box::new(Expression::IntegerLiteral(Token::Int(2))),
                        right: Box::new(Expression::IntegerLiteral(Token::Int(3)))
                    }),
                    Expression::Infix(InfixExpression {
                        operator: Token::Plus,
                        left: Box::new(Expression::IntegerLiteral(Token::Int(4))),
                        right: Box::new(Expression::IntegerLiteral(Token::Int(5)))
                    })
                ]
            })
        }))
    );
    assert_eq!(program.next(), None);
}
