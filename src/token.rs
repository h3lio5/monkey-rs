#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Illegal,
    Eof,

    // Identifiers, Literals
    Identifier(String),
    Int(i64),
    String(String),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    // Comparators
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,

    // Delimiters
    Comma,
    Semicolon,
    Colon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Keywords
    Func,
    True,
    False,
    Let,
    If,
    Else,
    Return,
}

impl Token {
    pub fn get_keyword_or_identifier(identifier: &str) -> Token {
        match identifier {
            "fn" => Token::Func,
            "let" => Token::Let,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Identifier(identifier.to_string()),
        }
    }
}
