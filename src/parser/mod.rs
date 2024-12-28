use super::lexer::Lexer;
use super::token::Token;


#[derive(Debug, Clone)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Option<Token>,
    peek_token: Option<Token>
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {

        let mut parser = Parser {
            lexer,
            cur_token: None,
            peek_token: None
        };

        parser.advance();
        parser.advance();

        parser
    }

    fn advance(&mut self) {
        self.cur_token = self.peek_token.take();
        self.peek_token = Some(self.lexer.next_token());
    }

    fn parse_program(&mut self) -> Program {

        let mut program = 
        todo!()
    }
    
}
