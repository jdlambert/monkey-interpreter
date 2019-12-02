use crate::ast::{Expression, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use std::mem;

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    ExpectedIdentifierToken(Token),
    ExpectedAssign(Token),
    ExpectedExpression(Token),
    ExpectedStatement(Token),
}

type Result<T> = std::result::Result<T, ParserError>;

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            cur_token: Token::Illegal,
            peek_token: Token::Illegal,
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        self.cur_token = mem::replace(&mut self.peek_token, self.lexer.next_token());
        println!("{:?}", self.cur_token);
    }

    fn expect_token(&mut self, token: Token, expected: fn(Token) -> ParserError) -> Result<()> {
        if self.cur_token != token {
            return Err(expected(self.cur_token.clone()));
        }
        self.next_token();
        Ok(())
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        match self.cur_token {
            Token::Int(i) => {
              self.next_token();
              Ok(Expression::IntLiteral(i))
            },
            _ => Err(ParserError::ExpectedStatement(self.cur_token.clone())),
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression> {
        if let Token::Ident(ident) = self.cur_token.clone() {
            self.next_token();
            Ok(Expression::Identifier(ident))
        } else {
            Err(ParserError::ExpectedIdentifierToken(self.cur_token.clone()))
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
        self.next_token(); // Consume the `let`

        let identifier = self.parse_identifier()?;
        self.expect_token(Token::Assign, ParserError::ExpectedAssign)?;
        let value = self.parse_expression()?;

        if self.cur_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Let(identifier, value))
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.cur_token {
            Token::Let => Ok(self.parse_let_statement()?),
            _ => Err(ParserError::ExpectedStatement(self.cur_token.clone())),
        }
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let mut statements = vec![];

        while self.cur_token != Token::Eof {
            statements.push(self.parse_statement()?);
        }

        Ok(Program { statements })
    }
}

#[cfg(test)]
mod tests {

    use super::Parser;
    use crate::ast::{Expression, Statement};
    use crate::lexer::Lexer;

    #[test]
    fn test_let_statements() {
        let input = r#"let x = 5
                   let y = 10
                   let foobar = 838383"#;

        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();

        assert_eq!(
            program.statements,
            vec![
                Statement::Let(
                    Expression::Identifier("x".to_string()),
                    Expression::IntLiteral(5)
                ),
                Statement::Let(
                    Expression::Identifier("y".to_string()),
                    Expression::IntLiteral(10)
                ),
                Statement::Let(
                    Expression::Identifier("foobar".to_string()),
                    Expression::IntLiteral(838383)
                ),
            ]
        )
    }
}
