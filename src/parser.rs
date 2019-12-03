use crate::ast::{Expression, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use std::mem;

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<ParserError>,
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    UnexpectedEOF,
    ExpectedIdentifier(Token),
    ExpectedSemicolon(Token),
    ExpectedAssign(Token),
    ExpectedExpression(Token),
    ExpectedStatement(Token),
}

#[derive(Debug, PartialEq)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

type Result<T> = std::result::Result<T, ParserError>;
type PrefixParseFn = fn(&mut Parser) -> Result<Expression>;

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            errors: vec![],
            cur_token: Token::Illegal,
            peek_token: Token::Illegal,
        };
        parser.next_token().unwrap();
        parser.next_token().unwrap();
        parser
    }

    fn next_token(&mut self) -> Result<()> {
        if self.cur_token == Token::Eof {
            return Err(ParserError::UnexpectedEOF);
        }
        self.cur_token = mem::replace(&mut self.peek_token, self.lexer.next_token());
        Ok(())
    }

    fn expect_token(&mut self, token: Token, expected: fn(Token) -> ParserError) -> Result<()> {
        if self.cur_token != token {
            return Err(expected(self.cur_token.clone()));
        }
        self.next_token()?;
        Ok(())
    }

    // Prefix parse functions

    fn parse_integer_literal(&mut self) -> Result<Expression> {
        match self.cur_token.clone() {
            Token::Int(i) => {
                self.next_token()?;
                Ok(Expression::IntLiteral(i))
            }
            _ => return Err(ParserError::ExpectedIdentifier(self.cur_token.clone())),
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression> {
        match self.cur_token.clone() {
            Token::Ident(i) => {
                self.next_token()?;
                Ok(Expression::Identifier(i))
            }
            _ => return Err(ParserError::ExpectedIdentifier(self.cur_token.clone())),
        }
    }

    fn prefix_parse_fn(&self) -> Option<PrefixParseFn> {
        match &self.cur_token {
            Token::Ident(_) => Some(Parser::parse_identifier),
            Token::Int(_) => Some(Parser::parse_integer_literal),
            _ => None,
        }
    }

    // Expression, Statement, and Program Parsing

    fn parse_expression(&mut self, _precedence: Precedence) -> Result<Expression> {
        match self.prefix_parse_fn() {
            Some(function) => function(self),
            None => Err(ParserError::ExpectedExpression(self.cur_token.clone())),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
        self.next_token()?; // Consume the `let`

        let identifier = self.parse_identifier()?;
        self.expect_token(Token::Assign, ParserError::ExpectedAssign)?;
        let value = self.parse_expression(Precedence::Lowest)?;

        Ok(Statement::Let(identifier, value))
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        self.next_token()?; // Consume the `return`
        let expression = self.parse_expression(Precedence::Lowest)?;

        Ok(Statement::Return(expression))
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        let statement = match self.cur_token {
            Token::Let => self.parse_let_statement()?,
            Token::Return => self.parse_return_statement()?,
            _ => Statement::Expression(self.parse_expression(Precedence::Lowest)?),
        };
        self.expect_token(Token::Semicolon, ParserError::ExpectedSemicolon)?;
        Ok(statement)
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut statements = vec![];

        while self.cur_token != Token::Eof {
            let statement_result = self.parse_statement();
            match statement_result {
                Ok(statement) => statements.push(statement),
                Err(error) => {
                    // Consume all tokens until the semicolon so as not to get stuck
                    while self.cur_token != Token::Semicolon {
                        self.next_token().unwrap();
                    }
                    self.next_token().unwrap();
                    self.errors.push(error)
                }
            }
        }

        if self.errors.len() > 0 {
            None
        } else {
            Some(Program { statements })
        }
    }
}

#[cfg(test)]
mod tests {

    use super::{Parser, ParserError};
    use crate::ast::{Expression, Statement};
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn test_let_statements() {
        let input = r#"let x = 5;
                   let y = 10;
                   let foobar = 838383;"#;

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

    #[test]
    fn test_bad_let_statements() {
        let input = r#"let x 5; let = 10; let 10100101;"#;

        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);
        assert_eq!(parser.parse_program(), None);
        assert_eq!(
            parser.errors,
            vec![
                ParserError::ExpectedAssign(Token::Int(5)),
                ParserError::ExpectedIdentifier(Token::Assign),
                ParserError::ExpectedIdentifier(Token::Int(10100101)),
            ]
        );
    }

    #[test]
    fn test_return_statements() {
        let input = r#"return 5; return 10; return 42;"#;

        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        assert_eq!(
            program.statements,
            vec![
                Statement::Return(Expression::IntLiteral(5)),
                Statement::Return(Expression::IntLiteral(10)),
                Statement::Return(Expression::IntLiteral(42)),
            ]
        );
    }

    #[test]
    fn test_lone_identifiers() {
        let input = r#"foobar; a; b;"#;

        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        assert_eq!(
            program.statements,
            vec![
                Statement::Expression(Expression::Identifier("foobar".to_string())),
                Statement::Expression(Expression::Identifier("a".to_string())),
                Statement::Expression(Expression::Identifier("b".to_string())),
            ]
        );
    }
}
