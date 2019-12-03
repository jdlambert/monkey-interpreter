use crate::ast::{Expression, Infix, Prefix, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use std::convert::TryFrom;
use std::mem;

type Result<T> = std::result::Result<T, ParserError>;
type PrefixParseFn = fn(&mut Parser) -> Result<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Result<Expression>;

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
    ExpectedInfix(Token),
    ExpectedRparen(Token),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl From<&Token> for Precedence {
    fn from(token: &Token) -> Self {
        match token {
            Token::Equal => Precedence::Equals,
            Token::NotEqual => Precedence::Equals,
            Token::Lt => Precedence::LessGreater,
            Token::Gt => Precedence::LessGreater,
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Slash => Precedence::Product,
            Token::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
}

impl TryFrom<&Token> for Infix {
    type Error = &'static str;

    fn try_from(token: &Token) -> std::result::Result<Self, Self::Error> {
        Ok(match token {
            Token::Equal => Infix::Equal,
            Token::NotEqual => Infix::NotEqual,
            Token::Lt => Infix::Lt,
            Token::Gt => Infix::Gt,
            Token::Plus => Infix::Plus,
            Token::Minus => Infix::Minus,
            Token::Slash => Infix::Slash,
            Token::Asterisk => Infix::Asterisk,
            _ => return Err(&"No matching infix!"),
        })
    }
}

fn prefix_parse_fn(token: &Token) -> Option<PrefixParseFn> {
    match &token {
        Token::Ident(_) => Some(Parser::parse_identifier),
        Token::Int(_) => Some(Parser::parse_integer_literal),
        Token::Bang => Some(Parser::parse_prefix_bang),
        Token::Minus => Some(Parser::parse_prefix_minus),
        Token::Lparen => Some(Parser::parse_paren_expression),
        _ => None,
    }
}

fn infix_parse_fn(token: &Token) -> Option<InfixParseFn> {
    match token {
        Token::Plus => Some(Parser::parse_infix_expression),
        Token::Minus => Some(Parser::parse_infix_expression),
        Token::Asterisk => Some(Parser::parse_infix_expression),
        Token::Slash => Some(Parser::parse_infix_expression),
        Token::Equal => Some(Parser::parse_infix_expression),
        Token::NotEqual => Some(Parser::parse_infix_expression),
        Token::Lt => Some(Parser::parse_infix_expression),
        Token::Gt => Some(Parser::parse_infix_expression),
        _ => None,
    }
}

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

    fn parse_prefix_minus(&mut self) -> Result<Expression> {
        match self.cur_token.clone() {
            Token::Minus => {
                self.next_token()?;
                Ok(Expression::Prefix(
                    Prefix::Minus,
                    Box::new(self.parse_expression(Precedence::Prefix)?),
                ))
            }
            _ => return Err(ParserError::ExpectedIdentifier(self.cur_token.clone())),
        }
    }

    fn parse_prefix_bang(&mut self) -> Result<Expression> {
        match self.cur_token.clone() {
            Token::Bang => {
                self.next_token()?;
                Ok(Expression::Prefix(
                    Prefix::Bang,
                    Box::new(self.parse_expression(Precedence::Prefix)?),
                ))
            }
            _ => return Err(ParserError::ExpectedIdentifier(self.cur_token.clone())),
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression> {
        match Infix::try_from(&self.cur_token) {
            Ok(infix) => {
                let precedence = Precedence::from(&self.cur_token);
                self.next_token()?;
                let right = self.parse_expression(precedence)?;
                Ok(Expression::Infix(infix, Box::new(left), Box::new(right)))
            }
            Err(_) => Err(ParserError::ExpectedInfix(self.cur_token.clone())),
        }
    }

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

    fn parse_paren_expression(&mut self) -> Result<Expression> {
        
        self.next_token()?; // Consume the left parenthesis
        let expression = self.parse_expression(Precedence::Lowest)?;
        self.expect_token(Token::Rparen, ParserError::ExpectedRparen)?;

        Ok(expression)
    }

    // Expression, Statement, and Program Parsing

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        let mut left = match prefix_parse_fn(&self.cur_token) {
            Some(parse_function) => parse_function(self)?,
            None => return Err(ParserError::ExpectedExpression(self.cur_token.clone())),
        };

        while self.cur_token != Token::Semicolon && precedence < Precedence::from(&self.cur_token) {
            left = match infix_parse_fn(&self.cur_token) {
                Some(parse_function) => parse_function(self, left)?,
                None => return Ok(left),
            };
        }
        Ok(left)
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
        if self.cur_token == Token::Semicolon {
            self.next_token()?;
        }
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
    use crate::ast::{Expression, Infix, Prefix, Statement};
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

    #[test]
    fn test_prefix_expressions() {
        let tests = vec![
            ("!5;", Prefix::Bang, Expression::IntLiteral(5)),
            ("-15;", Prefix::Minus, Expression::IntLiteral(15)),
        ];
        for (input, operator, value) in tests {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program().unwrap();

            assert_eq!(
                program.statements,
                vec![Statement::Expression(Expression::Prefix(
                    operator,
                    Box::new(value),
                ))]
            );
        }
    }

    #[test]
    fn test_infix_expressions() {
        let tests = vec![
            ("5 + 5;", 5, Infix::Plus, 5),
            ("5 - 5;", 5, Infix::Minus, 5),
            ("5 * 5;", 5, Infix::Asterisk, 5),
            ("5 / 5;", 5, Infix::Slash, 5),
            ("5 > 5;", 5, Infix::Gt, 5),
            ("5 < 5;", 5, Infix::Lt, 5),
            ("5 == 5;", 5, Infix::Equal, 5),
            ("5 != 5;", 5, Infix::NotEqual, 5),
        ];
        for (input, left, operator, right) in tests {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program().unwrap();

            assert_eq!(
                program.statements,
                vec![Statement::Expression(Expression::Infix(
                    operator,
                    Box::new(Expression::IntLiteral(left)),
                    Box::new(Expression::IntLiteral(right))
                ))]
            );
        }
    }
}
