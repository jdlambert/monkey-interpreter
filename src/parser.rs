use crate::ast::{BlockStatement, Expression, Infix, Prefix, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use std::convert::TryFrom;

type Result<T> = std::result::Result<T, ParserError>;
type PrefixParseFn = fn(&mut Parser) -> Result<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Result<Expression>;

#[derive(Debug, PartialEq, Clone)]
pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    pub errors: Vec<ParserError>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParserError {
    UnexpectedEOF,
    ExpectedBoolean(Token),
    ExpectedIdentifier(Token),
    ExpectedSemicolon(Token),
    ExpectedAssign(Token),
    ExpectedExpression(Token),
    ExpectedStatement(Token),
    ExpectedInfix(Token),
    ExpectedLparen(Token),
    ExpectedRparen(Token),
    ExpectedLbrace(Token),
    ExpectedRbrace(Token),
    ExpectedRbracket(Token),
    ExpectedComma(Token),
    ExpectedColon(Token),
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
    Index,
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
            Token::Lparen => Precedence::Call,
            Token::Lbracket => Precedence::Index,
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
        Token::Int(_) => Some(Parser::parse_literal),
        Token::String(_) => Some(Parser::parse_literal),
        Token::Lbracket => Some(Parser::parse_array),
        Token::Lbrace => Some(Parser::parse_hash),
        Token::True => Some(Parser::parse_literal),
        Token::False => Some(Parser::parse_literal),
        Token::Bang => Some(Parser::parse_prefix_bang),
        Token::Minus => Some(Parser::parse_prefix_minus),
        Token::Lparen => Some(Parser::parse_paren_expression),
        Token::If => Some(Parser::parse_if_expression),
        Token::Function => Some(Parser::parse_function_literal),
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
        Token::Lbracket => Some(Parser::parse_index),
        Token::Lt => Some(Parser::parse_infix_expression),
        Token::Gt => Some(Parser::parse_infix_expression),
        Token::Lparen => Some(Parser::parse_call_expression),
        _ => None,
    }
}

impl Parser {
    pub fn from_input(input: String) -> Self {
        Parser::new(Lexer::new(input))
    }

    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            errors: vec![],
            cur_token: Token::Illegal,
        };
        parser.next_token().unwrap();
        parser
    }

    fn next_token(&mut self) -> Result<()> {
        if self.cur_token == Token::Eof {
            return Err(ParserError::UnexpectedEOF);
        }
        self.cur_token = self.lexer.next_token();
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

    fn parse_literal(&mut self) -> Result<Expression> {
        match self.cur_token.clone() {
            Token::Int(i) => {
                self.next_token()?;
                Ok(Expression::IntLiteral(i))
            }
            Token::String(s) => {
                self.next_token()?;
                Ok(Expression::String(s))
            }
            Token::True => {
                self.next_token()?;
                Ok(Expression::Boolean(true))
            }
            Token::False => {
                self.next_token()?;
                Ok(Expression::Boolean(false))
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

    fn parse_block_statement(&mut self) -> Result<BlockStatement> {
        let mut statements = vec![];

        self.expect_token(Token::Lbrace, ParserError::ExpectedLbrace)?;
        while self.cur_token != Token::Rbrace {
            statements.push(self.parse_statement()?);
        }
        self.next_token()?; // Consume the `}`

        Ok(BlockStatement { statements })
    }

    fn parse_function_params(&mut self) -> Result<Vec<String>> {
        let mut parameters = vec![];
        self.expect_token(Token::Lparen, ParserError::ExpectedLparen)?;

        if self.cur_token != Token::Rparen {
            if let Expression::Identifier(i) = self.parse_identifier()? {
                parameters.push(i);
            }

            while self.cur_token == Token::Comma {
                self.next_token()?; // Consume the `,`
                if let Expression::Identifier(i) = self.parse_identifier()? {
                    parameters.push(i);
                }
            }
        }

        self.expect_token(Token::Rparen, ParserError::ExpectedRparen)?;

        Ok(parameters)
    }

    fn parse_index(&mut self, left: Expression) -> Result<Expression> {
        self.next_token()?; // Consume the `[`
        let index = self.parse_expression(Precedence::Lowest)?;
        self.expect_token(Token::Rbracket, ParserError::ExpectedRbracket)?;
        Ok(Expression::Index(Box::new(left), Box::new(index)))
    }

    fn parse_array(&mut self) -> Result<Expression> {
        self.next_token()?; // Consume the `[`

        Ok(Expression::Array(self.parse_expressions(
            Token::Rbracket,
            ParserError::ExpectedRbracket,
        )?))
    }

    fn parse_hash(&mut self) -> Result<Expression> {
        self.next_token()?; // Consume the `{`
        let mut kvs = vec![];

        if self.cur_token != Token::Rbrace {
            let key = self.parse_expression(Precedence::Lowest)?;
            self.expect_token(Token::Colon, ParserError::ExpectedColon)?;
            let value = self.parse_expression(Precedence::Lowest)?;
            kvs.push((key, value));
            while self.cur_token == Token::Comma {
                self.next_token()?; // Consume the `,`
                let key = self.parse_expression(Precedence::Lowest)?;
                self.expect_token(Token::Colon, ParserError::ExpectedColon)?;
                let value = self.parse_expression(Precedence::Lowest)?;
                kvs.push((key, value));
            }
        }
        self.expect_token(Token::Rbrace, ParserError::ExpectedRbrace)?;

        Ok(Expression::Hash(kvs))
    }

    fn parse_function_literal(&mut self) -> Result<Expression> {
        self.next_token()?; // Consume the `fn`

        let parameters = self.parse_function_params()?;
        let body = self.parse_block_statement()?;

        Ok(Expression::Function(parameters, body))
    }

    fn parse_expressions(
        &mut self,
        end_token: Token,
        expected: fn(Token) -> ParserError,
    ) -> Result<Vec<Expression>> {
        let mut expressions = vec![];

        if self.cur_token != end_token {
            expressions.push(self.parse_expression(Precedence::Lowest)?);
            while self.cur_token == Token::Comma {
                self.next_token()?; // Consume the `,`
                expressions.push(self.parse_expression(Precedence::Lowest)?);
            }
        }
        self.expect_token(end_token, expected)?;

        Ok(expressions)
    }

    fn parse_call_expression(&mut self, left: Expression) -> Result<Expression> {
        self.next_token()?; // Consume the `(`

        let parameters = self.parse_expressions(Token::Rparen, ParserError::ExpectedRparen)?;

        Ok(Expression::Call(Box::new(left), parameters))
    }

    fn parse_if_expression(&mut self) -> Result<Expression> {
        self.next_token()?; // Consume the `if`

        self.expect_token(Token::Lparen, ParserError::ExpectedLparen)?;
        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_token(Token::Rparen, ParserError::ExpectedRparen)?;

        let consequence = self.parse_block_statement()?;

        let alternative = if self.cur_token == Token::Else {
            self.next_token()?; // Consume the `else`
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(Expression::If(
            Box::new(condition),
            consequence,
            alternative,
        ))
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

        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_token(Token::Assign, ParserError::ExpectedAssign)?;
        let value = self.parse_expression(Precedence::Lowest)?;

        Ok(Statement::Let(expr, value))
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        self.next_token()?; // Consume the `return`

        Ok(if self.cur_token == Token::Semicolon {
            Statement::Return(None)
        } else {
            let expression = self.parse_expression(Precedence::Lowest)?;
            Statement::Return(Some(expression))
        })
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

    use super::Parser;

    fn test_parsing(tests: Vec<(&str, &str)>) {
        for (input, expected) in tests {
            let mut parser = Parser::from_input(input.to_string());

            let program = parser.parse_program().unwrap();

            assert_eq!(program.to_string(), expected);
        }
    }

    #[test]
    fn test_let_statements() {
        test_parsing(vec![
            ("let x = 5;", "let x = 5;"),
            ("let y = 10;", "let y = 10;"),
            ("let foobar = 83838383", "let foobar = 83838383;"),
        ]);
    }

    #[test]
    fn test_return_statements() {
        test_parsing(vec![
            ("return;", "return;"),
            ("return 42;", "return 42;"),
            ("return x", "return x;"),
            ("return x return 2 * 3", "return x;return (2 * 3);"),
            ("return 2 * 4 + 5;", "return ((2 * 4) + 5);"),
        ]);
    }

    #[test]
    fn test_lone_identifiers() {
        test_parsing(vec![("foobar;", "foobar;"), ("a", "a;"), ("b;", "b;")]);
    }

    #[test]
    fn test_prefix_expressions() {
        test_parsing(vec![("!5;", "(!5);"), ("-15;", "(-15);")]);
    }

    #[test]
    fn test_booleans() {
        test_parsing(vec![("true false; false true", "true;false;false;true;")]);
    }

    #[test]
    fn test_strings() {
        test_parsing(vec![(
            r#""string!""another string!""#,
            r#""string!";"another string!";"#,
        )]);
    }

    #[test]
    fn test_infix_expressions() {
        test_parsing(vec![
            ("5 + 5;", "(5 + 5);"),
            ("5 - 5;", "(5 - 5);"),
            ("5 * 5;", "(5 * 5);"),
            ("5 / 5;", "(5 / 5);"),
            ("5 > 5;", "(5 > 5);"),
            ("5 < 5;", "(5 < 5);"),
            ("5 == 5;", "(5 == 5);"),
            ("5 != 5;", "(5 != 5);"),
        ]);
    }

    #[test]
    fn test_operator_precedence() {
        test_parsing(vec![
            ("-a * b", "((-a) * b);"),
            ("!-a", "(!(-a));"),
            ("a + b + c", "((a + b) + c);"),
            ("a + b - c", "((a + b) - c);"),
            ("a * b * c", "((a * b) * c);"),
            ("a * b / c", "((a * b) / c);"),
            ("a + b / c", "(a + (b / c));"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f);"),
            ("3 + 4; -5 * 5", "(3 + 4);((-5) * 5);"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4));"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4));"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
            ),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4);"),
            ("(5 + 5) * 2", "((5 + 5) * 2);"),
            ("2 / (5 + 5)", "(2 / (5 + 5));"),
            ("-(5 + 5)", "(-(5 + 5));"),
        ]);
    }

    #[test]
    fn test_conditional_expressions() {
        test_parsing(vec![
            ("if (x < y) { x }", "if (x < y) { x; };"),
            (
                "if (x < y) { x } else { z }",
                "if (x < y) { x; } else { z; };",
            ),
            (
                "if (x < y) { return x; } else { return z; }",
                "if (x < y) { return x; } else { return z; };",
            ),
        ]);
    }

    #[test]
    fn test_function_literals() {
        test_parsing(vec![(
            "fn (x, y, z) { return x + 10 * y }",
            "fn (x, y, z) { return (x + (10 * y)); };",
        )]);
    }

    #[test]
    fn test_calls() {
        test_parsing(vec![("add(x, y, z)", "add(x, y, z);")]);
    }

    #[test]
    fn test_index_ops() {
        test_parsing(vec![("a[1]", "a[1];")]);
    }

    #[test]
    fn test_hashes() {
        test_parsing(vec![("{10: 12}", "{10: 12};")]);
    }
}
