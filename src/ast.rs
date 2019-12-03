use std::fmt;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(Expression, Expression),
    Return(Expression),
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Prefix {
    Bang,
    Minus,
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Prefix::Bang => write!(f, "!"),
            Prefix::Minus => write!(f, "-"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(String),
    IntLiteral(u32),
    Prefix(Prefix, Box<Expression>),
}
