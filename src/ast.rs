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

#[derive(Debug, PartialEq)]
pub enum Infix {
    Plus,
    Minus,
    Slash,
    Asterisk,
    Lt,
    Gt,
    Equal,
    NotEqual,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(String),
    IntLiteral(u32),
    Prefix(Prefix, Box<Expression>),
    Infix(Infix, Box<Expression>, Box<Expression>),
}
