#[derive(Debug, PartialEq)]
pub struct Program {
  pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
  Let(Expression, Expression),
  Return(Option<Expression>),
  Expression(Expression)
}

#[derive(Debug, PartialEq)]
pub enum Expression {
  Identifier(String),
  IntLiteral(u32),
}


