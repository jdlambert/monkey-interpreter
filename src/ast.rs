use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(Expression, Expression),
    Return(Expression),
    Expression(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(left, right) => write!(f, "let {} = {};", left, right),
            Statement::Return(expr) => write!(f, "return {};", expr),
            Statement::Expression(expr) => write!(f, "{};", expr),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{ ")?;
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        write!(f, " }}")?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Prefix {
    Bang,
    Minus,
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Prefix::Bang => write!(f, "!"),
            Prefix::Minus => write!(f, "-"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
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

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Infix::Equal => write!(f, "=="),
            Infix::NotEqual => write!(f, "!="),
            Infix::Lt => write!(f, "<"),
            Infix::Gt => write!(f, ">"),
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Asterisk => write!(f, "*"),
            Infix::Slash => write!(f, "/"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(String),
    IntLiteral(i64),
    Boolean(bool),
    Prefix(Prefix, Box<Expression>),
    Infix(Infix, Box<Expression>, Box<Expression>),
    If(Box<Expression>, BlockStatement, Option<BlockStatement>),
    Function(Vec<String>, BlockStatement),
    Call(Box<Expression>, Vec<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(ident) => write!(f, "{}", ident),
            Expression::IntLiteral(value) => write!(f, "{}", value),
            Expression::Boolean(value) => write!(f, "{}", value),
            Expression::Prefix(operator, exp) => write!(f, "({}{})", operator, exp),
            Expression::Infix(operator, left, right) => {
                write!(f, "({} {} {})", left, operator, right)
            }
            Expression::If(cond, then, alt) => {
                write!(f, "if {} {}", cond, then)?;
                if let Some(block) = alt {
                    write!(f, " else {}", block)?;
                }
                Ok(())
            }
            Expression::Function(parameters, body) => {
                write!(f, "fn ({}) {}", parameters.join(", "), body)
            }
            Expression::Call(name, parameters) => {
                let parameters = parameters
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{}({})", name, parameters)
            }
        }
    }
}
