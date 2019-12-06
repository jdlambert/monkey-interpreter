use crate::object::Object;
use crate::ast::{Program, Statement, Expression, Prefix};
use crate::{lexer::Lexer, parser::Parser};
use std::fmt;

pub type Result = std::result::Result<Object, EvalError>;
 
pub enum EvalError {
  Unimplemented,
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::Unimplemented => write!(f, "Unimplemented!"),
        }
    }
}

pub fn eval_input(input: &str) -> Result {
    let lexer = Lexer::new(input.to_owned());
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program().unwrap();
    eval(&program)
} 

fn eval(program: &Program) -> Result {
  match program.statements[0].clone() {
    Statement::Expression(expr) => eval_expression(&expr),
    _ => Err(EvalError::Unimplemented),
  }
}

fn eval_expression(expression: &Expression) -> Result {
  match expression {
    Expression::IntLiteral(val) => Ok(Object::Integer(*val)),
    Expression::Boolean(val) => Ok(Object::Boolean(*val)),
    Expression::Prefix(prefix, expr) => eval_prefix_expression(prefix, expr.as_ref()),
    _ => Err(EvalError::Unimplemented),
  }
}

fn eval_prefix_expression(prefix: &Prefix, expression: &Expression) -> Result {

  let obj = eval_expression(expression)?;

  match prefix {
    Prefix::Bang => Ok(Object::Boolean(!obj.is_truthy())),
    Prefix::Minus => match obj {
      Object::Integer(val) => Ok(Object::Integer(-val)),
      _ => Err(EvalError::Unimplemented),
    }
  }
}

#[cfg(test)]
mod tests {

  use crate::eval;
  
  fn expect_eval(tests: Vec<(&str, &str)>) {
    for (input, expected) in &tests {
        match eval::eval_input(input) {
            Ok(obj) => {
                assert_eq!(obj.to_string(), expected.to_string(), "on input `{}`", input);
            }
            Err(err) => {
                assert_eq!(&err.to_string(), expected, "for `{}`", input);
            }
        }
    }
  }

  #[test]
  fn eval_literals() {
      expect_eval(vec![
          ("5", "5"),
          ("true", "true"),
      ]);
  }

  #[test]
  fn eval_prefixes() {
      expect_eval(vec![
          ("!true", "false"),
          ("!!false", "false"),
          ("-(-(-(-1)))", "1"),
          ("-1", "-1"),
      ]);
  }
  
}