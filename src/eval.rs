use crate::object::Object;
use crate::ast::{Program, Statement, Expression};

type Result = std::result::Result<Object, &'static str>;

fn eval(program: &Program) -> Result {
  match program.statements[0].clone() {
    Statement::Expression(expr) => eval_expression(expr),
    _ => Err("unimplemented")
  }
}

fn eval_expression(expression: Expression) -> Result {
  match expression {
    Expression::IntLiteral(val) => Ok(Object::Integer(val)),
    _ => Err("unimplemented")
  }
}

#[cfg(test)]
mod tests {

  use crate::{eval, lexer::Lexer, parser::Parser};

  #[test]
  fn eval_integer() {
      expect_eval(vec![
          ("5", "6"),
      ]);
  }
  
  fn eval(input: &str) -> super::Result {
    let lexer = Lexer::new(input.to_owned());
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program().unwrap();
    eval::eval(&program)
  } 

  fn expect_eval(tests: Vec<(&str, &str)>) {
    for (input, expected) in &tests {
        match eval(input) {
            Ok(obj) => {
                assert_eq!(obj.to_string(), expected.to_string(), "on input `{}`", input);
            }
            Err(err) => {
                assert_eq!(&err, expected, "for `{}`", input);
            }
        }
    }
  }

}