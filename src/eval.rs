use crate::ast::{BlockStatement, Expression, Infix, Prefix, Program, Statement};
use crate::object::Object;
use crate::{lexer::Lexer, parser::Parser, environment::Environment};
use std::fmt;

pub type Result = std::result::Result<Object, EvalError>;

pub enum EvalError {
    Unimplemented,
    TypeMismatch(Infix, Object, Object),
    InvalidLValue(Expression),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::Unimplemented => write!(f, "Unimplemented!"),
            EvalError::TypeMismatch(infix, left, right) => {
                write!(f, "Type mismatch: {} {} {}", left, infix, right)
            }
            EvalError::InvalidLValue(expr)=> write!(f, "Invalid L Value {}!", expr),
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
    let env = Environment::new();
    eval_statements(&program.statements, &env)
}

fn eval_statements(statements: &Vec<Statement>, env: &Environment) -> Result {
    let mut result = Object::Null;
    for statement in statements {
        result = match statement {
            Statement::Expression(expr) => eval_expression(&expr, &env)?,
            Statement::Return(expr) => {
                return match expr {
                    None => Ok(Object::Null),
                    Some(expr) => eval_expression(&expr, &env),
                }
            },
            Statement::Let(left, right) => eval_let_statement(&left, &right, &env)?,
        }
    }
    Ok(result)
}

fn eval_let_statement(left: &Expression, right: &Expression, env: &Environment) -> Result {
    let right = eval_expression(right, env)?;
    match left {
        Expression::Identifier(name) => env.set(&name, right.clone()),
        _ => return Err(EvalError::InvalidLValue(left.clone())),
    }
    Ok(right)
}

fn eval_expression(expression: &Expression, env: &Environment) -> Result {
    match expression {
        Expression::IntLiteral(val) => Ok(Object::Integer(*val)),
        Expression::Boolean(val) => Ok(Object::Boolean(*val)),
        Expression::Prefix(prefix, expr) => eval_prefix_expression(prefix, expr.as_ref(), env),
        Expression::Infix(infix, left, right) => {
            eval_infix_expression(infix, left.as_ref(), right.as_ref(), env)
        }
        Expression::If(condition, then, alt) => eval_if_expression(condition, then, alt, env),
        Expression::Identifier(ident) => eval_identifier(ident, env),
        _ => Err(EvalError::Unimplemented),
    }
}

fn eval_identifier(ident: &str, env: &Environment) -> Result {
    match env.get(ident) {
        Some(value) => Ok(value),
        None => Ok(Object::Null),
    }
}

fn eval_if_expression(
    condition: &Expression,
    then: &BlockStatement,
    alt: &Option<BlockStatement>,
    env: &Environment,
) -> Result {
    let condition = eval_expression(condition, env)?;

    Ok(if condition.is_truthy() {
        eval_statements(&then.statements, env)?
    } else {
        match alt {
            None => Object::Null,
            Some(block) => eval_statements(&block.statements, env)?,
        }
    })
}

fn eval_infix_expression(infix: &Infix, left: &Expression, right: &Expression, env: &Environment) -> Result {
    let left_obj = eval_expression(left, env)?;
    let right_obj = eval_expression(right, env)?;

    match (left_obj, right_obj) {
        (Object::Boolean(left), Object::Boolean(right)) => {
            eval_boolean_infix_expression(infix, left, right)
        }
        (Object::Integer(left), Object::Integer(right)) => {
            eval_integer_infix_expression(infix, left, right)
        }
        (left, right) => Err(EvalError::TypeMismatch(infix.clone(), left, right)),
    }
}

fn eval_boolean_infix_expression(infix: &Infix, left: bool, right: bool) -> Result {
    Ok(match infix {
        Infix::Equal => Object::Boolean(left == right),
        Infix::NotEqual => Object::Boolean(left != right),
        _ => {
            return Err(EvalError::TypeMismatch(
                infix.clone(),
                Object::Boolean(left),
                Object::Boolean(right),
            ))
        }
    })
}

fn eval_integer_infix_expression(infix: &Infix, left: i64, right: i64) -> Result {
    Ok(match infix {
        Infix::Plus => Object::Integer(left + right),
        Infix::Minus => Object::Integer(left - right),
        Infix::Asterisk => Object::Integer(left * right),
        Infix::Slash => Object::Integer(left / right),
        Infix::Equal => Object::Boolean(left == right),
        Infix::NotEqual => Object::Boolean(left != right),
        Infix::Lt => Object::Boolean(left < right),
        Infix::Gt => Object::Boolean(left > right),
    })
}

fn eval_prefix_expression(prefix: &Prefix, expression: &Expression, env: &Environment) -> Result {
    let obj = eval_expression(expression, env)?;

    match prefix {
        Prefix::Bang => Ok(Object::Boolean(!obj.is_truthy())),
        Prefix::Minus => match obj {
            Object::Integer(val) => Ok(Object::Integer(-val)),
            _ => Err(EvalError::Unimplemented),
        },
    }
}

#[cfg(test)]
mod tests {

    use crate::eval;

    fn expect_eval(tests: Vec<(&str, &str)>) {
        for (input, expected) in &tests {
            match eval::eval_input(input) {
                Ok(obj) => {
                    assert_eq!(
                        obj.to_string(),
                        expected.to_string(),
                        "on input `{}`",
                        input
                    );
                }
                Err(err) => {
                    assert_eq!(&err.to_string(), expected, "for `{}`", input);
                }
            }
        }
    }

    #[test]
    fn eval_literals() {
        expect_eval(vec![("5", "5"), ("true", "true")]);
    }

    #[test]
    fn eval_prefixes() {
        expect_eval(vec![
            ("!true", "false"),
            ("!!false", "false"),
            ("-(-(-(-1)))", "1"),
            ("-1", "-1"),
            ("!!5", "true"),
            ("!5", "false"),
        ]);
    }

    #[test]
    fn eval_infixes() {
        expect_eval(vec![
            ("5 + 10 - 2", "13"),
            ("50 / 2 * 2 + 10", "60"),
            ("2 * (5 + 10)", "30"),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", "50"),
            ("5 > 3", "true"),
            ("5 == 5", "true"),
            ("(5 == 5) != (3 != 2)", "false"),
            ("true == false", "false"),
        ]);
    }

    #[test]
    fn eval_conditionals() {
        expect_eval(vec![
            ("if (false) { 10 }", "null"),
            ("if (5 > 10) { 1 } else { 2 }", "2"),
        ]);
    }

    #[test]
    fn eval_returns() {
        expect_eval(vec![("1 + 1; return 2; 3 + 3", "2")]);
    }

    #[test]
    fn eval_bindings() {
        expect_eval(vec![("x", "null"), ("let x = 1; x", "1"), ("let x = 10; let y = 42; x + y", "52")]);
    }
}
