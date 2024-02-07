use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{Expression, Infix, Prefix, Program, Statement},
    object::{env::Env, EvalError, EvalResult, Object},
};

pub fn eval(program: &Program, env: Rc<RefCell<Env>>) -> EvalResult {
    let mut result = Object::Null;
    for statement in &program.statements {
        result = eval_statement(statement, Rc::clone(&env))?;
    }

    Ok(result)
}

fn eval_statement(statement: &Statement, env: Rc<RefCell<Env>>) -> EvalResult {
    match statement {
        Statement::Expression(exp) => eval_expression(exp, env),
        _ => todo!(),
    }
}

fn eval_expression(exp: &Expression, env: Rc<RefCell<Env>>) -> EvalResult {
    match exp {
        Expression::IntegerLiteral(val) => Ok(Object::Integer(*val)),
        Expression::Bool(val) => Ok(Object::Bool(*val)),
        Expression::Prefix(prefix, exp) => eval_prefix_exp(prefix, exp.as_ref(), env),
        Expression::Infix(left, infix, right) => {
            eval_infix_exp(left.as_ref(), infix, right.as_ref(), env)
        }
        _ => todo!(),
    }
}

fn eval_prefix_exp(prefix: &Prefix, exp: &Expression, env: Rc<RefCell<Env>>) -> EvalResult {
    let obj = eval_expression(exp, env)?;

    match prefix {
        Prefix::Bang => Ok(Object::Bool(!obj.is_truthy())),
        Prefix::Minus => match obj {
            Object::Integer(val) => Ok(Object::Integer(-val)),
            _ => Err(EvalError::UnknownPrefixOperator(prefix.clone(), obj)),
        },
        Prefix::Inc => match obj {
            Object::Integer(val) => Ok(Object::Integer(val + 1)),
            _ => Err(EvalError::UnknownPrefixOperator(prefix.clone(), obj)),
        },
        Prefix::Dec => match obj {
            Object::Integer(val) => Ok(Object::Integer(val - 1)),
            _ => Err(EvalError::UnknownPrefixOperator(prefix.clone(), obj)),
        },
    }
}

fn eval_infix_exp(
    left: &Expression,
    infix: &Infix,
    right: &Expression,
    env: Rc<RefCell<Env>>,
) -> EvalResult {
    let left_obj = eval_expression(left, Rc::clone(&env))?;
    let right_obj = eval_expression(right, Rc::clone(&env))?;

    match (left_obj, right_obj) {
        (Object::Bool(left), Object::Bool(right)) => eval_boolean_infix_exp(left, infix, right),
        (Object::Integer(left), Object::Integer(right)) => {
            eval_integer_infix_exp(left, infix, right)
        }
        (left, right) => Err(EvalError::TypeMismatch(left, infix.clone(), right)),
    }
}

fn eval_boolean_infix_exp(left: bool, infix: &Infix, right: bool) -> EvalResult {
    match infix {
        Infix::Eq => Ok(Object::Bool(left == right)),
        Infix::Neq => Ok(Object::Bool(left != right)),
        _ => Err(EvalError::UnknownInfixOperator(
            Object::Bool(left),
            infix.clone(),
            Object::Bool(right),
        )),
    }
}

fn eval_integer_infix_exp(left: i64, infix: &Infix, right: i64) -> EvalResult {
    match infix {
        Infix::Plus => Ok(Object::Integer(left + right)),
        Infix::Minus => Ok(Object::Integer(left - right)),
        Infix::Asterisk => Ok(Object::Integer(left * right)),
        Infix::Slash => Ok(Object::Integer(left / right)),
        Infix::Eq => Ok(Object::Bool(left == right)),
        Infix::Neq => Ok(Object::Bool(left != right)),
        Infix::Leq => Ok(Object::Bool(left <= right)),
        Infix::Geq => Ok(Object::Bool(left >= right)),
        Infix::Lt => Ok(Object::Bool(left < right)),
        Infix::Gt => Ok(Object::Bool(left > right)),
    }
}

#[cfg(test)]
mod eval_tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        eval,
        lexer::Lexer,
        object::{env::Env, EvalResult},
        parser::Parser,
    };

    #[test]
    fn eval_bool() {
        expect_values(vec![
            ("false", "false"),
            ("true", "true"),
            // Prefix
            ("!true", "false"),
            ("!false", "true"),
            ("!!true", "true"),
            ("!!false", "false"),
            // Infix
            ("true == true", "true"),
            ("true == false", "false"),
            ("true != true", "false"),
            ("true != false", "true"),
            ("1 == 2", "false"),
            ("1 != 2", "true"),
            ("1 > 2", "false"),
            ("1 < 2", "true"),
        ])
    }

    #[test]
    fn eval_int() {
        expect_values(vec![
            ("5", "5"),
            ("10", "10"),
            // Prefix
            ("-5", "-5"),
            ("-10", "-10"),
            ("-(-10)", "10"),
            // Infix
            ("5 + 5", "10"),
            ("5 - 5", "0"),
            ("5 * 5", "25"),
            ("5 / 5", "1"),
            ("100 + 100 - 200", "0"),
            ("5 * (3 + 2)", "25"),
        ])
    }

    fn expect_values(tests: Vec<(&str, &str)>) {
        for (input, expected) in tests {
            match eval_input(input) {
                Ok(obj) => {
                    assert_eq!(obj.to_string(), expected.to_string(), "for `{}`", input);
                }
                Err(err) => {
                    panic!(
                        "expected `{}`, but got error=`{}` for `{}`",
                        expected, err, input
                    );
                }
            }
        }
    }

    fn eval_input(input: &str) -> EvalResult {
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let env = Rc::new(RefCell::new(Env::new()));
        eval::eval(&program, env)
    }
}
