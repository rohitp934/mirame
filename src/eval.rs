use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{Expression, Prefix, Program, Statement},
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
        ])
    }

    #[test]
    fn eval_int() {
        expect_values(vec![("5", "5"), ("10", "10")])
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
