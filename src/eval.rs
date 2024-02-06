use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{Expression, Program, Statement},
    object::{env::Env, EvalResult, Object},
};

pub fn eval(program: &Program, env: Rc<RefCell<Env>>) -> EvalResult {
    let mut result = Object::Null;
    for statement in &program.statements {
        result = eval_statement(statement, Rc::clone(&env));
    }

    Ok(result)
}

fn eval_statement(statement: &Statement, env: Rc<RefCell<Env>>) -> Object {
    match statement {
        Statement::Expression(exp) => eval_expression(exp, env),
        _ => todo!(),
    }
}

fn eval_expression(exp: &Expression, env: Rc<RefCell<Env>>) -> Object {
    match exp {
        Expression::IntegerLiteral(val) => Object::Integer(*val),
        Expression::Bool(val) => Object::Bool(*val),
        _ => todo!(),
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
        expect_values(vec![("false", "false"), ("true", "true")])
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
