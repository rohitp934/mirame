use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{BlockStatement, Expression, Infix, Prefix, Program, Statement},
    object::{env::Env, EvalError, EvalResult, Object},
};

pub fn eval(program: &Program, env: Rc<RefCell<Env>>) -> EvalResult {
    let mut result = Object::Null;
    for statement in &program.statements {
        result = eval_statement(statement, Rc::clone(&env))?;

        if let Object::ReturnValue(val) = result {
            return Ok(*val);
        }
    }

    Ok(result)
}

fn eval_statement(statement: &Statement, env: Rc<RefCell<Env>>) -> EvalResult {
    match statement {
        Statement::Expression(exp) => eval_expression(exp, env),
        Statement::Return(Some(exp)) => {
            let result = eval_expression(exp, env)?;
            Ok(Object::ReturnValue(Box::new(result)))
        }
        Statement::Return(None) => Ok(Object::ReturnValue(Box::new(Object::Null))),
        Statement::Let(ident, exp) => {
            let result = eval_expression(exp, Rc::clone(&env))?;

            env.borrow_mut().set(ident, result.clone());
            Ok(Object::Null)
        }
        _ => todo!(),
    }
}

fn eval_block_statement(block: &BlockStatement, env: Rc<RefCell<Env>>) -> EvalResult {
    let mut result = Object::Null;
    for statement in &block.statements {
        result = eval_statement(statement, Rc::clone(&env))?;

        if let Object::ReturnValue(_) = result {
            // Not unwrapping the ReturnValue here, because we want to return the ReturnValue
            return Ok(result);
        }
    }

    Ok(result)
}

fn eval_expression(exp: &Expression, env: Rc<RefCell<Env>>) -> EvalResult {
    match exp {
        Expression::IntegerLiteral(val) => Ok(Object::Integer(*val)),
        Expression::Bool(val) => Ok(Object::Bool(*val)),
        Expression::Prefix(prefix, exp) => eval_prefix_exp(prefix, exp.as_ref(), env),
        Expression::Infix(left, infix, right) => {
            eval_infix_exp(left.as_ref(), infix, right.as_ref(), env)
        }
        Expression::If(condition, then, alt) => {
            eval_if_exp(condition.as_ref(), then, alt.as_ref(), env)
        }
        Expression::Identifier(ident) => eval_identifier(ident, env),
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

fn eval_if_exp(
    condition: &Expression,
    then: &BlockStatement,
    alt: Option<&BlockStatement>,
    env: Rc<RefCell<Env>>,
) -> EvalResult {
    let result = eval_expression(condition, Rc::clone(&env))?;

    if result.is_truthy() {
        eval_block_statement(then, env)
    } else {
        alt.map(|a| eval_block_statement(a, env))
            .unwrap_or(Ok(Object::Null))
    }
}

fn eval_identifier(ident: &str, env: Rc<RefCell<Env>>) -> EvalResult {
    if let Some(obj) = env.borrow().get(ident) {
        return Ok(obj.clone());
    }

    Err(EvalError::IdentifierNotFound(ident.to_string()))
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

    #[test]
    fn eval_if() {
        expect_values(vec![
            ("if (true) { 10 }", "10"),
            ("if (false) { 10 }", "null"),
            ("if (1) { 10 }", "10"),
            ("if (1 < 2) { 10 }", "10"),
            ("if (1 > 2) { 10 }", "null"),
            ("if (1 > 2) { 10 } else { 20 }", "20"),
            ("if (1 < 2) { 10 } else { 20 }", "10"),
        ]);
    }

    #[test]
    fn eval_return() {
        expect_values(vec![
            ("return", "null"),
            ("return 10;", "10"),
            ("false; return; 1 + 2;", "null"),
            ("if (true) { return 10; }", "10"),
            ("if (true) { if (true) { return 10; } return 1; }", "10"),
        ])
    }

    #[test]
    fn error_handling() {
        expect_errors(vec![
            ("5 + true", "type mismatch: INTEGER + BOOL"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOL"),
            ("-true;", "unknown operator: -BOOL"),
            ("true + false;", "unknown operator: BOOL + BOOL"),
            ("5; true + false; 5", "unknown operator: BOOL + BOOL"),
            (
                "if (5 < 10) { true + false }",
                "unknown operator: BOOL + BOOL",
            ),
            (
                "if (3 == true) { 1 } else { 2 }",
                "type mismatch: INTEGER == BOOL",
            ),
            ("foobar", "identifier not found: foobar"),
        ])
    }

    #[test]
    fn let_statement() {
        expect_values(vec![
            ("let x = 10; x;", "10"),
            ("let x = 10; let y = 5; x + y;", "15"),
            ("let x = 10; let y = 5; let z = x + y; z;", "15"),
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

    fn expect_errors(tests: Vec<(&str, &str)>) {
        for (input, expected) in &tests {
            match eval_input(input) {
                Ok(obj) => {
                    panic!(
                        "expected error=`{}`, but got `{}` for `{}`",
                        expected, obj, input
                    );
                }
                Err(err) => {
                    assert_eq!(err.to_string(), expected.to_string(), "for `{}`", input);
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
