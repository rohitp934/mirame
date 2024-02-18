use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{BlockStatement, Expression, Infix, Prefix, Program, Statement},
    object::{assert_arg_count, builtins, env::Env, EvalError, EvalResult, Object},
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
        Expression::FloatLiteral(val) => Ok(Object::Float(*val)),
        Expression::StringLiteral(val) => Ok(Object::String(val.clone())),
        Expression::Bool(val) => Ok(Object::Bool(*val)),
        Expression::Prefix(prefix, exp) => eval_prefix_exp(prefix, exp.as_ref(), env),
        Expression::Infix(left, infix, right) => {
            eval_infix_exp(left.as_ref(), infix, right.as_ref(), env)
        }
        Expression::If(condition, then, alt) => {
            eval_if_exp(condition.as_ref(), then, alt.as_ref(), env)
        }
        Expression::Identifier(ident) => eval_identifier(ident, env),
        Expression::Function(params, body) => {
            Ok(Object::Function(params.to_vec(), body.clone(), env))
        }
        Expression::Call(func, args) => {
            let func = eval_expression(func, Rc::clone(&env))?;
            let arguments = eval_expressions(args, env)?;
            apply_function(func, arguments)
        }
        Expression::Array(elements) => eval_array_literal(elements, env),
        Expression::Index(left, index) => eval_index_exp(left, index, env),
        _ => todo!(),
    }
}

fn eval_prefix_exp(prefix: &Prefix, exp: &Expression, env: Rc<RefCell<Env>>) -> EvalResult {
    let obj = eval_expression(exp, env)?;

    match prefix {
        Prefix::Bang => Ok(Object::Bool(!obj.is_truthy())),
        Prefix::Minus => match obj {
            Object::Integer(val) => Ok(Object::Integer(-val)),
            Object::Float(val) => Ok(Object::Float(-val)),
            _ => Err(EvalError::UnknownPrefixOperator(prefix.clone(), obj)),
        },
        Prefix::Inc => match obj {
            Object::Integer(val) => Ok(Object::Integer(val + 1)),
            Object::Float(val) => Ok(Object::Float(val + 1.0)),
            _ => Err(EvalError::UnknownPrefixOperator(prefix.clone(), obj)),
        },
        Prefix::Dec => match obj {
            Object::Integer(val) => Ok(Object::Integer(val - 1)),
            Object::Float(val) => Ok(Object::Float(val - 1.0)),
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
        (Object::Float(left), Object::Float(right)) => eval_float_infix_exp(left, infix, right),
        (Object::Float(left), Object::Integer(right)) => {
            eval_float_infix_exp(left, infix, right as f64)
        }
        (Object::Integer(left), Object::Float(right)) => {
            eval_float_infix_exp(left as f64, infix, right)
        }
        (Object::String(left), Object::String(right)) => eval_string_infix_exp(left, infix, right),
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

fn eval_array_literal(elements: &[Expression], env: Rc<RefCell<Env>>) -> EvalResult {
    let values = eval_expressions(elements, env)?;
    Ok(Object::Array(values))
}

fn eval_index_exp(left: &Expression, index: &Expression, env: Rc<RefCell<Env>>) -> EvalResult {
    let left_evaluated = eval_expression(left, env.clone())?;
    let index_evaluated = eval_expression(index, env)?;

    match (left_evaluated, index_evaluated) {
        (Object::Array(array), Object::Integer(val)) => {
            Ok(array.get(val as usize).cloned().unwrap_or(Object::Null))
        }
        (l, i) => Err(EvalError::UnknownIndexOperator(l, i)),
    }
}

fn eval_expressions(exps: &[Expression], env: Rc<RefCell<Env>>) -> Result<Vec<Object>, EvalError> {
    let mut results = vec![];
    for exp in exps {
        results.push(eval_expression(exp, Rc::clone(&env))?);
    }
    Ok(results)
}

fn eval_identifier(ident: &str, env: Rc<RefCell<Env>>) -> EvalResult {
    if let Some(obj) = env.borrow().get(ident) {
        return Ok(obj.clone());
    }
    if let Some(obj) = builtins::lookup(ident) {
        return Ok(obj);
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

fn eval_float_infix_exp(left: f64, infix: &Infix, right: f64) -> EvalResult {
    match infix {
        Infix::Plus => Ok(Object::Float(left + right)),
        Infix::Minus => Ok(Object::Float(left - right)),
        Infix::Asterisk => Ok(Object::Float(left * right)),
        Infix::Slash => Ok(Object::Float(left / right)),
        Infix::Eq => Ok(Object::Bool(left == right)),
        Infix::Neq => Ok(Object::Bool(left != right)),
        Infix::Leq => Ok(Object::Bool(left <= right)),
        Infix::Geq => Ok(Object::Bool(left >= right)),
        Infix::Lt => Ok(Object::Bool(left < right)),
        Infix::Gt => Ok(Object::Bool(left > right)),
    }
}

fn eval_string_infix_exp(left: String, infix: &Infix, right: String) -> EvalResult {
    match infix {
        Infix::Plus => Ok(Object::String(format!("{left}{right}"))),
        _ => Err(EvalError::UnknownInfixOperator(
            Object::String(left),
            infix.clone(),
            Object::String(right),
        )),
    }
}

fn apply_function(func: Object, args: Vec<Object>) -> EvalResult {
    match func {
        Object::Function(params, body, env) => {
            assert_arg_count(params.len(), &args)?;
            let new_env = extend_function_env(params, args, env);
            let evaluated = eval_block_statement(&body, new_env)?;
            unwrap_return_value(evaluated)
        }
        Object::Builtin(func) => func(args),
        _ => Err(EvalError::NotCallable(func.clone())),
    }
}

fn extend_function_env(
    params: Vec<String>,
    args: Vec<Object>,
    env: Rc<RefCell<Env>>,
) -> Rc<RefCell<Env>> {
    let new_env = Rc::new(RefCell::new(Env::extend(env)));
    for (i, param) in params.iter().enumerate() {
        let arg = args.get(i).cloned().unwrap_or(Object::Null);
        new_env.borrow_mut().set(param, arg);
    }

    new_env
}

fn unwrap_return_value(obj: Object) -> EvalResult {
    match obj {
        Object::ReturnValue(val) => Ok(*val),
        _ => Ok(obj),
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
    fn eval_float() {
        expect_values(vec![
            ("5.0", "5"),
            ("10.0", "10"),
            // Prefix
            ("-1.5", "-1.5"),
            ("-10.0", "-10"),
            // Infix
            ("5.0 + 5.0", "10"),
            ("5.0 - 5.0", "0"),
            ("5.0 * 5.0", "25"),
            ("5.0 / 5.0", "1"),
            ("100.0 + 100.0 - 200.0", "0"),
            ("5.0 * (3.0 + 2.0)", "25"),
        ])
    }

    #[test]
    fn eval_string() {
        expect_values(vec![
            (r#""Hello, World!""#, r#""Hello, World!""#),
            (r#""hello" + " " + "world""#, r#""hello world""#),
        ]);
        expect_errors(vec![(
            r#""hello world" - "hello""#,
            "unknown operator: STRING - STRING",
        )]);
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

    #[test]
    fn function_obj() {
        expect_values(vec![
            ("fn(a) { a; }", "fn(a) {\n{ a; }\n}"),
            ("fn(a, b) { a + b; }", "fn(a, b) {\n{ (a + b); }\n}"),
        ])
    }

    #[test]
    fn function_call() {
        expect_values(vec![
            ("let identity = fn(a) { a; }; identity(5);", "5"),
            ("let identity = fn(a) { return a; }; identity(5);", "5"),
            ("let double = fn(a) { a * 2; }; double(5);", "10"),
            ("let add = fn(a, b) { a + b; }; add(5, 5);", "10"),
            (
                "let add = fn(a, b) { a + b; }; add(5 + 5, add(5, 5));",
                "20",
            ),
            ("fn(a) { a; }(5)", "5"),
            ("fn(a) { a; }(1); 5;", "5"),
        ])
    }

    #[test]
    fn closure() {
        expect_values(vec![(
            "let newAdder = fn(x) { fn(y) { x + y } }; let addTwo = newAdder(2); addTwo(3);",
            "5",
        )])
    }

    #[test]
    fn array_literal() {
        expect_values(vec![("[1, 2 * 2, 3 / 3]", "[1, 4, 1]")])
    }

    #[test]
    fn array_index_expression() {
        expect_values(vec![
            ("[1, 2, 3][0]", "1"),
            ("[1, 2, 3][1]", "2"),
            ("[1, 2, 3][2]", "3"),
            ("let i = 0; [1][i];", "1"),
            ("[1, 2, 3][1 + 1];", "3"),
            ("let myArray = [1, 2, 3]; myArray[2];", "3"),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                "6",
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                "2",
            ),
            ("[1, 2, 3][3]", "null"),
            ("[1, 2, 3][-1]", "null"),
        ]);
    }

    #[test]
    fn builtin_function() {
        expect_values(vec![
            (r#"len("")"#, "0"),
            (r#"len("four")"#, "4"),
            (r#"len("hello world")"#, "11"),
            ("len([1, 2, 3])", "3"),
            ("len([])", "0"),
        ]);
        expect_errors(vec![
            ("len(1)", "unknown args for builtin function: len(INTEGER)"),
            (
                r#"len("one", "two")"#,
                "wrong number of args: expected 1, got 2",
            ),
        ]);
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
