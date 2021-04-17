use crate::ast::{BlockStmt, Expr, Program, Stmt};
use crate::object::builtin;
use crate::object::environment::{Environment, MutEnv};
use crate::object::{Object, FALSE, NULL, TRUE};
use crate::token::Token;
use std::{collections::HashMap, fmt::Display, rc::Rc};

pub type Result<T> = std::result::Result<T, EvalError>;

#[derive(Debug, PartialEq)]
pub enum EvalError {
    TypeMismatch(String, String),
    UnknownOperator(String, String),
    // SyntaxError(String, String),
    IdentifierNotFound(String, String),
    ExpectedIdentifier(String, String),
    WrongNumberOfArguments(String, String),
    IndexOutOfBounds(String, String),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
        match self {
            EvalError::TypeMismatch(s, func) => write!(f, "type mismatch: {} in {}", s, func),
            EvalError::UnknownOperator(s, func) => write!(f, "unknown operator: {} in {}", s, func),
            // EvalError::SyntaxError(s, func) => write!(f, "syntax error: {} in {}", s, func),
            EvalError::IdentifierNotFound(s, func) => {
                write!(f, "identifier not found: {} in {}", s, func)
            }
            EvalError::ExpectedIdentifier(s, func) => {
                write!(f, "expected identifier: {} in {}", s, func)
            }
            EvalError::WrongNumberOfArguments(s, func) => {
                write!(f, "wrong number of arguments: {} in {}", s, func)
            }
            EvalError::IndexOutOfBounds(s, func) => {
                write!(f, "index out of bounds: {} in {}", s, func)
            }
        }
    }
}

pub fn eval(program: Program, env: MutEnv) -> Result<Object> {
    let mut result = NULL;

    for statement in program.statements.iter() {
        result = eval_statement(statement, Rc::clone(&env))?;
        if let Object::Return(expression) = &result {
            result = *expression.clone();
            break;
        }
    }

    Ok(result)
}

fn eval_block_statement(block: &BlockStmt, env: MutEnv) -> Result<Object> {
    let mut result = NULL;

    for statement in block.statements.iter() {
        result = eval_statement(statement, Rc::clone(&env))?;
        if let Object::Return(_) = &result {
            break;
        }
    }

    Ok(result)
}

fn eval_statement(statement: &Stmt, env: MutEnv) -> Result<Object> {
    match statement {
        Stmt::Expression(expression) => eval_expression(expression, env),
        Stmt::Return(expression) => Ok(Object::Return(Box::new(eval_expression(expression, env)?))),
        Stmt::Let(ident, expression) => {
            let object = eval_expression(expression, Rc::clone(&env))?;
            env.borrow_mut().set(ident, object);
            Ok(NULL)
        }
    }
}

fn eval_expression(expression: &Expr, env: MutEnv) -> Result<Object> {
    match expression {
        Expr::Integer(i) => Ok(Object::Integer(*i)),
        Expr::Boolean(b) => Ok(Object::Boolean(*b)),
        Expr::Str(s) => Ok(Object::Str(s.clone())),
        Expr::Array(items) => Ok(Object::Array(eval_expressions(items, env)?)),
        Expr::Hash(items) => Ok(eval_hash_map(items, env)?),
        Expr::Index(left, index) => eval_index_expression(left, index, env),
        Expr::Prefix(operator, expression) => {
            eval_prefix_expression(operator, eval_expression(expression, env)?)
        }
        Expr::Infix(left, operator, right) => {
            let left = eval_expression(left, Rc::clone(&env))?;
            let right = eval_expression(right, env)?;
            eval_infix_expression(operator, left, right)
        }
        Expr::If(condition, consequence, alternative) => {
            eval_if_expression(condition, consequence, alternative, env)
        }
        Expr::Identifier(ident) => eval_identifier(ident, env),
        Expr::Function(parameters, body) => Ok(Object::Function(
            parameters.clone(),
            body.clone(),
            Rc::clone(&env),
        )),
        Expr::Call(func, arguments) => {
            let function = eval_expression(func, Rc::clone(&env))?;
            let arguments = eval_expressions(arguments, env)?;
            apply_function(function, arguments)
        }
        // _ => Err(EvalError::UnknownOperator(
        //     format!("No match arm for {:?}", expression),
        //     "eval_expression".to_string(),
        // )),
    }
}

fn eval_prefix_expression(operator: &Token, object: Object) -> Result<Object> {
    match operator {
        Token::Bang => match object {
            TRUE => Ok(FALSE),
            FALSE => Ok(TRUE),
            object => Err(EvalError::UnknownOperator(
                format!("{}{}", operator, object.debug_type()),
                "eval_prefix_expression".to_string(),
            )),
        },
        Token::Minus => match object {
            Object::Integer(i) => Ok(Object::Integer(-i)),
            _ => Err(EvalError::UnknownOperator(
                format!("{}{}", operator, object.debug_type()),
                "eval_prefix_expression".to_string(),
            )),
        },
        _ => Err(EvalError::UnknownOperator(
            format!("{}{}", operator, object.debug_type()),
            "eval_prefix_expression".to_string(),
        )),
    }
}

fn eval_infix_expression(operator: &Token, left: Object, right: Object) -> Result<Object> {
    match (&left, &right) {
        (Object::Integer(i), Object::Integer(j)) => eval_integer_infix_expression(operator, *i, *j),
        (Object::Boolean(b0), Object::Boolean(b1)) => {
            eval_boolean_infix_expression(operator, *b0, *b1)
        }
        (Object::Str(s0), Object::Str(s1)) => eval_string_infix_expression(operator, s0, s1),
        (_, _) => Err(EvalError::TypeMismatch(
            format!("{} {} {}", left.debug_type(), operator, right.debug_type()),
            "eval_infix_expression".to_string(),
        )),
    }
}

fn eval_integer_infix_expression(operator: &Token, i: i32, j: i32) -> Result<Object> {
    match operator {
        Token::Plus => Ok(Object::Integer(i + j)),
        Token::Minus => Ok(Object::Integer(i - j)),
        Token::Asterisk => Ok(Object::Integer(i * j)),
        Token::Slash => Ok(Object::Integer(i / j)),
        Token::Lt => Ok(Object::get_bool(i < j)),
        Token::Gt => Ok(Object::get_bool(i > j)),
        Token::Eq => Ok(Object::get_bool(i == j)),
        Token::NotEq => Ok(Object::get_bool(i != j)),
        _ => Err(EvalError::UnknownOperator(
            format!("{}", operator),
            "eval_integer_infix_expression".to_string(),
        )),
    }
}

fn eval_boolean_infix_expression(operator: &Token, b0: bool, b1: bool) -> Result<Object> {
    match operator {
        Token::Eq => Ok(Object::get_bool(b0 == b1)),
        Token::NotEq => Ok(Object::get_bool(b0 != b1)),
        _ => Err(EvalError::UnknownOperator(
            format!("{}", operator),
            "eval_boolean_infix_expression".to_string(),
        )),
    }
}

fn eval_string_infix_expression(operator: &Token, s0: &str, s1: &str) -> Result<Object> {
    match operator {
        Token::Plus => Ok(Object::Str(s0.to_string() + s1)),
        Token::Eq => Ok(Object::get_bool(s0 == s1)),
        Token::NotEq => Ok(Object::get_bool(s0 != s1)),
        _ => Err(EvalError::UnknownOperator(
            format!("{}", operator),
            "eval_string_infix_expression".to_string(),
        )),
    }
}

fn eval_hash_map(items: &[(Expr, Expr)], env: MutEnv) -> Result<Object> {
    let mut map = HashMap::new();

    for (key, value) in items {
        let key = eval_expression(key, Rc::clone(&env))?;
        let value = eval_expression(value, Rc::clone(&env))?;
        map.insert(key, value);
    }

    Ok(Object::Hash(map))
}

fn eval_index_expression(left: &Expr, index: &Expr, env: MutEnv) -> Result<Object> {
    let left = eval_expression(&left, Rc::clone(&env))?;
    let index = eval_expression(&index, env)?;
    match (left, index) {
        (Object::Array(items), Object::Integer(i)) => {
            if i < 0 {
                Err(EvalError::IndexOutOfBounds(
                    format!("negative indices not supported. index = {}", i),
                    "eval_index_expression".to_string(),
                ))
            } else if i as usize > items.len() - 1 {
                Err(EvalError::IndexOutOfBounds(
                    format!("array length = {}, index = {}", items.len(), i),
                    "eval_index_expression".to_string(),
                ))
            } else {
                Ok(items.get(i as usize).unwrap().clone())
            }
        }
        (Object::Hash(map), o) => match o {
            Object::Integer(_) | Object::Boolean(_) | Object::Str(_) => match map.get(&o) {
                Some(o) => Ok(o.clone()),
                None => Ok(NULL),
            },
            o => Err(EvalError::TypeMismatch(
                format!("{} is not hashable", o.debug_type()),
                "eval_index_expression".to_string(),
            )),
        },
        (obj_0, obj_1) => Err(EvalError::TypeMismatch(
            format!("{}[{}]", obj_0, obj_1),
            "eval_index_expression".to_string(),
        )),
    }
}

fn eval_if_expression(
    condition: &Expr,
    consequence: &BlockStmt,
    alternative: &Option<BlockStmt>,
    env: MutEnv,
) -> Result<Object> {
    let condition = eval_expression(condition, Rc::clone(&env))?;
    if is_truthy(condition) {
        eval_block_statement(consequence, env)
    } else {
        match alternative {
            Some(alternative) => eval_block_statement(alternative, env),
            None => Ok(NULL),
        }
    }
}

fn eval_identifier(ident: &str, env: MutEnv) -> Result<Object> {
    if let Some(object) = env.borrow().get(ident) {
        return Ok(object);
    }
    if let Some(object) = builtin::get(ident) {
        return Ok(object);
    }
    Err(EvalError::IdentifierNotFound(
        ident.to_string(),
        "eval_identifier".to_string(),
    ))
}

fn apply_function(function: Object, args: Vec<Object>) -> Result<Object> {
    match function {
        Object::Function(params, body, env) => {
            // extend function's environment with arguments
            let extended_env = extend_function_environment(&params, env, args)?;
            match eval_block_statement(&body, extended_env)? {
                Object::Return(o) => Ok(*o),
                o => Ok(o),
            }
        }
        Object::Builtin(_, func) => func(args),
        o => Err(EvalError::TypeMismatch(
            format!("{} is not a function", o.debug_type()),
            "eval_call_expression".to_string(),
        )),
    }
}

fn extend_function_environment(params: &[Expr], env: MutEnv, args: Vec<Object>) -> Result<MutEnv> {
    if params.len() != args.len() {
        return Err(EvalError::WrongNumberOfArguments(
            format!("parameters {}, arguments {}", params.len(), args.len()),
            "extend_function_environment".to_string(),
        ));
    };

    let extended_env = Environment::new_enclosed(env);
    for (param, arg) in params.iter().zip(args.into_iter()) {
        match param {
            Expr::Identifier(ident) => extended_env.borrow_mut().set(ident, arg),
            e => {
                return Err(EvalError::ExpectedIdentifier(
                    e.to_string(),
                    "extend_function_environment".to_string(),
                ))
            }
        }
    }
    Ok(extended_env)
}

fn eval_expressions(exprs: &[Expr], env: MutEnv) -> Result<Vec<Object>> {
    let mut results = vec![];
    for expr in exprs {
        results.push(eval_expression(expr, Rc::clone(&env))?);
    }
    Ok(results)
}

fn is_truthy(condition: Object) -> bool {
    match condition {
        NULL => false,
        FALSE => false,
        TRUE => true,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;

    #[test]
    fn integer_expression() {
        let tests = vec![
            ("10;", Object::Integer(10)),
            ("5;", Object::Integer(5)),
            ("0;", Object::Integer(0)),
            ("-5;", Object::Integer(-5)),
            ("-10;", Object::Integer(-10)),
            ("1 + 2", Object::Integer(3)),
            ("1 + 2 * 3", Object::Integer(7)),
            ("1 - 2 * 3", Object::Integer(-5)),
            ("1 * 2 * 3", Object::Integer(6)),
            ("1 + 2 / 3", Object::Integer(1)),
            ("1 + 6 / -3", Object::Integer(-1)),
            ("2 / 2 + 5", Object::Integer(6)),
            ("-2 / 2 + 5", Object::Integer(4)),
            ("8 / 4 + 5", Object::Integer(7)),
            ("(10 + 5) * 3", Object::Integer(45)),
        ];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
        }
    }

    #[test]
    fn boolean_expression() {
        let tests = vec![
            ("true", TRUE),
            ("false", FALSE),
            ("true;", TRUE),
            ("1 < 2", TRUE),
            ("1 > 2", FALSE),
            ("1 < 1", FALSE),
            ("1 > 1", FALSE),
            ("1 == 1", TRUE),
            ("1 != 1", FALSE),
            ("1 == 2", FALSE),
            ("1 != 2", TRUE),
            ("true == true", TRUE),
            ("false == false", TRUE),
            ("true == false", FALSE),
            ("true != false", TRUE),
            ("false != true", TRUE),
            ("(1 < 2) == true", TRUE),
            ("(1 < 2) == false", FALSE),
            ("(1 > 2) == true", FALSE),
            ("(1 > 2) == false", TRUE),
        ];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
        }
    }

    #[test]
    fn string_expression() {
        let tests = vec![
            ("\"hello\"", "hello"),
            ("\":)\"", ":)"),
            ("\"true\"", "true"),
            ("\"123\"", "123"),
        ];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(
                eval(program, env).unwrap(),
                Object::Str(expected.to_string()),
                "{}",
                input
            );
        }
    }

    #[test]
    fn string_infix_expression() {
        let tests = vec![
            (
                "\"hello, \" + \"world!\"",
                Object::Str("hello, world!".to_string()),
            ),
            (
                "\"hello\" + \" \" + \":)\"",
                Object::Str("hello :)".to_string()),
            ),
            ("\"hello\" == \"hello\"", TRUE),
            ("\"hello\" == \":)\"", FALSE),
            ("\"hello\" != \":)\"", TRUE),
            ("\"hello\" != \"hello\"", FALSE),
        ];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
        }
    }

    #[test]
    fn array_literals() {
        let tests = vec![
            ("[]", Object::Array(vec![])),
            ("[1]", Object::Array(vec![Object::Integer(1)])),
            (
                "[1, 2]",
                Object::Array(vec![Object::Integer(1), Object::Integer(2)]),
            ),
        ];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
        }
    }

    #[test]
    fn array_index_expressions() {
        let tests = vec![
            ("[1,2,3][0]", Object::Integer(1)),
            ("[1,2,3][1]", Object::Integer(2)),
            ("[1,2,3][2]", Object::Integer(3)),
            ("let i = 0; [1,2,3][i]", Object::Integer(1)),
            ("[1,2,3][1+1]", Object::Integer(3)),
            ("let myArray = [1,2,3]; myArray[1]", Object::Integer(2)),
            (
                "let myArray = [1,2,3]; myArray[0] + myArray[1] + myArray[2]",
                Object::Integer(6),
            ),
            (
                "let myArray = [1,2,3]; let i = myArray[0]; myArray[i]",
                Object::Integer(2),
            ),
        ];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
        }
    }

    #[test]
    fn test_hash_literals() {
        let map_1 = HashMap::new();
        let mut map_2 = HashMap::new();
        map_2.insert(Object::Integer(2), Object::Str("two".to_string()));
        map_2.insert(TRUE, Object::Integer(3));
        map_2.insert(Object::Str("four".to_string()), Object::Integer(4));
        map_2.insert(Object::Str("five".to_string()), Object::Integer(5));

        let tests = vec![
            ("{}", Object::Hash(map_1)),
            (
                "let five = \"five\";
              {
                2: \"two\",
                true: 3,
                \"fo\" + \"ur\": 4,
                five: 5,
              }",
                Object::Hash(map_2),
            ),
        ];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
        }
    }

    #[test]
    fn test_hash_index_expressions() {
        let tests = vec![
            ("{}[1]", NULL),
            ("{\"one\": 1}[\"one\"]", Object::Integer(1)),
            ("{2: true}[2]", TRUE),
            ("{false: 1, false: 2}[false]", Object::Integer(2)),
        ];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
        }
    }

    #[test]
    fn bang_operator() {
        let tests = vec![
            ("!true", Object::Boolean(false)),
            ("!false", Object::Boolean(true)),
            ("!!true", Object::Boolean(true)),
            ("!!false", Object::Boolean(false)),
        ];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
        }
    }

    #[test]
    fn if_expressions() {
        let tests = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", NULL),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", NULL),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
        }
    }

    #[test]
    fn return_statements() {
        let tests = vec![
            ("return 11;", Object::Integer(11)),
            ("return 12; 9;", Object::Integer(12)),
            ("return 3 + (2 * 5); 9;", Object::Integer(13)),
            ("9; return 2 * 7; 9;", Object::Integer(14)),
            (
                "if (10 > 1) { if (true) { return 15; } return 1; } ",
                Object::Integer(15),
            ),
        ];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
        }
    }

    #[test]
    fn error_handling() {
        let tests = vec![
            (
                "5 + true;",
                EvalError::TypeMismatch("Integer + Boolean".to_string(), "eval_infix_expression".to_string())
            ),
            (
                "5 + true; 5;",
                EvalError::TypeMismatch("Integer + Boolean".to_string(), "eval_infix_expression".to_string())
            ),
            ("-true", EvalError::UnknownOperator("-Boolean".to_string(), "eval_prefix_expression".to_string())),
            (
                "true + false",
                EvalError::UnknownOperator("+".to_string(), "eval_boolean_infix_expression".to_string())
            ),
            (
                "5; true + false; 5",
                EvalError::UnknownOperator("+".to_string(), "eval_boolean_infix_expression".to_string())
            ),
            (
                "if (2 > 1) { true + false; } else { 5 };",
                EvalError::UnknownOperator("+".to_string(), "eval_boolean_infix_expression".to_string())
            ),
            (
                "if (2 > 1) { if (2 < 1) { return 1; } else { return true + true; } } else { return 2; };",
                EvalError::UnknownOperator("+".to_string(), "eval_boolean_infix_expression".to_string())
            ),
            (
                "let add = fn(a, b) { a + b }; add(1, 2, 3);",
                EvalError::WrongNumberOfArguments("parameters 2, arguments 3".to_string(), "extend_function_environment".to_string())
            ),
            (
                "\"Hello\" - \"Hello\"",
                EvalError::UnknownOperator("-".to_string(), "eval_string_infix_expression".to_string())
            ),
            ("[1,2,3][3]", EvalError::IndexOutOfBounds("array length = 3, index = 3".to_string(), "eval_index_expression".to_string())),
            ("[1,2,3][-1]", EvalError::IndexOutOfBounds("negative indices not supported. index = -1".to_string(), "eval_index_expression".to_string())),
            ("{true: 2}[[1,2,3]]", EvalError::TypeMismatch("Array is not hashable".to_string(), "eval_index_expression".to_string())),
        ];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(eval(program, env).unwrap_err(), expected, "{}", input);
        }
    }

    #[test]
    fn let_statements() {
        let tests = vec![
            ("let a = 5; a;", Object::Integer(5)),
            ("let a = 5 * 5; a;", Object::Integer(25)),
            ("let a = 5; let b = a; b;", Object::Integer(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Object::Integer(15),
            ),
            (
                "let a = 5; let b = a; let c = a + b + 5; c * 3;",
                Object::Integer(45),
            ),
        ];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
        }
    }

    #[test]
    fn functions_objects_0() {
        let tests = vec![(
            "fn(x) { x + 2; };",
            Object::Function(
                vec![Expr::Identifier("x".to_string())],
                BlockStmt {
                    statements: vec![Stmt::Expression(Expr::Infix(
                        Box::new(Expr::Identifier("x".to_string())),
                        Token::Plus,
                        Box::new(Expr::Integer(2)),
                    ))],
                },
                Environment::new(),
            ),
        )];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
        }
    }

    #[test]
    fn functions_objects_1() {
        let tests = vec![("fn(x) { x + 2; };", "fn(x) (x + 2)".to_string())];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(
                eval(program, env).unwrap().to_string(),
                expected,
                "{}",
                input
            );
        }
    }

    #[test]
    fn function_application() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", Object::Integer(5)),
            ("let identity = fn(x) { return x; }; identity(5);", Object::Integer(5)),
            ("let double = fn(x) { x * 2; }; double(5);", Object::Integer(10)),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", Object::Integer(10)),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", Object::Integer(20)),
            ("fn(x) { x; }(5)", Object::Integer(5)),
            ("fn(x) { return x; }(5)", Object::Integer(5)),
            ("fn(x) { x; }(3 * 3)", Object::Integer(9)),
            ("let factorial = fn(n) { if (n == 0) { 1 } else { n * factorial(n - 1) } }; factorial(5);", Object::Integer(120)),
            ("let addThree = fn(x) { x + 3 }; let callTwoTimes = fn(x, func) { func(func(x)) }; callTwoTimes(3, addThree);", Object::Integer(9)),
            ("let fib = fn(i) { if (i < 2) { 1 } else { fib(i-1) + fib(i-2); } }; fib(9)", Object::Integer(55)),
            ("let fib = fn(i) { if (i==0) { return 1 } else { if (i==1) { return 1; } else { return fib(i-1) + fib(i-2); } } }; fib(9)", Object::Integer(55)),
            ("let n = 3; let add_n = fn(x) { x + n; }; add_n(2)", Object::Integer(5)),
            ("let n = 3; let add_n = fn(x) { x + n; }; let n = 1; add_n(2)", Object::Integer(3)), // not sure I like how environments behave
        ];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
        }
    }

    #[test]
    fn closures() {
        let tests = vec![(
            "let newAdder = fn(x) { fn(y) { x + y } }; let addTwo = newAdder(2); addTwo(2)",
            Object::Integer(4),
        )];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
        }
    }

    #[test]
    fn higher_order_functions() {
        let tests = vec![(
            "let add = fn(a, b) { a + b };
                 let applyFunc = fn(a, b, func) { func(a, b) };
                 applyFunc(2, 2, add);",
            Object::Integer(4),
        )];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
        }
    }

    #[test]
    fn functional() {
        let tests = vec![
            (
                "
            let map = fn(f, xs) {
                let iter = fn(accumulated, remaining) {
                    if (len(remaining) == 0) {
                        accumulated
                    } else {
                        iter(push(accumulated, f(first(remaining))), rest(remaining))
                    }
                };
                iter([], xs)
            };
            let square = fn(x) {
                x*x
            };
            let array = [1, 2, 3, 4];
            map(square, array)
            ",
                Object::Array(vec![
                    Object::Integer(1),
                    Object::Integer(4),
                    Object::Integer(9),
                    Object::Integer(16),
                ]),
            ),
            (
                "
            let reduce = fn(acc, f, xs) {
                let iter = fn(acc, xs) {
                    if (len(xs)==0) {
                        acc
                    } else {
                        iter(f(acc, first(xs)), rest(xs))
                    }
                };
                iter(acc, xs)
            }
            let array = [1, 2, 3, 4];
            reduce(0, fn(a, b){a + b}, array)
            ",
                Object::Integer(10),
            ),
        ];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
        }
    }

    #[test]
    fn builtin_functions() {
        let tests = vec![
            ("len(\"\")", Object::Integer(0)),
            ("len(\"four\")", Object::Integer(4)),
            ("len(\"hiiiiiiiii\")", Object::Integer(10)),
            ("len(\"hello\" + \" :)\")", Object::Integer(8)),
            ("len([1,2,3,4,5])", Object::Integer(5)),
            ("first([1,2,3,4,5])", Object::Integer(1)),
            ("last([1,2,3,4,5])", Object::Integer(5)),
            (
                "rest([1,2,3,4,5])",
                Object::Array(vec![
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                ]),
            ),
            (
                "rest(rest([1,2,3,4,5]))",
                Object::Array(vec![
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                ]),
            ),
            (
                "rest(rest(rest(rest(rest([1,2,3,4,5])))))",
                Object::Array(vec![]),
            ),
            (
                "push([1], 2)",
                Object::Array(vec![Object::Integer(1), Object::Integer(2)]),
            ),
            (
                "let x = [1]; let x = push(x, 2); let x = push(x, 3); x",
                Object::Array(vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                ]),
            ),
        ];

        for (input, expected) in tests {
            let program = Program::new(input);
            let env = Environment::new();
            assert_eq!(eval(program, env).unwrap(), expected, "{}", input);
        }
    }
}
