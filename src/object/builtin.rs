use crate::{evaluator::EvalError, object::Object};
use crate::{evaluator::Result, object::NULL};

pub fn get(ident: &str) -> Option<Object> {
    match ident {
        "len" => Some(Object::Builtin(ident.to_string(), len)),
        "first" => Some(Object::Builtin(ident.to_string(), first)),
        "last" => Some(Object::Builtin(ident.to_string(), last)),
        "rest" => Some(Object::Builtin(ident.to_string(), rest)),
        "push" => Some(Object::Builtin(ident.to_string(), push)),
        "puts" => Some(Object::Builtin(ident.to_string(), puts)),
        // "map" => Some(Object::Builtin(ident.to_string(), map)),
        _ => None,
    }
}

fn assert_n_args(args: &[Object], n: usize, name: &str) -> Result<()> {
    if args.len() != n {
        Err(EvalError::WrongNumberOfArguments(
            format!("parameters {}, arguments {}", n, args.len()),
            format!("builtin {}", name),
        ))
    } else {
        Ok(())
    }
}

// there is only ever one len object so we could easily make it a const
fn len(args: Vec<Object>) -> Result<Object> {
    let name = "len";
    assert_n_args(&args, 1, name)?;

    match &args[0] {
        Object::Str(s) => Ok(Object::Integer(s.len() as i32)),
        Object::Array(items) => Ok(Object::Integer(items.len() as i32)),
        o => Err(EvalError::TypeMismatch(
            format!("{}", o.debug_type()),
            format!("builtin {}", name),
        )),
    }
}

fn first(args: Vec<Object>) -> Result<Object> {
    let name = "first";
    assert_n_args(&args, 1, name)?;

    match &args[0] {
        Object::Array(items) => match items.first() {
            Some(obj) => Ok(obj.clone()),
            None => Err(EvalError::IndexOutOfBounds(
                "array length = 0".to_string(),
                format!("builtin {}", name),
            )),
        },
        o => Err(EvalError::TypeMismatch(
            format!("{}", o.debug_type()),
            format!("builtin {}", name),
        )),
    }
}

fn last(args: Vec<Object>) -> Result<Object> {
    let name = "last";
    assert_n_args(&args, 1, name)?;

    match &args[0] {
        Object::Array(items) => match items.last() {
            Some(obj) => Ok(obj.clone()),
            None => Err(EvalError::IndexOutOfBounds(
                "array length = 0".to_string(),
                format!("builtin {}", name),
            )),
        },
        o => Err(EvalError::TypeMismatch(
            format!("{}", o.debug_type()),
            format!("builtin {}", name),
        )),
    }
}

fn rest(args: Vec<Object>) -> Result<Object> {
    let name = "rest";
    assert_n_args(&args, 1, name)?;

    match &args[0] {
        Object::Array(items) => match items.split_first() {
            Some((_, rest)) => Ok(Object::Array(rest.to_vec())),
            None => Err(EvalError::IndexOutOfBounds(
                format!("not enough items, length = {}", items.len()),
                format!("builtin {}", name),
            )),
        },
        o => Err(EvalError::TypeMismatch(
            format!("{}", o.debug_type()),
            format!("builtin {}", name),
        )),
    }
}

fn push(args: Vec<Object>) -> Result<Object> {
    let name = "push";
    assert_n_args(&args, 2, name)?;

    match (&args[0], &args[1]) {
        (Object::Array(items), obj) => {
            let mut new_items = vec![];
            for item in items {
                new_items.push(item.clone());
            }
            new_items.push(obj.clone());
            Ok(Object::Array(new_items))
        }
        (not_array, _) => Err(EvalError::TypeMismatch(
            format!("could not append to {}", not_array.debug_type()),
            format!("builtin {}", name),
        )),
    }
}

fn puts(args: Vec<Object>) -> Result<Object> {
    let name = "puts";
    assert_n_args(&args, 1, name)?;

    for obj in args {
        println!("{}", obj)
    }

    Ok(NULL)
}
