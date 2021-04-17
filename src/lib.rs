pub mod ast;
pub mod evaluator;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod token;

use object::environment;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn monkey_eval(input: &str) -> String {
    let env = environment::Environment::new();
    let program = ast::Program::new(input);
    match evaluator::eval(program, env) {
        Ok(o) => o.to_string(),
        Err(e) => e.to_string(),
    }
}
