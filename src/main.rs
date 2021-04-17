use std::{io, rc::Rc};

mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod token;

use object::environment;

fn main() {
    println!("This is the Monkey programming language!");
    println!("Feel free to type in commands");
    let env = environment::Environment::new();
    loop {
        eprint!(">>> ");

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read input");

        let program = ast::Program::new(&input);

        let evaluated = evaluator::eval(program, Rc::clone(&env));
        let evaluated = match evaluated {
            Ok(o) if o == object::NULL => continue,
            Ok(o) => o.to_string(),
            Err(e) => e.to_string(),
        };
        eprintln!("\t{}", evaluated);
    }
}
