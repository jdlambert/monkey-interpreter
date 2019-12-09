use std::{
    env,
    io::{stdin, stdout, Write},
};

use crate::{eval::eval_input, environment::Environment};

pub fn start() {
    let username = env::var("LOGNAME").unwrap_or_else(|_| "anonymous".to_string());
    println!(
        "Hello {}! This is the Monkey programming language!",
        username
    );
    println!("Feel free to type in commands");

    let env = Environment::new();
    loop {
        let result = eval_input(&get_input(), &env);
        match result {
            Ok(obj) => {
                println!("{}", obj);
            }
            Err(err) => {
                println!("Invalid input! Error: {}", err);
            }
        }
    }
}

fn get_input() -> String {
    print!(">>");

    stdout().flush().expect("Failed to flush stdout!");
    let mut input = String::new();

    stdin()
        .read_line(&mut input)
        .expect("Failed to read input!");

    input
}
