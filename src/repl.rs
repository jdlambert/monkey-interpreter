use std::{
    env,
    io::{stdin, stdout, Write},
};

use crate::parser::Parser;

pub fn start() {
    let username = env::var("LOGNAME").unwrap_or_else(|_| "anonymous".to_string());
    println!(
        "Hello {}! This is the Monkey programming language!",
        username
    );
    println!("Feel free to type in commands");

    loop {
        let mut parser = Parser::from_input(get_input());
        let program = parser.parse_program();
        match program {
            Some(program) => {
                println!("{}", program);
            },
            None => {
                println!("Invalid input! Errors:");
                for parser_error in parser.errors {
                    println!("{:?}", parser_error);
                }
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
