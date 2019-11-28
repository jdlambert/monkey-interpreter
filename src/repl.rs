use std::{
    env,
    io::{stdin, stdout, Write},
};

use crate::{lexer::Lexer, token::Token};

pub fn start() {
    let username = env::var("LOGNAME").unwrap_or_else(|_| "anonymous".to_string());
    println!(
        "Hello {}! This is the Monkey programming language!",
        username
    );
    println!("Feel free to type in commands");

    loop {
        let input = get_input();
        let mut lexer = Lexer::new(input);
        loop {
            let token = lexer.next_token();
            if token == Token::Eof {
                break;
            } else {
                println!("{:?}", token);
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
