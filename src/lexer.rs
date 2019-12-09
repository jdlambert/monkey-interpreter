use crate::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        lexer.read_char();
        lexer
    }

    pub fn next_token(self: &mut Self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            '=' => {
                if '=' == self.peek_char() {
                    self.read_char();
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            ';' => Token::Semicolon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            '-' => Token::Minus,
            '!' => {
                if '=' == self.peek_char() {
                    self.read_char();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            }
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '<' => Token::Lt,
            '>' => Token::Gt,
            '\0' => Token::Eof,
            '"' => {
                return Token::String(self.read_string());
            }
            _ => {
                if is_letter(self.ch) {
                    return Token::from_ident(self.read_identifier());
                } else if is_digit(self.ch) {
                    return Token::Int(self.read_number());
                }
                Token::Illegal
            }
        };
        self.read_char();
        tok
    }

    fn peek_char(&mut self) -> char {
        self.input.chars().nth(self.read_position).unwrap_or('\0')
    }

    fn read_char(&mut self) {
        self.ch = self.peek_char();
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_number(&mut self) -> i64 {
        let position = self.position;
        while is_digit(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].parse::<i64>().unwrap()
    }

    fn read_identifier(&mut self) -> &str {
        let position = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        &self.input[position..self.position]
    }

    fn read_string(&mut self) -> String {
        self.read_char(); // Consume the initial `"`
        let mut out = String::new();
        while self.ch != '"' {
           // TODO: handle unterminated strings: Look for EOF, LexerErrors.
           out.push(self.ch);
           self.read_char(); 
        }
        out
    }

    fn skip_whitespace(&mut self) {
        while is_whitespace(self.ch) {
            self.read_char();
        }
    }
}

fn is_letter(ch: char) -> bool {
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

fn is_whitespace(ch: char) -> bool {
    ' ' == ch || '\t' == ch || '\n' == ch || '\r' == ch
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;

    fn test_next_token(input: &str, expected: &[Token]) {
        let mut lexer = Lexer::new(input.to_owned());

        for expected_token in expected.iter() {
            let token = lexer.next_token();

            assert_eq!(&token, expected_token);
            println!("{:?}", token);
        }
    }

    #[test]
    fn test_core_functionality() {
        let input = r#"let five = 5;
            let ten = 10;
            
            let add = fn(x, y) {
              x + y;
            };
            
            let result = add(five, ten);"#;

        let expected = [
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::Lparen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::Rparen,
            Token::Semicolon,
            Token::Eof,
        ];

        test_next_token(input, &expected);
    }

    #[test]
    fn test_extended_chars() {
        let input = r#"!-/*5;
        5 < 10 > 5;~"#;

        let expected = [
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
            Token::Semicolon,
            Token::Illegal,
            Token::Eof,
        ];

        test_next_token(input, &expected);
    }

    #[test]
    fn test_extended_keywords() {
        let input = r#"true false if else return"#;

        let expected = [
            Token::True,
            Token::False,
            Token::If,
            Token::Else,
            Token::Return,
        ];

        test_next_token(input, &expected);
    }

    #[test]
    fn test_two_char_tokens() {
        let input = r#"== !="#;

        let expected = [Token::Equal, Token::NotEqual];

        test_next_token(input, &expected);
    }

    #[test]
    fn test_strings() {
        let input = r#""string!""#;

        let expected = [Token::String("string!".to_string())];

        test_next_token(input, &expected);
    }
}
