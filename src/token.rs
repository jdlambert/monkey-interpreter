#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Illegal,
    Eof,
    Ident(String),
    Int(u32),
    Assign,
    Plus,
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Function,
    Let,
    Bang,
    Minus,
    Slash,
    Asterisk,
    Lt,
    Gt,
    If,
    Else,
    Return,
    True,
    False,
    Equal,
    NotEqual,
}

impl Token {
    pub fn from_ident(keyword: &str) -> Self {
        match keyword {
            "fn" => Token::Function,
            "let" => Token::Let,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            "true" => Token::True,
            "false" => Token::False,
            _ => Token::Ident(keyword.to_owned()),
        }
    }
}
