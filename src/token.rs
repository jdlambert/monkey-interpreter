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
}

impl Token {
    pub fn from_ident(keyword: &str) -> Token {
        match keyword {
            "fn" => Token::Function,
            "let" => Token::Let,
            _ => Token::Ident(keyword.to_owned()),
        }
    }
}
