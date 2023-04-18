use std::{iter::Peekable, str::Chars};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Illegal,
    EOF,
    Int(usize),
    Identifier(String),
    Assign,
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    Equal,
    NotEqual,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
}
static KEYWORDS: phf::Map<&'static str, Token> = phf::phf_map! {
    "true" => Token::True,
    "false" => Token::False,
    "fn" => Token::Function,
    "let" => Token::Let,
    "if" => Token::If,
    "else" => Token::Else,
    "return" => Token::Return,
};

pub struct Lexer<'a> {
    pub chars: Peekable<Chars<'a>>,

}
impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Lexer<'a> {
        return Lexer {
            chars: code.chars().peekable(),
        };
    }

    fn peek(&mut self) -> Option<&char> {
        return self.chars.peek();
    }

    fn read_char(&mut self) -> Option<char> {
        return self.chars.next();
    }

    fn skip_whitespace(&mut self) {
        while let Some(_) = self.chars.next_if(|x| x.is_whitespace()) {}
    }

    fn keep_reading(&mut self, c: char, f: impl Fn(&char) -> bool) -> Vec<char> {
        let mut out = vec![c];
        while let Some(c) = self.chars.next_if(&f) {
            out.push(c);
        }

        return out;
    }
}
impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        while let Some(token) = self.read_char() {
            match token {
                '=' => {
                    if let Some(next_char) = self.peek() {
                        if *next_char == '='{
                            self.read_char();
                            return Some(Token::Equal);
                        }
                    }
                    return Some(Token::Assign);
                },
                '+' => return Some(Token::Plus),
                '(' => return Some(Token::LParen),
                ')' => return Some(Token::RParen),
                '{' => return Some(Token::LBrace),
                '}' => return Some(Token::RBrace),
                ',' => return Some(Token::Comma),
                ';' => return Some(Token::Semicolon),
                '-' => return Some(Token::Minus),
                '!' => {
                    if let Some(next_char) = self.peek() {
                        if *next_char == '='{
                            self.read_char();
                            return Some(Token::NotEqual);
                        }
                    }
                    return Some(Token::Bang)
                }
                '/' => return Some(Token::Slash),
                '*' => return Some(Token::Asterisk),
                '<' => return Some(Token::Lt),
                '>' => return Some(Token::Gt),

                _ => {
                    if token.is_alphabetic() {
                        let ident = self.keep_reading(token, |c| c.is_ascii_alphabetic());
                        let ident = ident.into_iter().collect::<String>();

                        if let Some((_, v)) = KEYWORDS.get_entry(&ident) {
                            return Some(v.clone());
                        }
                        return Some(Token::Identifier(ident));
                    }
                    if token.is_digit(10) {
                        let str = self.keep_reading(token, |c| c.is_digit(10));
                        let str = str.into_iter().collect::<String>();
                        return Some(Token::Int(
                            str::parse::<usize>(&str).expect("this should always work"),
                        ));
                    }
                }
            }
        }
        return None;

    }
}

#[cfg(test)]
mod test {
    use super::{Lexer, Token};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_iter_lexer() {
        let input = "=+(){},;";
        let expected = vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
        ];

        let output = Lexer::new(input).into_iter().collect::<Vec<Token>>();
        assert_eq!(output, expected);
    }
    #[test]
    fn test_lexer_1() {
        let input = "let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};
let result = add(five, ten);";
        let expected = vec![
            Token::Let,
            Token::Identifier(String::from("five")),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("ten")),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Identifier(String::from("x")),
            Token::Comma,
            Token::Identifier(String::from("y")),
            Token::RParen,
            Token::LBrace,
            Token::Identifier(String::from("x")),
            Token::Plus,
            Token::Identifier(String::from("y")),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("result")),
            Token::Assign,
            Token::Identifier(String::from("add")),
            Token::LParen,
            Token::Identifier(String::from("five")),
            Token::Comma,
            Token::Identifier(String::from("ten")),
            Token::RParen,
            Token::Semicolon,

        ];

        let output = Lexer::new(input).into_iter().collect::<Vec<Token>>();
        assert_eq!(output, expected);
    }



}
    #[test]
    fn test_lexer_2() {
        let input = "!-/*5;
5 < 10 > 5;";
        let expected = vec![
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

        ];

        let output = Lexer::new(input).into_iter().collect::<Vec<Token>>();
        assert_eq!(output, expected);
    }
    #[test]
    fn test_lexer_3() {
        let input = "10 == 10;
10 != 9;";
        let expected = vec![
            Token::Int(10),
            Token::Equal,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::NotEqual,
            Token::Int(9),
            Token::Semicolon,
        ];

        let output = Lexer::new(input).into_iter().collect::<Vec<Token>>();
        assert_eq!(output, expected);
    }
