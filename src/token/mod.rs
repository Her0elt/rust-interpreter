use std::{iter::Peekable, str::Chars};

#[derive(PartialEq, Debug, Eq, Clone)]
pub enum Token {
    Illegal,
    EOF,
    Int(i64),
    Identifier(String),
    String(String),
    Assign,
    Plus,
    Comma,
    Colon,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
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

#[derive(Debug, Clone)]
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
                        if *next_char == '=' {
                            self.read_char();
                            return Some(Token::Equal);
                        }
                    }
                    return Some(Token::Assign);
                }
                '+' => return Some(Token::Plus),
                '(' => return Some(Token::LParen),
                ')' => return Some(Token::RParen),
                '{' => return Some(Token::LBrace),
                '}' => return Some(Token::RBrace),
                '[' => return Some(Token::LBracket),
                ']' => return Some(Token::RBracket),
                ',' => return Some(Token::Comma),
                ';' => return Some(Token::Semicolon),
                ':' => return Some(Token::Colon),
                '-' => return Some(Token::Minus),
                '!' => {
                    if let Some(next_char) = self.peek() {
                        if *next_char == '=' {
                            self.read_char();
                            return Some(Token::NotEqual);
                        }
                    }
                    return Some(Token::Bang);
                }
                '/' => return Some(Token::Slash),
                '*' => return Some(Token::Asterisk),
                '<' => return Some(Token::Lt),
                '>' => return Some(Token::Gt),
                '\"' => {
                    if let Some(start) = self.read_char() {
                        if start != '\"' {
                            let string = self
                                .keep_reading(start, |c| *c != '\"')
                                .into_iter()
                                .collect();
                            self.read_char();
                            return Some(Token::String(string));
                        }
                        return Some(Token::String("".to_owned()));
                    }
                    return None;
                }
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
                            str::parse::<i64>(&str).expect("this should always work"),
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
    #[test]
    fn test_lexer_4() {
        let input = "\"foobar\"";
        let expected = vec![Token::String("foobar".to_owned())];
        let output = Lexer::new(input).into_iter().collect::<Vec<Token>>();
        assert_eq!(output, expected);
    }
    #[test]
    fn test_lexer_5() {
        let input = "[1,2]";
        let expected = vec![
            Token::LBracket,
            Token::Int(1),
            Token::Comma,
            Token::Int(2),
            Token::RBracket,
        ];
        let output = Lexer::new(input).into_iter().collect::<Vec<Token>>();
        assert_eq!(output, expected);
    }
    #[test]
    fn test_lexer_6() {
        let input = "{\"foo\": \"bar\"}";
        let expected = vec![
            Token::LBrace,
            Token::String("foo".to_owned()),
            Token::Colon,
            Token::String("bar".to_owned()),
            Token::RBrace,
        ];
        let output = Lexer::new(input).into_iter().collect::<Vec<Token>>();
        assert_eq!(output, expected);
    }
}
