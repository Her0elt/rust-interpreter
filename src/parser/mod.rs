use crate::token::{Lexer, Token};

pub type Program = Vec<Stmt>;
#[derive(PartialEq, Debug, Eq, Clone)]
pub enum Stmt {
    LetStmt(Ident, Expr),
    ReturnStmt(Expr),
    ExprStmt(Expr),
}

#[derive(PartialEq, Debug, Eq, Clone)]
pub enum Expr {
    IdentExpr(Ident),
    LitExpr(Literal),
    PrefixExpr(Prefix, Box<Expr>),
    InfixExpr(Infix, Box<Expr>, Box<Expr>),
    IfExpr {
        cond: Box<Expr>,
        consequence: Program,
        alternative: Option<Program>,
    },
    FnExpr {
        params: Vec<Ident>,
        body: Program,
    },
    CallExpr {
        function: Box<Expr>,
        arguments: Vec<Expr>,
    },
    ArrayExpr(Vec<Expr>),
    HashExpr(Vec<(Literal, Expr)>),
    IndexExpr {
        array: Box<Expr>,
        index: Box<Expr>,
    },
}
#[derive(PartialEq, Debug, Eq, Clone)]
pub enum Prefix {
    PrefixPlus,
    PrefixMinus,
    Not,
}

#[derive(PartialEq, Debug, Eq, Clone)]
pub enum Infix {
    Plus,
    Minus,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

#[derive(PartialEq, Debug, Eq, Clone)]
pub enum Literal {
    IntLiteral(i64),
    BoolLiteral(bool),
    StringLiteral(String),
}

#[derive(PartialEq, Debug, Eq, Clone)]
pub struct Ident(pub String);

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    curr_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            curr_token: None,
            peek_token: None,
            errors: vec![],
        };
        parser.next_token();
        parser.next_token();
        return parser;
    }
    fn token_to_precedence(tok: &Token) -> Precedence {
        match tok {
            Token::Equal | Token::NotEqual => Precedence::Equals,
            Token::Lt => Precedence::LessGreater,
            Token::Gt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            Token::LBracket => Precedence::Index,
            Token::LParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    pub fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next();
    }

    fn peek_error(&mut self, token: &Token) {
        let msg = String::from(format!(
            "Wrong token expected {:?} got {:?}",
            token, self.peek_token
        ));
        self.errors.push(msg);
    }
    fn peek_token_is(&self, token: &Token) -> bool {
        if let Some(peek_token) = self.peek_token.clone() {
            return peek_token == *token;
        } else {
            return false;
        }
    }

    fn curr_token_is(&self, token: &Token) -> bool {
        if let Some(curr_token) = self.curr_token.clone() {
            return curr_token == *token;
        } else {
            return false;
        }
    }

    fn expect_peek(&mut self, token: &Token) -> bool {
        if self.peek_token_is(token) {
            self.next_token();
            return true;
        }
        self.peek_error(token);
        return false;
    }

    fn peek_token_precedence(&self) -> Precedence {
        return match self.peek_token.clone() {
            Some(Token::Equal) | Some(Token::NotEqual) => Precedence::Equals,
            Some(Token::Lt) => Precedence::LessGreater,
            Some(Token::Gt) => Precedence::LessGreater,
            Some(Token::Plus) | Some(Token::Minus) => Precedence::Sum,
            Some(Token::Slash) | Some(Token::Asterisk) => Precedence::Product,
            Some(Token::LBracket) => Precedence::Index,
            Some(Token::LParen) => Precedence::Call,
            _ => Precedence::Lowest,
        };
    }
    fn curr_token_precedence(&self) -> Precedence {
        return match self.curr_token.clone() {
            Some(Token::Equal) | Some(Token::NotEqual) => Precedence::Equals,
            Some(Token::Lt) => Precedence::LessGreater,
            Some(Token::Gt) => Precedence::LessGreater,
            Some(Token::Plus) | Some(Token::Minus) => Precedence::Sum,
            Some(Token::Slash) | Some(Token::Asterisk) => Precedence::Product,
            Some(Token::LBracket) => Precedence::Index,
            Some(Token::LParen) => Precedence::Call,
            _ => Precedence::Lowest,
        };
    }

    fn parse_ident(&mut self) -> Option<Ident> {
        if let Some(ident) = self.curr_token.clone() {
            return match ident {
                Token::Identifier(x) => Some(Ident(x)),
                _ => {
                    self.peek_error(&Token::Identifier(String::from("Value")));
                    None
                }
            };
        }
        return None;
    }
    fn parse_ident_expr(&mut self) -> Option<Expr> {
        return match self.parse_ident() {
            Some(ident) => Some(Expr::IdentExpr(ident)),
            _ => None,
        };
    }
    fn parse_int_expr(&mut self) -> Option<Expr> {
        if let Some(curr_token) = self.curr_token.clone() {
            return match curr_token {
                Token::Int(int) => Some(Expr::LitExpr(Literal::IntLiteral(int.clone()))),
                _ => None,
            };
        }
        return None;
    }
    fn parse_string_expr(&mut self) -> Option<Expr> {
        if let Some(curr_token) = self.curr_token.clone() {
            return match curr_token {
                Token::String(string) => {
                    Some(Expr::LitExpr(Literal::StringLiteral(string.clone())))
                }
                _ => None,
            };
        }
        return None;
    }

    fn parse_bool_expr(&mut self) -> Option<Expr> {
        if let Some(curr_token) = self.curr_token.clone() {
            return match curr_token {
                Token::True => Some(Expr::LitExpr(Literal::BoolLiteral(true))),
                Token::False => Some(Expr::LitExpr(Literal::BoolLiteral(false))),
                _ => None,
            };
        }
        return None;
    }
    fn parse_infix_expr(&mut self, left: Expr) -> Option<Expr> {
        let infix = match self.curr_token.clone() {
            Some(Token::Plus) => Infix::Plus,
            Some(Token::Minus) => Infix::Minus,
            Some(Token::Slash) => Infix::Divide,
            Some(Token::Asterisk) => Infix::Multiply,
            Some(Token::Equal) => Infix::Equal,
            Some(Token::NotEqual) => Infix::NotEqual,
            Some(Token::Lt) => Infix::LessThan,
            Some(Token::Gt) => Infix::GreaterThan,
            _ => return None,
        };

        let precedence = self.curr_token_precedence();

        self.next_token();

        return match self.parse_expr(precedence) {
            Some(expr) => Some(Expr::InfixExpr(infix, Box::new(left), Box::new(expr))),
            None => None,
        };
    }
    fn parse_index_expr(&mut self, left: Expr) -> Option<Expr> {
        self.next_token();

        let index = match self.parse_expr(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        if !self.expect_peek(&Token::RBracket) {
            return None;
        }

        return Some(Expr::IndexExpr {
            array: Box::new(left),
            index: Box::new(index),
        });
    }
    fn parse_expr_list(&mut self, end: Token) -> Option<Vec<Expr>> {
        let mut list = vec![];

        if self.peek_token_is(&end) {
            self.next_token();
            return Some(list);
        }

        self.next_token();

        match self.parse_expr(Precedence::Lowest) {
            Some(expr) => list.push(expr),
            None => return None,
        }
        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();

            match self.parse_expr(Precedence::Lowest) {
                Some(expr) => list.push(expr),
                None => return None,
            }
        }

        if !self.expect_peek(&end) {
            return None;
        }

        return Some(list);
    }
    fn parse_call_expr(&mut self, func: Expr) -> Option<Expr> {
        let args = match self.parse_expr_list(Token::RParen) {
            Some(args) => args,
            None => return None,
        };

        return Some(Expr::CallExpr {
            function: Box::new(func),
            arguments: args,
        });
    }
    fn parse_prefix_expr(&mut self) -> Option<Expr> {
        let prefix = match self.curr_token {
            Some(Token::Bang) => Prefix::Not,
            Some(Token::Minus) => Prefix::PrefixMinus,
            Some(Token::Plus) => Prefix::PrefixPlus,
            _ => return None,
        };

        self.next_token();

        match self.parse_expr(Precedence::Prefix) {
            Some(expr) => Some(Expr::PrefixExpr(prefix, Box::new(expr))),
            None => None,
        }
    }
    fn parse_grouped_expr(&mut self) -> Option<Expr> {
        self.next_token();

        let expr = self.parse_expr(Precedence::Lowest);

        if !self.expect_peek(&Token::RParen) {
            return None;
        }
        return expr;
    }
    fn parse_block_stmt(&mut self) -> Program {
        self.next_token();

        let mut block = vec![];

        while !self.curr_token_is(&Token::RBrace) && self.curr_token != None {
            match self.parse_statement() {
                Some(stmt) => block.push(stmt),
                None => {}
            }
            self.next_token();
        }
        return block;
    }
    fn parse_if_expr(&mut self) -> Option<Expr> {
        if !self.expect_peek(&Token::LParen) {
            return None;
        }
        self.next_token();
        let cond = match self.parse_expr(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        if !self.expect_peek(&Token::RParen) || !self.expect_peek(&Token::LBrace) {
            return None;
        }
        let cons = self.parse_block_stmt();
        let mut alternative = None;
        if self.peek_token_is(&Token::Else) {
            self.next_token();

            if !self.expect_peek(&Token::LBrace) {
                return None;
            }

            alternative = Some(self.parse_block_stmt());
        }

        return Some(Expr::IfExpr {
            cond: Box::new(cond),
            consequence: cons,
            alternative,
        });
    }
    fn parse_function_parameters(&mut self) -> Option<Vec<Ident>> {
        let mut params = vec![];
        if self.peek_token_is(&Token::RParen) {
            self.next_token();
            return Some(params);
        }
        self.next_token();
        match self.parse_ident() {
            Some(ident) => params.push(ident),
            _ => return None,
        };
        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();

            match self.parse_ident() {
                Some(ident) => params.push(ident),
                _ => return None,
            };
        }
        if !self.expect_peek(&Token::RParen) {
            return None;
        }

        return Some(params);
    }
    fn parse_function_expr(&mut self) -> Option<Expr> {
        if !self.expect_peek(&Token::LParen) {
            return None;
        }

        let params = match self.parse_function_parameters() {
            Some(params) => params,
            _ => return None,
        };

        if !self.expect_peek(&Token::LBrace) {
            return None;
        }
        let body = self.parse_block_stmt();

        return Some(Expr::FnExpr { params, body });
    }
    fn parse_array_expr(&mut self) -> Option<Expr> {
        return match self.parse_expr_list(Token::RBracket) {
            Some(list) => Some(Expr::ArrayExpr(list)),
            None => None,
        };
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Option<Expr> {
        let mut left = match self.curr_token {
            Some(Token::Identifier(_)) => self.parse_ident_expr(),
            Some(Token::Int(_)) => self.parse_int_expr(),
            Some(Token::String(_)) => self.parse_string_expr(),
            Some(Token::True) | Some(Token::False) => self.parse_bool_expr(),
            Some(Token::Bang) | Some(Token::Minus) | Some(Token::Plus) => self.parse_prefix_expr(),
            Some(Token::LParen) => self.parse_grouped_expr(),
            Some(Token::LBracket) => self.parse_array_expr(),
            Some(Token::LBrace) => self.parse_array_expr(),
            Some(Token::If) => self.parse_if_expr(),
            Some(Token::Function) => self.parse_function_expr(),
            _ => None,
        };

        while !self.curr_token_is(&Token::Semicolon) && precedence < self.peek_token_precedence() {
            match self.peek_token.clone() {
                Some(Token::Plus)
                | Some(Token::Minus)
                | Some(Token::Slash)
                | Some(Token::Asterisk)
                | Some(Token::Equal)
                | Some(Token::NotEqual)
                | Some(Token::Lt)
                | Some(Token::Gt) => {
                    self.next_token();
                    left = self.parse_infix_expr(left.unwrap());
                }
                Some(Token::LBracket) => {
                    self.next_token();
                    left = self.parse_index_expr(left.unwrap());
                }

                Some(Token::LParen) => {
                    self.next_token();
                    left = self.parse_call_expr(left.unwrap());
                }
                _ => return left,
            }
        }
        return left;
    }

    fn parse_expr_statment(&mut self) -> Option<Stmt> {
        return match self.parse_expr(Precedence::Lowest) {
            Some(expr) => {
                if self.curr_token_is(&Token::Semicolon) {
                    self.next_token();
                }
                Some(Stmt::ExprStmt(expr))
            }
            _ => None,
        };
    }

    fn parse_let_statement(&mut self) -> Option<Stmt> {
        match self.peek_token.clone() {
            Some(Token::Identifier(_)) => self.next_token(),
            _ => return None,
        };
        let ident = self.parse_ident();

        if !self.expect_peek(&Token::Assign) {
            return None;
        }
        self.next_token();

        let expr = match self.parse_expr(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        while !self.curr_token_is(&Token::Semicolon) {
            self.next_token();
        }
        return match ident {
            Some(id) => Some(Stmt::LetStmt(id, expr)),
            _ => None,
        };
    }
    pub fn parse_return_statement(&mut self) -> Option<Stmt> {
        self.next_token();
        let expr = self.parse_expr(Precedence::Lowest);
        while !self.curr_token_is(&Token::Semicolon) {
            self.next_token();
        }
        return match expr {
            Some(expr) => Some(Stmt::ReturnStmt(expr)),
            _ => None,
        };
    }

    pub fn parse_statement(&mut self) -> Option<Stmt> {
        return match self.curr_token {
            Some(Token::Let) => self.parse_let_statement(),
            Some(Token::Return) => self.parse_return_statement(),
            _ => self.parse_expr_statment(),
        };
    }
    pub fn parse_program(&mut self) -> Program {
        let mut program: Program = vec![];
        while self.curr_token != None {
            if let Some(stmt) = self.parse_statement() {
                program.push(stmt);
            }
            self.next_token();
        }
        return program;
    }
}

#[cfg(test)]
mod test {
    use std::vec;

    use super::{Expr, Ident, Infix, Lexer, Literal, Parser, Prefix, Program, Stmt};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_string_statements() {
        let input = "\"hello world\"";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let expected: Program = vec![Stmt::ExprStmt(Expr::LitExpr(Literal::StringLiteral(
            "hello world".to_owned(),
        )))];
        assert_eq!(program, expected);
    }

    #[test]
    fn test_let_statements() {
        let input = "let x = 5;
let y = 10;
let foobar = 838383;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let expected: Program = vec![
            Stmt::LetStmt(
                Ident(String::from("x")),
                Expr::LitExpr(Literal::IntLiteral(5)),
            ),
            Stmt::LetStmt(
                Ident(String::from("y")),
                Expr::LitExpr(Literal::IntLiteral(10)),
            ),
            Stmt::LetStmt(
                Ident(String::from("foobar")),
                Expr::LitExpr(Literal::IntLiteral(838383)),
            ),
        ];
        assert_eq!(program, expected);
    }
    #[test]
    fn test_return_statements() {
        let input = "return 5;
return 10;
return 993322;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let expected: Program = vec![
            Stmt::ReturnStmt(Expr::LitExpr(Literal::IntLiteral(5))),
            Stmt::ReturnStmt(Expr::LitExpr(Literal::IntLiteral(10))),
            Stmt::ReturnStmt(Expr::LitExpr(Literal::IntLiteral(993322))),
        ];
        assert_eq!(program, expected);
    }
    #[test]
    fn test_ident_expression_statements() {
        let input = "foobar";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let expected: Program = vec![Stmt::ExprStmt(Expr::IdentExpr(Ident("foobar".to_owned())))];
        assert_eq!(program, expected);
    }

    #[test]
    fn test_int_expression_statements() {
        let input = "5;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let expected: Program = vec![Stmt::ExprStmt(Expr::LitExpr(Literal::IntLiteral(5)))];
        assert_eq!(program, expected);
    }

    #[test]
    fn test_prefix_expression_statements() {
        let input = "!5;
        -15;
        +15;
        !true;
        !false;
        ";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let expected = vec![
            Stmt::ExprStmt(Expr::PrefixExpr(
                Prefix::Not,
                Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
            )),
            Stmt::ExprStmt(Expr::PrefixExpr(
                Prefix::PrefixMinus,
                Box::new(Expr::LitExpr(Literal::IntLiteral(15))),
            )),
            Stmt::ExprStmt(Expr::PrefixExpr(
                Prefix::PrefixPlus,
                Box::new(Expr::LitExpr(Literal::IntLiteral(15))),
            )),
            Stmt::ExprStmt(Expr::PrefixExpr(
                Prefix::Not,
                Box::new(Expr::LitExpr(Literal::BoolLiteral(true))),
            )),
            Stmt::ExprStmt(Expr::PrefixExpr(
                Prefix::Not,
                Box::new(Expr::LitExpr(Literal::BoolLiteral(false))),
            )),
        ];
        assert_eq!(program, expected);
    }
    #[test]
    fn test_infix_expression_statements() {
        let input = "-a*b
            !-a
            a+b+c
            a+b-c
            a*b*c
            a*b/c
            a+b/c
            a+b*c+d/e-f
            3+4;-5*5
            5>4==3<4
            5<4!=3>4
            3+4*5==3*1+4*5
            true == true
            true != false
            false == false
            a + add(b * c) + d
            add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))
            add(a + b + c * d / f + g)
            ";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let expected = vec![
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Multiply,
                Box::new(Expr::PrefixExpr(
                    Prefix::PrefixMinus,
                    Box::new(Expr::IdentExpr(Ident(String::from("a")))),
                )),
                Box::new(Expr::IdentExpr(Ident(String::from("b")))),
            )),
            Stmt::ExprStmt(Expr::PrefixExpr(
                Prefix::Not,
                Box::new(Expr::PrefixExpr(
                    Prefix::PrefixMinus,
                    Box::new(Expr::IdentExpr(Ident(String::from("a")))),
                )),
            )),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Plus,
                Box::new(Expr::InfixExpr(
                    Infix::Plus,
                    Box::new(Expr::IdentExpr(Ident(String::from("a")))),
                    Box::new(Expr::IdentExpr(Ident(String::from("b")))),
                )),
                Box::new(Expr::IdentExpr(Ident(String::from("c")))),
            )),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Minus,
                Box::new(Expr::InfixExpr(
                    Infix::Plus,
                    Box::new(Expr::IdentExpr(Ident(String::from("a")))),
                    Box::new(Expr::IdentExpr(Ident(String::from("b")))),
                )),
                Box::new(Expr::IdentExpr(Ident(String::from("c")))),
            )),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Multiply,
                Box::new(Expr::InfixExpr(
                    Infix::Multiply,
                    Box::new(Expr::IdentExpr(Ident(String::from("a")))),
                    Box::new(Expr::IdentExpr(Ident(String::from("b")))),
                )),
                Box::new(Expr::IdentExpr(Ident(String::from("c")))),
            )),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Divide,
                Box::new(Expr::InfixExpr(
                    Infix::Multiply,
                    Box::new(Expr::IdentExpr(Ident(String::from("a")))),
                    Box::new(Expr::IdentExpr(Ident(String::from("b")))),
                )),
                Box::new(Expr::IdentExpr(Ident(String::from("c")))),
            )),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Plus,
                Box::new(Expr::IdentExpr(Ident(String::from("a")))),
                Box::new(Expr::InfixExpr(
                    Infix::Divide,
                    Box::new(Expr::IdentExpr(Ident(String::from("b")))),
                    Box::new(Expr::IdentExpr(Ident(String::from("c")))),
                )),
            )),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Minus,
                Box::new(Expr::InfixExpr(
                    Infix::Plus,
                    Box::new(Expr::InfixExpr(
                        Infix::Plus,
                        Box::new(Expr::IdentExpr(Ident(String::from("a")))),
                        Box::new(Expr::InfixExpr(
                            Infix::Multiply,
                            Box::new(Expr::IdentExpr(Ident(String::from("b")))),
                            Box::new(Expr::IdentExpr(Ident(String::from("c")))),
                        )),
                    )),
                    Box::new(Expr::InfixExpr(
                        Infix::Divide,
                        Box::new(Expr::IdentExpr(Ident(String::from("d")))),
                        Box::new(Expr::IdentExpr(Ident(String::from("e")))),
                    )),
                )),
                Box::new(Expr::IdentExpr(Ident(String::from("f")))),
            )),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Plus,
                Box::new(Expr::LitExpr(Literal::IntLiteral(3))),
                Box::new(Expr::LitExpr(Literal::IntLiteral(4))),
            )),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Multiply,
                Box::new(Expr::PrefixExpr(
                    Prefix::PrefixMinus,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
                )),
                Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
            )),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Equal,
                Box::new(Expr::InfixExpr(
                    Infix::GreaterThan,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(4))),
                )),
                Box::new(Expr::InfixExpr(
                    Infix::LessThan,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(3))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(4))),
                )),
            )),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::NotEqual,
                Box::new(Expr::InfixExpr(
                    Infix::LessThan,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(4))),
                )),
                Box::new(Expr::InfixExpr(
                    Infix::GreaterThan,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(3))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(4))),
                )),
            )),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Equal,
                Box::new(Expr::InfixExpr(
                    Infix::Plus,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(3))),
                    Box::new(Expr::InfixExpr(
                        Infix::Multiply,
                        Box::new(Expr::LitExpr(Literal::IntLiteral(4))),
                        Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
                    )),
                )),
                Box::new(Expr::InfixExpr(
                    Infix::Plus,
                    Box::new(Expr::InfixExpr(
                        Infix::Multiply,
                        Box::new(Expr::LitExpr(Literal::IntLiteral(3))),
                        Box::new(Expr::LitExpr(Literal::IntLiteral(1))),
                    )),
                    Box::new(Expr::InfixExpr(
                        Infix::Multiply,
                        Box::new(Expr::LitExpr(Literal::IntLiteral(4))),
                        Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
                    )),
                )),
            )),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Equal,
                Box::new(Expr::LitExpr(Literal::BoolLiteral(true))),
                Box::new(Expr::LitExpr(Literal::BoolLiteral(true))),
            )),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::NotEqual,
                Box::new(Expr::LitExpr(Literal::BoolLiteral(true))),
                Box::new(Expr::LitExpr(Literal::BoolLiteral(false))),
            )),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Equal,
                Box::new(Expr::LitExpr(Literal::BoolLiteral(false))),
                Box::new(Expr::LitExpr(Literal::BoolLiteral(false))),
            )),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Plus,
                Box::new(Expr::InfixExpr(
                    Infix::Plus,
                    Box::new(Expr::IdentExpr(Ident(String::from("a")))),
                    Box::new(Expr::CallExpr {
                        function: Box::new(Expr::IdentExpr(Ident(String::from("add")))),
                        arguments: vec![Expr::InfixExpr(
                            Infix::Multiply,
                            Box::new(Expr::IdentExpr(Ident(String::from("b")))),
                            Box::new(Expr::IdentExpr(Ident(String::from("c")))),
                        )],
                    }),
                )),
                Box::new(Expr::IdentExpr(Ident(String::from("d")))),
            )),
            Stmt::ExprStmt(Expr::CallExpr {
                function: Box::new(Expr::IdentExpr(Ident(String::from("add")))),
                arguments: vec![
                    Expr::IdentExpr(Ident(String::from("a"))),
                    Expr::IdentExpr(Ident(String::from("b"))),
                    Expr::LitExpr(Literal::IntLiteral(1)),
                    Expr::InfixExpr(
                        Infix::Multiply,
                        Box::new(Expr::LitExpr(Literal::IntLiteral(2))),
                        Box::new(Expr::LitExpr(Literal::IntLiteral(3))),
                    ),
                    Expr::InfixExpr(
                        Infix::Plus,
                        Box::new(Expr::LitExpr(Literal::IntLiteral(4))),
                        Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
                    ),
                    Expr::CallExpr {
                        function: Box::new(Expr::IdentExpr(Ident(String::from("add")))),
                        arguments: vec![
                            Expr::LitExpr(Literal::IntLiteral(6)),
                            Expr::InfixExpr(
                                Infix::Multiply,
                                Box::new(Expr::LitExpr(Literal::IntLiteral(7))),
                                Box::new(Expr::LitExpr(Literal::IntLiteral(8))),
                            ),
                        ],
                    },
                ],
            }),
            Stmt::ExprStmt(Expr::CallExpr {
                function: Box::new(Expr::IdentExpr(Ident(String::from("add")))),
                arguments: vec![Expr::InfixExpr(
                    Infix::Plus,
                    Box::new(Expr::InfixExpr(
                        Infix::Plus,
                        Box::new(Expr::InfixExpr(
                            Infix::Plus,
                            Box::new(Expr::IdentExpr(Ident(String::from("a")))),
                            Box::new(Expr::IdentExpr(Ident(String::from("b")))),
                        )),
                        Box::new(Expr::InfixExpr(
                            Infix::Divide,
                            Box::new(Expr::InfixExpr(
                                Infix::Multiply,
                                Box::new(Expr::IdentExpr(Ident(String::from("c")))),
                                Box::new(Expr::IdentExpr(Ident(String::from("d")))),
                            )),
                            Box::new(Expr::IdentExpr(Ident(String::from("f")))),
                        )),
                    )),
                    Box::new(Expr::IdentExpr(Ident(String::from("g")))),
                )],
            }),
        ];
        assert_eq!(program, expected);
    }

    #[test]
    fn test_bool_expression_statements() {
        let input = "true
            false
            3>5==false
            3<5 == true";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let expected = vec![
            Stmt::ExprStmt(Expr::LitExpr(Literal::BoolLiteral(true))),
            Stmt::ExprStmt(Expr::LitExpr(Literal::BoolLiteral(false))),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Equal,
                Box::new(Expr::InfixExpr(
                    Infix::GreaterThan,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(3))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
                )),
                Box::new(Expr::LitExpr(Literal::BoolLiteral(false))),
            )),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Equal,
                Box::new(Expr::InfixExpr(
                    Infix::LessThan,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(3))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
                )),
                Box::new(Expr::LitExpr(Literal::BoolLiteral(true))),
            )),
        ];
        assert_eq!(program, expected);
    }
    #[test]
    fn test_grouped_expressions() {
        let input = "1 + (2+3) + 4;
        (5+5)*2;
        2/(5+5);
        -(5+5);
        !(true==true);
        ";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let expected = vec![
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Plus,
                Box::new(Expr::InfixExpr(
                    Infix::Plus,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(1))),
                    Box::new(Expr::InfixExpr(
                        Infix::Plus,
                        Box::new(Expr::LitExpr(Literal::IntLiteral(2))),
                        Box::new(Expr::LitExpr(Literal::IntLiteral(3))),
                    )),
                )),
                Box::new(Expr::LitExpr(Literal::IntLiteral(4))),
            )),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Multiply,
                Box::new(Expr::InfixExpr(
                    Infix::Plus,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
                )),
                Box::new(Expr::LitExpr(Literal::IntLiteral(2))),
            )),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Divide,
                Box::new(Expr::LitExpr(Literal::IntLiteral(2))),
                Box::new(Expr::InfixExpr(
                    Infix::Plus,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
                )),
            )),
            Stmt::ExprStmt(Expr::PrefixExpr(
                Prefix::PrefixMinus,
                Box::new(Expr::InfixExpr(
                    Infix::Plus,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
                )),
            )),
            Stmt::ExprStmt(Expr::PrefixExpr(
                Prefix::Not,
                Box::new(Expr::InfixExpr(
                    Infix::Equal,
                    Box::new(Expr::LitExpr(Literal::BoolLiteral(true))),
                    Box::new(Expr::LitExpr(Literal::BoolLiteral(true))),
                )),
            )),
        ];
        assert_eq!(program, expected);
    }
    #[test]
    fn test_if_expressions() {
        let input = "
            if (x < y) { x }
            if (x < y) { x } else { y }
        ";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let expected = vec![
            Stmt::ExprStmt(Expr::IfExpr {
                cond: Box::new(Expr::InfixExpr(
                    Infix::LessThan,
                    Box::new(Expr::IdentExpr(Ident("x".to_owned()))),
                    Box::new(Expr::IdentExpr(Ident("y".to_owned()))),
                )),
                consequence: vec![Stmt::ExprStmt(Expr::IdentExpr(Ident("x".to_owned())))],
                alternative: None,
            }),
            Stmt::ExprStmt(Expr::IfExpr {
                cond: Box::new(Expr::InfixExpr(
                    Infix::LessThan,
                    Box::new(Expr::IdentExpr(Ident("x".to_owned()))),
                    Box::new(Expr::IdentExpr(Ident("y".to_owned()))),
                )),
                consequence: vec![Stmt::ExprStmt(Expr::IdentExpr(Ident("x".to_owned())))],
                alternative: Some(vec![Stmt::ExprStmt(Expr::IdentExpr(Ident("y".to_owned())))]),
            }),
        ];
        assert_eq!(program, expected);
    }
    #[test]
    fn test_function_expressions() {
        let input = "
        fn(x, y) { x + y; };
        fn() {};
        fn(x) {};
        fn(x, y, z) {};
        ";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let expected = vec![
            Stmt::ExprStmt(Expr::FnExpr {
                params: vec![Ident("x".to_owned()), Ident("y".to_owned())],
                body: vec![Stmt::ExprStmt(Expr::InfixExpr(
                    Infix::Plus,
                    Box::new(Expr::IdentExpr(Ident("x".to_owned()))),
                    Box::new(Expr::IdentExpr(Ident("y".to_owned()))),
                ))],
            }),
            Stmt::ExprStmt(Expr::FnExpr {
                params: vec![],
                body: vec![],
            }),
            Stmt::ExprStmt(Expr::FnExpr {
                params: vec![Ident("x".to_owned())],
                body: vec![],
            }),
            Stmt::ExprStmt(Expr::FnExpr {
                params: vec![
                    Ident("x".to_owned()),
                    Ident("y".to_owned()),
                    Ident("z".to_owned()),
                ],
                body: vec![],
            }),
        ];
        assert_eq!(program, expected);
    }
    #[test]
    fn test_call_expressions() {
        let input = "
        add(1, 2 * 3, 4 + 5);
        ";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let expected = vec![Stmt::ExprStmt(Expr::CallExpr {
            function: Box::new(Expr::IdentExpr(Ident("add".to_owned()))),
            arguments: vec![
                Expr::LitExpr(Literal::IntLiteral(1)),
                Expr::InfixExpr(
                    Infix::Multiply,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(2))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(3))),
                ),
                Expr::InfixExpr(
                    Infix::Plus,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(4))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
                ),
            ],
        })];
        assert_eq!(program, expected);
    }
    #[test]
    fn test_array_expressions() {
        let input = "
        [1, 2 * 2, 3 + 3];
        a * [1, 2, 3, 4][b * c] * d;
        add(a * b[2], b[1], 2 * [1, 2][1]);
        ";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let expected = vec![
            Stmt::ExprStmt(Expr::ArrayExpr(vec![
                Expr::LitExpr(Literal::IntLiteral(1)),
                Expr::InfixExpr(
                    Infix::Multiply,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(2))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(2))),
                ),
                Expr::InfixExpr(
                    Infix::Plus,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(3))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(3))),
                ),
            ])),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Multiply,
                Box::new(Expr::InfixExpr(
                    Infix::Multiply,
                    Box::new(Expr::IdentExpr(Ident(String::from("a")))),
                    Box::new(Expr::IndexExpr {
                        array: Box::new(Expr::ArrayExpr(vec![
                            Expr::LitExpr(Literal::IntLiteral(1)),
                            Expr::LitExpr(Literal::IntLiteral(2)),
                            Expr::LitExpr(Literal::IntLiteral(3)),
                            Expr::LitExpr(Literal::IntLiteral(4)),
                        ])),
                        index: Box::new(Expr::InfixExpr(
                            Infix::Multiply,
                            Box::new(Expr::IdentExpr(Ident(String::from("b")))),
                            Box::new(Expr::IdentExpr(Ident(String::from("c")))),
                        )),
                    }),
                )),
                Box::new(Expr::IdentExpr(Ident(String::from("d")))),
            )),
            Stmt::ExprStmt(Expr::CallExpr {
                function: Box::new(Expr::IdentExpr(Ident(String::from("add")))),
                arguments: vec![
                    Expr::InfixExpr(
                        Infix::Multiply,
                        Box::new(Expr::IdentExpr(Ident(String::from("a")))),
                        Box::new(Expr::IndexExpr {
                            array: Box::new(Expr::IdentExpr(Ident(String::from("b")))),
                            index: Box::new(Expr::LitExpr(Literal::IntLiteral(2))),
                        }),
                    ),
                    Expr::IndexExpr {
                        array: Box::new(Expr::IdentExpr(Ident(String::from("b")))),
                        index: Box::new(Expr::LitExpr(Literal::IntLiteral(1))),
                    },
                    Expr::InfixExpr(
                        Infix::Multiply,
                        Box::new(Expr::LitExpr(Literal::IntLiteral(2))),
                        Box::new(Expr::IndexExpr {
                            array: Box::new(Expr::ArrayExpr(vec![
                                Expr::LitExpr(Literal::IntLiteral(1)),
                                Expr::LitExpr(Literal::IntLiteral(2)),
                            ])),
                            index: Box::new(Expr::LitExpr(Literal::IntLiteral(1))),
                        }),
                    ),
                ],
            }),
        ];
        assert_eq!(program, expected);
    }
    #[test]
    fn test_array_expressions() {
        let input = "
        ";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let expected = vec![
            Stmt::ExprStmt(Expr::ArrayExpr(vec![
                Expr::LitExpr(Literal::IntLiteral(1)),
                Expr::InfixExpr(
                    Infix::Multiply,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(2))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(2))),
                ),
                Expr::InfixExpr(
                    Infix::Plus,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(3))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(3))),
                ),
            ])),
            Stmt::ExprStmt(Expr::InfixExpr(
                Infix::Multiply,
                Box::new(Expr::InfixExpr(
                    Infix::Multiply,
                    Box::new(Expr::IdentExpr(Ident(String::from("a")))),
                    Box::new(Expr::IndexExpr {
                        array: Box::new(Expr::ArrayExpr(vec![
                            Expr::LitExpr(Literal::IntLiteral(1)),
                            Expr::LitExpr(Literal::IntLiteral(2)),
                            Expr::LitExpr(Literal::IntLiteral(3)),
                            Expr::LitExpr(Literal::IntLiteral(4)),
                        ])),
                        index: Box::new(Expr::InfixExpr(
                            Infix::Multiply,
                            Box::new(Expr::IdentExpr(Ident(String::from("b")))),
                            Box::new(Expr::IdentExpr(Ident(String::from("c")))),
                        )),
                    }),
                )),
                Box::new(Expr::IdentExpr(Ident(String::from("d")))),
            )),
            Stmt::ExprStmt(Expr::CallExpr {
                function: Box::new(Expr::IdentExpr(Ident(String::from("add")))),
                arguments: vec![
                    Expr::InfixExpr(
                        Infix::Multiply,
                        Box::new(Expr::IdentExpr(Ident(String::from("a")))),
                        Box::new(Expr::IndexExpr {
                            array: Box::new(Expr::IdentExpr(Ident(String::from("b")))),
                            index: Box::new(Expr::LitExpr(Literal::IntLiteral(2))),
                        }),
                    ),
                    Expr::IndexExpr {
                        array: Box::new(Expr::IdentExpr(Ident(String::from("b")))),
                        index: Box::new(Expr::LitExpr(Literal::IntLiteral(1))),
                    },
                    Expr::InfixExpr(
                        Infix::Multiply,
                        Box::new(Expr::LitExpr(Literal::IntLiteral(2))),
                        Box::new(Expr::IndexExpr {
                            array: Box::new(Expr::ArrayExpr(vec![
                                Expr::LitExpr(Literal::IntLiteral(1)),
                                Expr::LitExpr(Literal::IntLiteral(2)),
                            ])),
                            index: Box::new(Expr::LitExpr(Literal::IntLiteral(1))),
                        }),
                    ),
                ],
            }),
        ];
        assert_eq!(program, expected);
    }
}
