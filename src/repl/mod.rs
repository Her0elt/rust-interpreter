use std::{cell::RefCell, rc::Rc};

use crate::{
    evaluator::{env::Env, Evaluator, Object},
    parser::{Parser, Stmt},
    token::{Lexer, Token},
};

pub struct Repl {}

impl Repl {
    pub fn new() -> Repl {
        return Repl {};
    }

    pub fn line(&self, line: &str) -> Option<Object> {
        let lex = Lexer::new(line);
        let mut parser = Parser::new(lex);
        let program = parser.parse_program();
        let env = Rc::new(RefCell::new(Env::new()));
        let mut evaluator = Evaluator::new(env);
        return evaluator.eval(program);
    }
}
