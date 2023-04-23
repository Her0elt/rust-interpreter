use std::{env, io::{BufReader, BufRead}, fs};
use std::{cell::RefCell, rc::Rc};

use rust_interpreter::token::Lexer;
use rust_interpreter::parser::Parser;
use rust_interpreter::evaluator::{builtins::new_builtins, env::Env, Evaluator};

fn main() {
  let input = env::args().nth(1).unwrap();
  let file = fs::read_to_string(input).unwrap();
  let lexer = Lexer::new(&file[..]);
  let mut parser = Parser::new(lexer);
  let program = parser.parse_program();
    let env = Rc::new(RefCell::new(Env::from(new_builtins())));
    let mut evaluator = Evaluator::new(env);
    if let Some(ans) = evaluator.eval(program) {
        println!("{}", ans);
    }else {
        println!("Something vent wrong or you did not end the file with a return value");
    }

}
