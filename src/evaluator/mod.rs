use std::{array, cell::RefCell, rc::Rc, fmt, collections::HashMap};

use crate::parser::{Expr, Ident, Infix, Literal, Prefix, Program, Stmt};
use std::hash::{Hash, Hasher};
use self::env::Env;
pub mod builtins;
pub mod env;

pub type BuiltinFunc = fn(Vec<Object>) -> Object;

#[derive(PartialEq, Debug, Clone)]
pub enum Object {
    Int(i64),
    Bool(bool),
    String(String),
    NULL,
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
    ReturnValue(Box<Object>),
    Error(String),
    Func(Vec<Ident>, Program, Rc<RefCell<Env>>),
    Builtin(i32, BuiltinFunc),
}
impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Object::Int(ref value) => write!(f, "{}", value),
            Object::String(ref value) => write!(f, "{}", value),
            Object::Bool(ref value) => write!(f, "{}", value),
            Object::Array(ref objects) => {
                let mut result = String::new();
                for (i, obj) in objects.iter().enumerate() {
                    if i < 1 {
                        result.push_str(&format!("{}", obj));
                    } else {
                        result.push_str(&format!(", {}", obj));
                    }
                }
                write!(f, "[{}]", result)
            }
            Object::Hash(ref hash) => {
                let mut result = String::new();
                for (i, (k, v)) in hash.iter().enumerate() {
                    if i < 1 {
                        result.push_str(&format!("{}: {}", k, v));
                    } else {
                        result.push_str(&format!(", {}: {}", k, v));
                    }
                }
                write!(f, "{{{}}}", result)
            }
            Object::Func(ref params, _, _) => {
                let mut result = String::new();
                for (i, Ident(ref s)) in params.iter().enumerate() {
                    if i < 1 {
                        result.push_str(&format!("{}", s));
                    } else {
                        result.push_str(&format!(", {}", s));
                    }
                }
                write!(f, "fn({}) {{ ... }}", result)
            }
            Object::Builtin(_, _) => write!(f, "[builtin function]"),
            Object::NULL => write!(f, "null"),
            Object::ReturnValue(ref value) => write!(f, "{}", value),
            Object::Error(ref value) => write!(f, "{}", value),
        }
    }
}
impl Eq for Object {}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Object::Int(ref i) => i.hash(state),
            Object::Bool(ref b) => b.hash(state),
            Object::String(ref s) => s.hash(state),
            _ => "".hash(state),
        }
    }
}

#[derive(PartialEq, Debug, Eq, Clone)]
pub struct Evaluator {
    env: Rc<RefCell<Env>>,
}

impl Evaluator {
    pub fn new(env: Rc<RefCell<Env>>) -> Self {
        return Evaluator { env };
    }

    fn error(msg: String) -> Object {
        Object::Error(msg)
    }
    fn is_error(obj: &Object) -> bool {
        match obj {
            Object::Error(_) => true,
            _ => false,
        }
    }
    fn is_truthy(obj: Object) -> bool {
        match obj {
            Object::NULL | Object::Bool(false) => false,
            _ => true,
        }
    }
    fn eval_not_op_expr(&mut self, right: Object) -> Object {
        match right {
            Object::Bool(true) => Object::Bool(false),
            Object::Bool(false) => Object::Bool(true),
            Object::NULL => Object::Bool(true),
            _ => Object::Bool(false),
        }
    }
    fn eval_minus_prefix_op_expr(&mut self, right: Object) -> Object {
        match right {
            Object::Int(value) => Object::Int(-value),
            _ => Self::error(format!("unknown operator: -{:?}", right)),
        }
    }

    fn eval_plus_prefix_op_expr(&mut self, right: Object) -> Object {
        match right {
            Object::Int(value) => Object::Int(value),
            _ => Self::error(format!("unknown operator: {:?}", right)),
        }
    }

    fn eval_prefix_expr(&mut self, prefix: Prefix, right: Object) -> Object {
        return match prefix {
            Prefix::Not => self.eval_not_op_expr(right),
            Prefix::PrefixMinus => self.eval_minus_prefix_op_expr(right),
            Prefix::PrefixPlus => self.eval_plus_prefix_op_expr(right),
        };
    }
    fn eval_infix_int_expr(&mut self, infix: Infix, left: i64, right: i64) -> Object {
        match infix {
            Infix::Plus => Object::Int(left + right),
            Infix::Minus => Object::Int(left - right),
            Infix::Multiply => Object::Int(left * right),
            Infix::Divide => Object::Int(left / right),
            Infix::LessThan => Object::Bool(left < right),
            Infix::GreaterThan => Object::Bool(left > right),
            Infix::Equal => Object::Bool(left == right),
            Infix::NotEqual => Object::Bool(left != right),
        }
    }
    fn eval_infix_bool_expr(&mut self, infix: Infix, left: bool, right: bool) -> Object {
        match infix {
            Infix::Equal => Object::Bool(left == right),
            Infix::NotEqual => Object::Bool(left != right),
            _ => Self::error(format!("type mismatch: {:?} {:?} {:?}", left, infix, right)),
        }
    }
    fn eval_infix_string_expr(&mut self, infix: Infix, left: String, right: String) -> Object {
        match infix {
            Infix::Plus => Object::String(format!("{}{}", left, right)),
            _ => Object::Error(String::from(format!(
                "unknown operator: {:?} {:?} {:?}",
                left, infix, right
            ))),
        }
    }
    fn eval_infix_expr(&mut self, infix: Infix, left: Object, right: Object) -> Object {
        match left {
            Object::Int(left_value) => {
                if let Object::Int(right_value) = right {
                    self.eval_infix_int_expr(infix, left_value, right_value)
                } else {
                    Self::error(format!("type mismatch: {:?} {:?} {:?}", left, infix, right))
                }
            }
            Object::Bool(left_value) => {
                if let Object::Bool(right_value) = right {
                    self.eval_infix_bool_expr(infix, left_value, right_value)
                } else {
                    Self::error(format!("type mismatch: {:?} {:?} {:?}", left, infix, right))
                }
            }
            Object::String(left_value) => {
                if let Object::String(right_value) = right {
                    self.eval_infix_string_expr(infix, left_value, right_value)
                } else {
                    Self::error(format!(
                        "type mismatch: {:?} {:?} {:?}",
                        left_value, infix, right
                    ))
                }
            }
            _ => Self::error(format!(
                "unknown operator: {:?} {:?} {:?}",
                left, infix, right
            )),
        }
    }
    fn eval_if_expr(
        &mut self,
        cond: Expr,
        consequence: Program,
        alternative: Option<Program>,
    ) -> Option<Object> {
        let cond = match self.eval_expr(cond) {
            Some(cond) => cond,
            None => return None,
        };

        if Self::is_truthy(cond) {
            self.eval_block_stmt(consequence)
        } else if let Some(alt) = alternative {
            self.eval_block_stmt(alt)
        } else {
            None
        }
    }
    fn eval_lit(&self, lit: Literal) -> Object {
        return match lit {
            Literal::IntLiteral(int) => Object::Int(int),
            Literal::BoolLiteral(bool) => Object::Bool(bool),
            Literal::StringLiteral(string) => Object::String(string),
            _ => Object::NULL,
        };
    }
    fn eval_ident(&mut self, ident: Ident) -> Object {
        let Ident(name) = ident;

        match self.env.borrow_mut().get(name.clone()) {
            Some(value) => value,
            None => Object::Error(String::from(format!("identifier not found: {}", name))),
        }
    }
    fn eval_call_expr(&mut self, func: Box<Expr>, args: Vec<Expr>) -> Object {
        let args = args
            .iter()
            .map(|e| self.eval_expr(e.clone()).unwrap_or(Object::NULL))
            .collect::<Vec<_>>();

        let (params, body, env) = match self.eval_expr(*func) {
            Some(Object::Func(params, body, env)) => (params, body, env),
            Some(Object::Builtin(expect_param_num, f)) => {
                if expect_param_num < 0 || expect_param_num == args.len() as i32 {
                    return f(args);
                } else {
                    return Self::error(format!(
                        "wrong number of arguments. got={}, want={}",
                        args.len(),
                        expect_param_num,
                    ));
                }
            }
            Some(o) => return Self::error(format!("{:?} is not valid function", o)),
            None => return Object::NULL,
        };

        if params.len() != args.len() {
            return Self::error(format!(
                "wrong number of arguments: {} expected but {} given",
                params.len(),
                args.len()
            ));
        }

        let current_env = Rc::clone(&self.env);
        let mut scoped_env = Env::new_with_outer(Rc::clone(&env));
        let list = params.iter().zip(args.iter());
        for (_, (ident, o)) in list.enumerate() {
            let Ident(name) = ident.clone();
            scoped_env.set(name, o);
        }

        self.env = Rc::new(RefCell::new(scoped_env));

        let object = self.eval_block_stmt(body);

        self.env = current_env;

        match object {
            Some(o) => o,
            None => Object::NULL,
        }
    }
    fn eval_index_expr(&mut self, left: Object, index: Object) -> Object {
        match left {
            Object::Array(ref array) => {
                if let Object::Int(i) = index {
                    self.eval_array_index_expr(array.clone(), i)
                } else {
                    Self::error(format!("index operator not supported: {:?}", left))
                }
            }
            // Object::Hash(ref hash) => match index {
            //     Object::Int(_) | Object::Bool(_) | Object::String(_) => match hash.get(&index) {
            //         Some(o) => o.clone(),
            //         None => Object::NULL,
            //     },
            //     Object::Error(_) => index,
            //     _ => Self::error(format!("unusable as hash key: {:?}", index)),
            // },
            _ => Self::error(format!("uknown operator: {:?} {:?}", left, index)),
        }
    }
    fn eval_array_index_expr(&mut self, array: Vec<Object>, index: i64) -> Object {
        let max = array.len() as i64;

        if index < 0 || index > max {
            return Object::NULL;
        }

        match array.get(index as usize) {
            Some(o) => o.clone(),
            None => Object::NULL,
        }
    }
    fn eval_array_expr(&mut self, objects: Vec<Expr>) -> Object {
        Object::Array(
            objects
                .iter()
                .map(|e| self.eval_expr(e.clone()).unwrap_or(Object::NULL))
                .collect::<Vec<_>>(),
        )
    }

    fn eval_expr(&mut self, expr: Expr) -> Option<Object> {
        return match expr {
            Expr::LitExpr(lit) => Some(self.eval_lit(lit)),
            Expr::IdentExpr(ident) => Some(self.eval_ident(ident)),
            Expr::PrefixExpr(prefix, right_expr) => {
                if let Some(right) = self.eval_expr(*right_expr) {
                    Some(self.eval_prefix_expr(prefix, right))
                } else {
                    None
                }
            }
            Expr::ArrayExpr(list) => Some(self.eval_array_expr(list)),
            Expr::InfixExpr(infix, left_expr, right_expr) => {
                let left = self.eval_expr(*left_expr);
                let right = self.eval_expr(*right_expr);
                if left.is_some() && right.is_some() {
                    Some(self.eval_infix_expr(infix, left.unwrap(), right.unwrap()))
                } else {
                    None
                }
            }
            Expr::IfExpr {
                cond,
                consequence,
                alternative,
            } => self.eval_if_expr(*cond, consequence, alternative),
            Expr::FnExpr { params, body } => Some(Object::Func(params, body, Rc::clone(&self.env))),
            Expr::CallExpr {
                function,
                arguments,
            } => Some(self.eval_call_expr(function, arguments)),
            Expr::IndexExpr { array, index } => {
                let array = self.eval_expr(*array);
                let index = self.eval_expr(*index);
                if array.is_some() && index.is_some() {
                    Some(self.eval_index_expr(array.unwrap(), index.unwrap()))
                } else {
                    None
                }
            }
            _ => None,
        };
    }

    fn eval_block_stmt(&mut self, stmts: Program) -> Option<Object> {
        let mut result = None;

        for stmt in stmts {
            match self.eval_stmt(stmt) {
                Some(Object::Error(msg)) => return Some(Object::Error(msg)),
                Some(Object::ReturnValue(value)) => return Some(Object::ReturnValue(value)),
                obj => result = obj,
            }
        }

        return result;
    }
    fn eval_stmt(&mut self, stmt: Stmt) -> Option<Object> {
        return match stmt {
            Stmt::ExprStmt(expr) => self.eval_expr(expr),
            Stmt::ReturnStmt(expr) => {
                let value = match self.eval_expr(expr) {
                    Some(value) => value,
                    None => return None,
                };
                if Self::is_error(&value) {
                    Some(value)
                } else {
                    Some(Object::ReturnValue(Box::new(value)))
                }
            }
            Stmt::LetStmt(ident, expr) => {
                let value = match self.eval_expr(expr) {
                    Some(value) => value,
                    None => return None,
                };
                if Self::is_error(&value) {
                    return Some(value);
                } else {
                    let Ident(name) = ident;
                    self.env.borrow_mut().set(name, &value);
                    return None;
                }
            }
        };
    }
    pub fn eval(&mut self, stmts: Program) -> Option<Object> {
        let mut result = None;
        for stmt in stmts {
            match self.eval_stmt(stmt) {
                Some(Object::Error(msg)) => return Some(Object::Error(msg)),
                Some(Object::ReturnValue(value)) => return Some(*value),
                obj => result = obj,
            }
        }
        return result;
    }
}

#[cfg(test)]
mod test {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        parser::{Expr, Ident, Infix, Literal, Parser, Stmt},
        token::Lexer,
    };

    use super::{builtins::new_builtins, env::Env, Evaluator, Object};
    use pretty_assertions::assert_eq;

    fn test(input: &str) -> Option<Object> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let env = Rc::new(RefCell::new(Env::from(new_builtins())));
        let mut eval = Evaluator::new(env);
        let output = eval.eval(program);
        return output;
    }

    #[test]
    fn test_eval_int() {
        let expected = vec![
            ("5", Some(Object::Int(5))),
            ("10", Some(Object::Int(10))),
            ("-5", Some(Object::Int(-5))),
            ("-10", Some(Object::Int(-10))),
            ("5+5+5+5-10", Some(Object::Int(10))),
            ("2*2*2*2*2", Some(Object::Int(32))),
            ("-50+100+-50", Some(Object::Int(0))),
            ("5*2+10", Some(Object::Int(20))),
            ("5+2*10", Some(Object::Int(25))),
            ("20+2*-10", Some(Object::Int(0))),
            ("2*(5+10)", Some(Object::Int(30))),
            ("3*3*3+10", Some(Object::Int(37))),
            ("(5+10*2+15/3)*2+-10", Some(Object::Int(50))),
        ];
        for (input, value) in expected {
            assert_eq!(test(input), value);
        }
    }
    #[test]
    fn test_eval_bool() {
        let expected = vec![
            ("true", Some(Object::Bool(true))),
            ("false", Some(Object::Bool(false))),
        ];
        for (input, value) in expected {
            assert_eq!(test(input), value);
        }
    }
    #[test]
    fn test_eval_string() {
        let expected = vec![
            (
                "\"hello world\"",
                Some(Object::String("hello world".to_owned())),
            ),
            (
                "\"hello\" + \" \" + \"world\"",
                Some(Object::String("hello world".to_owned())),
            ),
        ];
        for (input, value) in expected {
            assert_eq!(test(input), value);
        }
    }
    #[test]
    fn test_eval_prefix() {
        let expected = vec![
            ("!true", Some(Object::Bool(false))),
            ("!false", Some(Object::Bool(true))),
            ("!5", Some(Object::Bool(false))),
            ("!!true", Some(Object::Bool(true))),
            ("!!false", Some(Object::Bool(false))),
            ("!!5", Some(Object::Bool(true))),
        ];
        for (input, value) in expected {
            assert_eq!(test(input), value);
        }
    }
    #[test]
    fn test_eval_infix() {
        let expected = vec![
            ("true == true", Some(Object::Bool(true))),
            ("false == false", Some(Object::Bool(true))),
            ("true == false", Some(Object::Bool(false))),
            ("true != false", Some(Object::Bool(true))),
            ("false != true", Some(Object::Bool(true))),
            ("(1 <2) == true", Some(Object::Bool(true))),
            ("(1 <2) == false", Some(Object::Bool(false))),
            ("(1 > 2) == true", Some(Object::Bool(false))),
            ("(1 > 2) == false", Some(Object::Bool(true))),
        ];
        for (input, value) in expected {
            assert_eq!(test(input), value);
        }
    }
    #[test]
    fn test_eval_IfElse() {
        let expected = vec![
            ("if(true){10}", Some(Object::Int(10))),
            ("if(false){10}", None),
            ("if(1){10}", Some(Object::Int(10))),
            ("if(1<2){10}", Some(Object::Int(10))),
            ("if(1>2){10}", None),
            ("if(1>2){10}else{20}", Some(Object::Int(20))),
            ("if(1<2){10}else{20}", Some(Object::Int(10))),
        ];
        for (input, value) in expected {
            assert_eq!(test(input), value);
        }
    }
    #[test]
    fn test_eval_return() {
        let expected = vec![
            ("return 10;", Some(Object::Int(10))),
            ("return 10; 9;", Some(Object::Int(10))),
            ("return 2*5; 9;", Some(Object::Int(10))),
            ("9; return 2*5; 9;,", Some(Object::Int(10))),
            (
                "if (10 > 1) {
            if (10 > 1) {
            return 10;
            }
            return 1;
            }",
                Some(Object::Int(10)),
            ),
        ];
        for (input, value) in expected {
            assert_eq!(test(input), value);
        }
    }
    #[test]
    fn test_eval_let() {
        let expected = vec![
            ("let a = 5; a;", Some(Object::Int(5))),
            ("let a =5*5; a;", Some(Object::Int(25))),
            ("let a = 5; let b = a; b;", Some(Object::Int(5))),
            (
                "let a = 5; let b = a; let c = a +b +5; c;",
                Some(Object::Int(15)),
            ),
        ];
        for (input, value) in expected {
            assert_eq!(test(input), value);
        }
    }
    #[test]
    fn test_eval_function() {
        let expected = vec![(
            "fn(x) { x + 2; };",
            Some(Object::Func(
                vec![Ident(String::from("x"))],
                vec![Stmt::ExprStmt(Expr::InfixExpr(
                    Infix::Plus,
                    Box::new(Expr::IdentExpr(Ident(String::from("x")))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(2))),
                ))],
                Rc::new(RefCell::new(Env::from(new_builtins()))),
            )),
        )];
        for (input, value) in expected {
            assert_eq!(test(input), value);
        }
    }
    #[test]
    fn test_eval_call() {
        let expected = vec![
            (
                "let identity = fn(x) { x; }; identity(5);",
                Some(Object::Int(5)),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Some(Object::Int(5)),
            ),
            (
                "let double = fn(x) { x*2; }; double(5);",
                Some(Object::Int(10)),
            ),
            (
                "let add = fn(x, y) {  x + y; }; add(5, 5);",
                Some(Object::Int(10)),
            ),
            (
                "let add = fn(x, y) {  x + y; }; add(5+5, add(5,5));",
                Some(Object::Int(20)),
            ),
            (
                "let newAdder = fn(x) {
                fn(y) { x + y };
                };
                let addTwo = newAdder(2);
                addTwo(2);",
                Some(Object::Int(4)),
            ),
        ];
        for (input, value) in expected {
            assert_eq!(test(input), value);
        }
    }
    #[test]
    fn test_eval_builtins() {
        let expected = vec![
            ("len(\"\")", Some(Object::Int(0))),
            ("len(\"four\")", Some(Object::Int(4))),
            ("len(\"hello world\")", Some(Object::Int(11))),
             ("first([1, 2, 3])", Some(Object::Int(1))),
            ("first([])", Some(Object::NULL)),
            ("last([1, 2, 3])", Some(Object::Int(3))),
            ("last([])", Some(Object::NULL)),
            (
                "rest([1, 2, 3, 4])",
                Some(Object::Array(vec![
                    Object::Int(2),
                    Object::Int(3),
                    Object::Int(4),
                ])),
            ),
            (
                "rest([2, 3, 4])",
                Some(Object::Array(vec![Object::Int(3), Object::Int(4)])),
            ),
            ("rest([4])", Some(Object::Array(vec![]))),
            ("rest([])", Some(Object::NULL)),
            // push
            (
                "push([1, 2, 3], 4)",
                Some(Object::Array(vec![
                    Object::Int(1),
                    Object::Int(2),
                    Object::Int(3),
                    Object::Int(4),
                ])),
            ),
            ("push([], 1)", Some(Object::Array(vec![Object::Int(1)]))),
            (
                "let a = [1]; push(a, 2); a",
                Some(Object::Array(vec![Object::Int(1)])),
            ),
        ];
        for (input, value) in expected {
            assert_eq!(test(input), value);
        }
    }
    #[test]
    fn test_eval_array() {
        let expected = vec![(
            "[1, 2 * 2, 3 + 3]",
            Some(Object::Array(vec![
                Object::Int(1),
                Object::Int(4),
                Object::Int(6),
            ])),
        )];
        for (input, value) in expected {
            assert_eq!(test(input), value);
        }
    }
    #[test]
    fn test_eval_index() {
        let expected = vec![
            ("[1, 2, 3][0]", Some(Object::Int(1))),
            ("[1, 2, 3][1]", Some(Object::Int(2))),
            ("let i = 0; [1][i]", Some(Object::Int(1))),
            ("[1, 2, 3][1 + 1];", Some(Object::Int(3))),
            ("let myArray = [1, 2, 3]; myArray[2];", Some(Object::Int(3))),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Some(Object::Int(6)),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];",
                Some(Object::Int(2)),
            ),
            ("[1, 2, 3][3]", Some(Object::NULL)),
            ("[1, 2, 3][-1]", Some(Object::NULL)),
        ];
        for (input, value) in expected {
            assert_eq!(test(input), value);
        }
    }
}
