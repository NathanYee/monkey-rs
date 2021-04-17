use std::fmt::Display;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::Token;

pub struct Program {
    pub statements: Vec<Stmt>,
}

impl Program {
    pub fn new(input: &str) -> Self {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();
        program
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        let s: String = self.statements.iter().map(|x| x.to_string()).collect();
        f.write_str(&s)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stmt {
    Let(String, Expr),
    Return(Expr),
    Expression(Expr),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Stmt::Let(s, e) => write!(f, "let {} = {};", s, e),
            Stmt::Return(e) => write!(f, "return {};", e),
            Stmt::Expression(e) => write!(f, "{}", e),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Identifier(String),
    Integer(i32),
    Str(String),
    Boolean(bool),
    Array(Vec<Expr>),
    Hash(Vec<(Expr, Expr)>),
    Index(Box<Expr>, Box<Expr>),
    Prefix(Token, Box<Expr>),
    Infix(Box<Expr>, Token, Box<Expr>), // left expression, operator, right expression
    If(Box<Expr>, BlockStmt, Option<BlockStmt>), // condition, consequence, alternative
    Function(Vec<Expr>, BlockStmt),     // parameters, body
    Call(Box<Expr>, Vec<Expr>),         // function, arguments
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Expr::Identifier(s) => write!(f, "{}", s),
            Expr::Integer(i) => write!(f, "{}", i),
            Expr::Str(s) => write!(f, "{}", s),
            Expr::Boolean(b) => write!(f, "{}", b),
            Expr::Array(items) => write!(f, "[{}]", csv_str(items)),
            Expr::Hash(map) => {
                let pairs: Vec<String> = map.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
                let pairs = csv_str(&pairs);
                write!(f, "{{{}}}", pairs)
            }
            Expr::Index(left, idx) => write!(f, "({}[{}])", left, idx),
            Expr::Prefix(t, e) => write!(f, "({}{})", t, e),
            Expr::Infix(left, op, right) => {
                write!(f, "({} {} {})", left, op, right)
            }
            Expr::If(condition, consequence, alternative) => match alternative {
                Some(alt) => write!(f, "if {} {} else {}", condition, consequence, alt),
                None => write!(f, "if {} {}", condition, consequence),
            },
            Expr::Function(parameters, body) => {
                write!(f, "fn({}) {}", csv_str(parameters), body)
            }
            Expr::Call(func, args) => write!(f, "{}({})", func, csv_str(args)),
        }
    }
}

pub fn csv_str<T: Display>(arr: &[T]) -> String {
    arr.iter()
        .map(|e| e.to_string())
        .collect::<Vec<String>>()
        .join(", ")
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BlockStmt {
    pub statements: Vec<Stmt>,
}

impl BlockStmt {
    pub fn new() -> Self {
        BlockStmt { statements: vec![] }
    }
}

impl Display for BlockStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        let s: String = self.statements.iter().map(|x| x.to_string()).collect();
        f.write_str(&s)
    }
}
