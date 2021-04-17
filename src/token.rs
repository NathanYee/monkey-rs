#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Illegal(char),
    Eof,

    // Identifiers + literals
    Ident(String),
    Int(i32),
    Str(String),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,

    Eq,
    NotEq,

    // Delimiters
    Comma,
    Semicolon,
    Colon,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

use std::fmt::Display;

use Token::*;

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Illegal(c) => f.write_str(&c.to_string()),
            Eof => f.write_str("\0"),
            Ident(s) => f.write_str(s),
            Int(i) => f.write_str(&i.to_string()),
            Str(s) => write!(f, "\"{}\"", s),
            Assign => f.write_str("="),
            Plus => f.write_str("+"),
            Minus => f.write_str("-"),
            Bang => f.write_str("!"),
            Asterisk => f.write_str("*"),
            Slash => f.write_str("/"),
            Lt => f.write_str("<"),
            Gt => f.write_str(">"),
            Eq => f.write_str("=="),
            NotEq => f.write_str("!="),
            Comma => f.write_str(","),
            Semicolon => f.write_str(";"),
            Colon => f.write_str(":"),
            LParen => f.write_str("("),
            RParen => f.write_str(")"),
            LBrace => f.write_str("{"),
            RBrace => f.write_str("}"),
            LBracket => f.write_str("["),
            RBracket => f.write_str("]"),
            Function => f.write_str("fn"),
            Let => f.write_str("let"),
            True => f.write_str("true"),
            False => f.write_str("false"),
            If => f.write_str("if"),
            Else => f.write_str("else"),
            Return => f.write_str("return"),
        }
    }
}
