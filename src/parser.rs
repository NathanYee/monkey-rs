use crate::ast::{BlockStmt, Expr, Program, Stmt};
use crate::lexer::Lexer;
use crate::token::Token;

use std::mem;

type Result<T> = std::result::Result<T, ParserError>;

#[derive(Debug)]
enum ParserError {
    // ExpectedLet(Token),
    ExectedIdent(Token),
    ExpectedAssign(Token),
    ExpectedLParen(Token),
    ExpectedRParen(Token),
    ExpectedLBrace(Token),
    ExpectedRBracket(Token),
    ExpectedColon(Token),
    ExpectedComma(Token),
    ExpectedToken { expected: Token, got: Token },
    ExpectedRBrace(Token),
    ExpectedPrefixToken(Token),
    // NoFunctionForToken(Token),
}

#[derive(Debug, PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,      // ==, !=
    LessGreater, // >, <
    Sum,         // +, -
    Product,     // *, /
    Prefix,      // -x, !true
    Call,        // myFunction(x)
    Index,       // myArray[2]
}

impl Precedence {
    fn from_token(t: &Token) -> Precedence {
        // could be a hashmap which could potentially be faster
        match t {
            Token::Eq | Token::NotEq => Precedence::Equals,
            Token::Lt | Token::Gt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Asterisk | Token::Slash => Precedence::Product,
            Token::LParen => Precedence::Call,
            Token::LBracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(l: Lexer) -> Self {
        let mut p = Self {
            lexer: l,
            cur_token: Token::Eof,
            peek_token: Token::Eof,
            errors: vec![],
        };

        p.next_token();
        p.next_token();

        p
    }

    fn next_token(&mut self) {
        self.cur_token = mem::replace(&mut self.peek_token, self.lexer.next_token());
    }

    fn expect_peek<F>(&mut self, token: Token, parser_error: F) -> Result<()>
    where
        F: Fn(Token) -> ParserError,
    {
        let peek = self.peek_token.clone();
        if peek == token {
            self.next_token();
            Ok(())
        } else {
            Err(parser_error(peek))
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.cur_token != Token::Eof {
            match self.parse_statement() {
                Ok(stmt) => program.statements.push(stmt),
                Err(error) => self.errors.push(error),
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Result<Stmt> {
        match &self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Stmt> {
        let stmt = Stmt::Expression(self.parse_expression(Precedence::Lowest)?);

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(stmt)
    }

    fn parse_expression(&mut self, prec: Precedence) -> Result<Expr> {
        let mut left_exp = self.parse_prefix()?;

        while self.peek_token != Token::Semicolon && prec < Precedence::from_token(&self.peek_token)
        {
            left_exp = match Precedence::from_token(&self.peek_token) {
                Precedence::Equals
                | Precedence::LessGreater
                | Precedence::Sum
                | Precedence::Product => {
                    self.next_token();
                    self.parse_infix_expression(Box::new(left_exp))?
                }
                Precedence::Call => {
                    self.next_token();
                    self.parse_call_expression(Box::new(left_exp))?
                }
                Precedence::Index => {
                    self.next_token();
                    self.parse_index_expression(Box::new(left_exp))?
                }
                _ => break,
            };
        }

        Ok(left_exp)
    }

    fn parse_prefix(&mut self) -> Result<Expr> {
        match &self.cur_token {
            Token::Ident(s) => Ok(Expr::Identifier(s.clone())),
            Token::Int(i) => Ok(Expr::Integer(*i)),
            Token::Str(s) => Ok(Expr::Str(s.clone())),
            Token::True => Ok(Expr::Boolean(true)),
            Token::False => Ok(Expr::Boolean(false)),
            Token::LBracket => self.parse_array_literal(),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            Token::LParen => self.parse_grouped_expression(),
            Token::LBrace => self.parse_map_literal(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_literal(),
            t => Err(ParserError::ExpectedPrefixToken(t.clone())),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expr> {
        let cur_token = self.cur_token.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(Expr::Prefix(cur_token, Box::new(right)))
    }

    fn parse_infix_expression(&mut self, left: Box<Expr>) -> Result<Expr> {
        let token = self.cur_token.clone();

        let right = {
            let prec = Precedence::from_token(&token);
            self.next_token();
            let expr = self.parse_expression(prec)?;
            Box::new(expr)
        };

        Ok(Expr::Infix(left, token, right))
    }

    fn parse_call_expression(&mut self, left: Box<Expr>) -> Result<Expr> {
        let args = self.parse_expression_list(Token::RParen)?;
        Ok(Expr::Call(left, args))
    }

    fn parse_function_literal(&mut self) -> Result<Expr> {
        self.expect_peek(Token::LParen, ParserError::ExpectedLParen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(Token::LBrace, ParserError::ExpectedLBrace)?;

        let body = self.parse_block_statement()?;

        Ok(Expr::Function(parameters, body))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Expr>> {
        let mut parameters = vec![];

        self.next_token();
        if self.cur_token == Token::RParen {
            return Ok(parameters);
        }

        let ident = match &self.cur_token {
            Token::Ident(s) => Expr::Identifier(s.clone()),
            t => return Err(ParserError::ExectedIdent(t.clone())),
        };
        parameters.push(ident);

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            let ident = match &self.cur_token {
                Token::Ident(s) => Expr::Identifier(s.clone()),
                t => return Err(ParserError::ExectedIdent(t.clone())),
            };
            parameters.push(ident);
        }

        self.expect_peek(Token::RParen, ParserError::ExpectedRParen)?;

        Ok(parameters)
    }

    fn parse_array_literal(&mut self) -> Result<Expr> {
        let items = self.parse_expression_list(Token::RBracket)?;
        Ok(Expr::Array(items))
    }

    fn parse_index_expression(&mut self, left: Box<Expr>) -> Result<Expr> {
        // cur_token: LBracket
        self.next_token();
        let index = Box::new(self.parse_expression(Precedence::Lowest)?);

        self.expect_peek(Token::RBracket, ParserError::ExpectedRBracket)?;

        Ok(Expr::Index(left, index))
    }

    fn parse_expression_list(&mut self, end: Token) -> Result<Vec<Expr>> {
        let mut items = vec![];

        self.next_token();
        if self.cur_token != end {
            items.push(self.parse_expression(Precedence::Lowest)?);

            while self.peek_token == Token::Comma {
                self.next_token();
                self.next_token();
                items.push(self.parse_expression(Precedence::Lowest)?);
            }

            self.expect_peek(end.clone(), |got| ParserError::ExpectedToken {
                expected: end.clone(),
                got: got,
            })?;
        }

        Ok(items)
    }

    fn parse_grouped_expression(&mut self) -> Result<Expr> {
        self.next_token();
        let exp = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::RParen, ParserError::ExpectedRParen)?;

        Ok(exp)
    }

    fn parse_map_literal(&mut self) -> Result<Expr> {
        // cur_token: LBrace, peek_token: beginning of expression or RBRace
        let mut pairs = vec![];

        while self.peek_token != Token::RBrace {
            self.next_token(); // move cur_token to beginning of expression
            let key = self.parse_expression(Precedence::Lowest)?;

            self.expect_peek(Token::Colon, ParserError::ExpectedColon)?;

            // cur_token: colon, peek_token: beginning of expression
            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;
            pairs.push((key, value));

            if self.peek_token == Token::RBrace {
                break;
            }

            self.expect_peek(Token::Comma, ParserError::ExpectedComma)?;
        }

        self.expect_peek(Token::RBrace, ParserError::ExpectedRBrace)?;

        Ok(Expr::Hash(pairs))
    }

    fn parse_if_expression(&mut self) -> Result<Expr> {
        // cur_token: If, peek_token: LParen
        self.expect_peek(Token::LParen, ParserError::ExpectedLParen)?;
        self.next_token(); // jump over LParen to get expression

        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);

        self.expect_peek(Token::RParen, ParserError::ExpectedRParen)?;
        self.expect_peek(Token::LBrace, ParserError::ExpectedLBrace)?;

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token == Token::Else {
            self.next_token();
            self.expect_peek(Token::LBrace, ParserError::ExpectedLBrace)?;
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(Expr::If(condition, consequence, alternative))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStmt> {
        let mut block = BlockStmt::new();
        self.next_token();

        while self.cur_token != Token::RBrace && self.cur_token != Token::Eof {
            let stmt = self.parse_statement()?;
            block.statements.push(stmt);
            self.next_token();
        }

        Ok(block)
    }

    fn parse_let_statement(&mut self) -> Result<Stmt> {
        // cur_token: Let, peek_token: Ident
        let identifier: String;
        if let Token::Ident(ident) = self.peek_token.clone() {
            identifier = ident;
            self.next_token();
        } else {
            return Err(ParserError::ExectedIdent(self.peek_token.clone()));
        }

        // cur_token: Ident, peek_token: Assign
        self.expect_peek(Token::Assign, ParserError::ExpectedAssign)?;

        // cur_token: Assign, peek_token: beginning of expression
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Stmt::Let(identifier, value))
    }

    fn parse_return_statement(&mut self) -> Result<Stmt> {
        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Stmt::Return(return_value))
    }

    pub fn check_parser_errors(&self) {
        if self.errors.is_empty() {
            return;
        }

        eprintln!("Parser has {} errors:", self.errors.len());
        for (i, error) in self.errors.iter().enumerate() {
            eprintln!("\t{}. {:?}", i, error);
        }
    }
}

#[cfg(test)]
mod test_precidence {
    use super::*;

    #[test]
    fn test_ord() {
        assert!(Precedence::Lowest < Precedence::Equals);
        assert!(Precedence::Equals > Precedence::Lowest);
    }
}

#[cfg(test)]
mod test_parser_statements {
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = r"let x = 5;
                           let y = true;
                           let z = y;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected = vec![
            Stmt::Let("x".to_string(), Expr::Integer(5)),
            Stmt::Let("y".to_string(), Expr::Boolean(true)),
            Stmt::Let("z".to_string(), Expr::Identifier("y".to_string())),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_return_statements() {
        let input = r"return 5;
                           return 2 + 3;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected = vec![
            Stmt::Return(Expr::Integer(5)),
            Stmt::Return(Expr::Infix(
                Box::new(Expr::Integer(2)),
                Token::Plus,
                Box::new(Expr::Integer(3)),
            )),
        ];

        assert_eq!(program.statements, expected);
    }
}

#[cfg(test)]
mod test_parser_expressions {
    use std::vec;

    use super::*;

    #[test]
    fn test_identifier() {
        let input = r"x";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected = vec![Stmt::Expression(Expr::Identifier("x".to_string()))];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_integer() {
        let input = r"5";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::Integer(5))];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_string() {
        let input = "\"hello world\"; \"hello world 2\"";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected: Vec<Stmt> = vec![
            Stmt::Expression(Expr::Str("hello world".to_string())),
            Stmt::Expression(Expr::Str("hello world 2".to_string())),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_boolean() {
        let input = r"true;
                           false;
                           let foobar = true;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected: Vec<Stmt> = vec![
            Stmt::Expression(Expr::Boolean(true)),
            Stmt::Expression(Expr::Boolean(false)),
            Stmt::Let("foobar".to_string(), Expr::Boolean(true)),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_array() {
        let input = "[];
                          [1, 2];
                          [3, 4+5];
                          [6, 7, \"hello\"];";

        let program = Program::new(input);

        let expected: Vec<Stmt> = vec![
            Stmt::Expression(Expr::Array(vec![])),
            Stmt::Expression(Expr::Array(vec![Expr::Integer(1), Expr::Integer(2)])),
            Stmt::Expression(Expr::Array(vec![
                Expr::Integer(3),
                Expr::Infix(
                    Box::new(Expr::Integer(4)),
                    Token::Plus,
                    Box::new(Expr::Integer(5)),
                ),
            ])),
            Stmt::Expression(Expr::Array(vec![
                Expr::Integer(6),
                Expr::Integer(7),
                Expr::Str("hello".to_string()),
            ])),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_index() {
        let input = "myArray[2]";

        let program = Program::new(input);

        let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::Index(
            Box::new(Expr::Identifier("myArray".to_string())),
            Box::new(Expr::Integer(2)),
        ))];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_map_empty() {
        let input = "{}";
        let program = Program::new(input);

        let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::Hash(vec![]))];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_map_strings() {
        let input = "{\"one\": 1, \"two\": 2, \"three\": 3};
                          {1: \"one\", 23: \"two\" + \"three\"}";

        let program = Program::new(input);

        let expected: Vec<Stmt> = vec![
            Stmt::Expression(Expr::Hash(vec![
                (Expr::Str("one".to_string()), Expr::Integer(1)),
                (Expr::Str("two".to_string()), Expr::Integer(2)),
                (Expr::Str("three".to_string()), Expr::Integer(3)),
            ])),
            Stmt::Expression(Expr::Hash(vec![
                (Expr::Integer(1), Expr::Str("one".to_string())),
                (
                    Expr::Integer(23),
                    Expr::Infix(
                        Box::new(Expr::Str("two".to_string())),
                        Token::Plus,
                        Box::new(Expr::Str("three".to_string())),
                    ),
                ),
            ])),
        ];
        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_prefix_expression() {
        let input = "!5;
                          -15;
                          !true;
                          !false;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected: Vec<Stmt> = vec![
            Stmt::Expression(Expr::Prefix(Token::Bang, Box::new(Expr::Integer(5)))),
            Stmt::Expression(Expr::Prefix(Token::Minus, Box::new(Expr::Integer(15)))),
            Stmt::Expression(Expr::Prefix(Token::Bang, Box::new(Expr::Boolean(true)))),
            Stmt::Expression(Expr::Prefix(Token::Bang, Box::new(Expr::Boolean(false)))),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_infix_expressions() {
        let input = "5 + 5;
                          5 - 5;
                          5 * 5;
                          5 / 5;
                          5 > 5;
                          5 < 5;
                          5 == 5;
                          5 != 5;
                          true == true;
                          true != false;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected: Vec<Stmt> = vec![
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Plus,
                Box::new(Expr::Integer(5)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Minus,
                Box::new(Expr::Integer(5)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Asterisk,
                Box::new(Expr::Integer(5)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Slash,
                Box::new(Expr::Integer(5)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Gt,
                Box::new(Expr::Integer(5)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Lt,
                Box::new(Expr::Integer(5)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Eq,
                Box::new(Expr::Integer(5)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::NotEq,
                Box::new(Expr::Integer(5)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Boolean(true)),
                Token::Eq,
                Box::new(Expr::Boolean(true)),
            )),
            Stmt::Expression(Expr::Infix(
                Box::new(Expr::Boolean(true)),
                Token::NotEq,
                Box::new(Expr::Boolean(false)),
            )),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests: Vec<(&str, &str)> = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("(1 + 1)", "(1 + 1)"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            parser.check_parser_errors();
            assert_eq!(program.to_string(), expected.to_string());
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::If(
            Box::new(Expr::Infix(
                Box::new(Expr::Identifier("x".to_string())),
                Token::Lt,
                Box::new(Expr::Identifier("y".to_string())),
            )),
            BlockStmt {
                statements: vec![Stmt::Expression(Expr::Identifier("x".to_string()))],
            },
            None,
        ))];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::If(
            Box::new(Expr::Infix(
                Box::new(Expr::Identifier("x".to_string())),
                Token::Lt,
                Box::new(Expr::Identifier("y".to_string())),
            )),
            BlockStmt {
                statements: vec![Stmt::Expression(Expr::Identifier("x".to_string()))],
            },
            Some(BlockStmt {
                statements: vec![Stmt::Expression(Expr::Identifier("y".to_string()))],
            }),
        ))];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::Function(
            vec![
                Expr::Identifier("x".to_string()),
                Expr::Identifier("y".to_string()),
            ],
            BlockStmt {
                statements: vec![Stmt::Expression(Expr::Infix(
                    Box::new(Expr::Identifier("x".to_string())),
                    Token::Plus,
                    Box::new(Expr::Identifier("y".to_string())),
                ))],
            },
        ))];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_function_parameter_parsing() {
        let input = "fn() {};
                          fn(x) {};
                          fn(x, y, z) {};";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected: Vec<Stmt> = vec![
            Stmt::Expression(Expr::Function(vec![], BlockStmt { statements: vec![] })),
            Stmt::Expression(Expr::Function(
                vec![Expr::Identifier("x".to_string())],
                BlockStmt { statements: vec![] },
            )),
            Stmt::Expression(Expr::Function(
                vec![
                    Expr::Identifier("x".to_string()),
                    Expr::Identifier("y".to_string()),
                    Expr::Identifier("z".to_string()),
                ],
                BlockStmt { statements: vec![] },
            )),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::Call(
            Box::new(Expr::Identifier("add".to_string())),
            vec![
                Expr::Integer(1),
                Expr::Infix(
                    Box::new(Expr::Integer(2)),
                    Token::Asterisk,
                    Box::new(Expr::Integer(3)),
                ),
                Expr::Infix(
                    Box::new(Expr::Integer(4)),
                    Token::Plus,
                    Box::new(Expr::Integer(5)),
                ),
            ],
        ))];

        assert_eq!(program.statements, expected);
    }
}
