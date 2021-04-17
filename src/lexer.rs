use crate::token::Token;

pub struct Lexer {
    input: Vec<char>,
    position: usize,      // current position in input (points to current char)
    read_position: usize, // current reading position in input (after current char)
    ch: char,             // current char under examination
}

trait IsLetter {
    fn is_letter(&self) -> bool;
}

impl IsLetter for char {
    fn is_letter(&self) -> bool {
        self.is_alphabetic() || *self == '_'
    }
}

impl Lexer {
    #[allow(dead_code)]
    pub fn new(input: String) -> Self {
        let mut l = Lexer {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        self.ch = match self.input.get(self.read_position) {
            Some(&char) => char,
            None => '\0',
        };
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> char {
        match self.input.get(self.read_position) {
            Some(&char) => char,
            None => '\0',
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '<' => Token::Lt,
            '>' => Token::Gt,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            ':' => Token::Colon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            '\0' => Token::Eof,
            c if c.is_letter() => {
                let s = self.read_identifier();
                return self.lookup_identifier(s);
            }
            c if c.is_digit(10) => {
                let i = self.read_number();
                return Token::Int(i);
            }
            c if c == '"' => {
                let s = self.read_string();
                Token::Str(s)
            }
            c => Token::Illegal(c),
        };
        self.read_char();
        tok
    }

    fn lookup_identifier(&self, s: String) -> Token {
        match s.as_str() {
            "fn" => Token::Function,
            "let" => Token::Let,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Ident(s),
        }
    }

    fn read_identifier(&mut self) -> String {
        let start = self.position;
        while self.ch.is_letter() {
            self.read_char();
        }
        self.input[start..self.position].iter().collect()
    }

    fn read_number(&mut self) -> i32 {
        let start = self.position;
        while self.ch.is_digit(10) {
            self.read_char();
        }
        self.input[start..self.position]
            .iter()
            .collect::<String>()
            .parse()
            .unwrap()
    }

    fn read_string(&mut self) -> String {
        let start = self.position + 1;
        loop {
            self.read_char();
            if self.ch == '"' || self.ch == '\0' {
                break;
            }
        }
        self.input[start..self.position].iter().collect()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.position <= self.input.len() {
            Some(self.next_token())
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test_lexer {
    use super::*;

    fn input_produces_tokens(input: &str, tokens: Vec<Token>) {
        let lexer = Lexer::new(input.to_string());

        for (tok, expected) in lexer.zip(tokens.iter()) {
            assert_eq!(*expected, tok);
        }
    }

    #[test]
    fn test_next_token_0() {
        let input = r"let five = 5;
                           let ten = 10;
                           
                           let add = fn(x, y) {
                               x + y; 
                           };";

        let tokens = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Eof,
        ];

        input_produces_tokens(input, tokens);
    }

    #[test]
    fn test_next_token_1() {
        let input = r"let result = add(five, ten);
                           !-/*5;
                           5 < 10 > 5;";

        let tokens = vec![
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
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
            Token::Eof,
        ];

        input_produces_tokens(input, tokens);
    }

    #[test]
    fn test_next_token_2() {
        let input = r"if (5 < 10) {
                               return true;
                           } else {
                               return false;
                           }";

        let tokens = vec![
            Token::If,
            Token::LParen,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Eof,
        ];

        input_produces_tokens(input, tokens);
    }

    #[test]
    fn test_next_token_3() {
        let input = r"10 == 10;
                           10 != 9;";

        let tokens = vec![
            Token::Int(10),
            Token::Eq,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::NotEq,
            Token::Int(9),
            Token::Semicolon,
            Token::Eof,
            Token::Eof,
        ];

        input_produces_tokens(input, tokens);
    }

    #[test]
    fn test_next_token_strs() {
        let input = "\"foobar\";
                          \"foo bar\";
                          \"\";
                          \"123\";";

        let tokens = vec![
            Token::Str("foobar".to_string()),
            Token::Semicolon,
            Token::Str("foo bar".to_string()),
            Token::Semicolon,
            Token::Str("".to_string()),
            Token::Semicolon,
            Token::Str("123".to_string()),
            Token::Semicolon,
            Token::Eof,
        ];

        input_produces_tokens(input, tokens);
    }

    #[test]
    fn test_next_token_arrays() {
        let input = "[1, 2];
                          [3, \"hi\"];";

        let tokens = vec![
            Token::LBracket,
            Token::Int(1),
            Token::Comma,
            Token::Int(2),
            Token::RBracket,
            Token::Semicolon,
            Token::LBracket,
            Token::Int(3),
            Token::Comma,
            Token::Str("hi".to_string()),
            Token::RBracket,
            Token::Semicolon,
            Token::Eof,
        ];

        input_produces_tokens(input, tokens);
    }

    #[test]
    fn test_next_token_hash() {
        let input = "{\"foo\": \"bar\"}";

        let tokens = vec![
            Token::LBrace,
            Token::Str("foo".to_string()),
            Token::Colon,
            Token::Str("bar".to_string()),
            Token::RBrace,
            Token::Eof,
        ];

        input_produces_tokens(input, tokens);
    }
}
