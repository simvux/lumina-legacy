mod token;
use super::{ast, Tracked};
use std::convert::TryFrom;
use std::iter::Peekable;
pub use token::{Capture, Header, Inlinable, Key, Operator, RawToken, Token};

pub struct Tokenizer<I: Iterator<Item = char>> {
    source_code: Peekable<I>,
    pub position: usize,

    // For the peek implementation
    pending: Vec<Tracked<RawToken>>,
}

const DEFAULT_STOPPERS: &[char] = &[' ', ',', '(', ')', '[', ']', '\n', '#', '{', '}', '\\'];
const SINGLES: &[char] = &['(', '[', '{', '\n', ')', ']', '}', ',', '\\', '#'];
// TODO: Maybe the proper way to handle annotations is to make < and > singles. Although that'd
// break operators somewhat... I think I'll just have to hack annotations in here

impl<I: Iterator<Item = char>> From<Peekable<I>> for Tokenizer<I> {
    fn from(source_code: Peekable<I>) -> Self {
        Self {
            source_code,
            pending: Vec::new(),
            position: 0,
        }
    }
}

impl<I: Iterator<Item = char>> Tokenizer<I> {
    pub fn skip_spaces_and_newlines(&mut self) {
        self.skip_until(|c| c != '\n' && c != ' ')
    }
    pub fn skip_until(&mut self, predicate: impl Fn(char) -> bool) {
        loop {
            let c = self.source_code.peek();
            match c {
                None => return,
                Some(c) => {
                    if predicate(*c) {
                        return;
                    }
                    self.walk();
                }
            }
        }
    }
    pub fn gather_to_recursive(&mut self, predicate: impl Fn(char) -> (bool, bool)) -> String {
        let mut buf = String::new();
        loop {
            let c = self.source_code.peek().copied();
            match c {
                None => return buf,
                Some(c) => {
                    let r = predicate(c);
                    if r.0 {
                        return buf;
                    } else if r.1 {
                        self.walk();
                        return self.gather_to_recursive(predicate);
                    } else {
                        buf.push(c);
                        self.walk();
                    }
                }
            }
        }
    }

    fn walk(&mut self) -> Option<char> {
        self.position += 1;
        self.source_code.next()
    }

    fn gather_to(&mut self, stoppers: &[char]) -> (char, String) {
        let mut buf = String::new();
        self.skip_until(|c| c != ' ');
        loop {
            let c = match self.source_code.peek() {
                None => return (0 as char, buf),
                Some(c) => c,
            };
            if buf.is_empty() && SINGLES.contains(&c) {
                buf.push(*c);
                return (self.walk().unwrap(), buf);
            }
            match c {
                '<' => {
                    // Hack to make annotations parse correctly
                    if let Some(last) = buf.chars().rev().nth(0) {
                        if ast::NAME_CHARS.contains(last) {
                            self.walk();
                            // It's an annotation to previous
                            let complete_anot = self.gather_to_recursive(|c| match c {
                                '<' => (false, true),
                                '>' => (true, false),
                                _ => (false, false),
                            });
                            buf.push('<');
                            buf.push_str(&complete_anot);
                            buf.push('>');
                            assert_eq!(self.source_code.peek(), Some(&'>'));
                            self.walk();
                            return ('>', buf);
                        }
                    }
                    if stoppers.contains(&c) {
                        return (*c, buf);
                    } else {
                        buf.push(self.walk().unwrap());
                    }
                }
                '/' => {
                    self.walk();
                    let after = self.source_code.peek();
                    match after {
                        Some('/') => {
                            self.walk();
                            self.single_line_comment();
                        }
                        Some('*') => {
                            self.walk();
                            self.multi_line_comment();
                        }
                        None => return (0 as char, buf),
                        _ => {
                            if stoppers.contains(&'/') {
                                return ('/', buf);
                            } else {
                                self.walk();
                                buf.push('/');
                            }
                        }
                    }
                }
                _ => {
                    if stoppers.contains(&c) {
                        return (*c, buf);
                    } else {
                        buf.push(self.walk().unwrap());
                    }
                }
            }
        }
    }

    fn single_line_comment(&mut self) {
        loop {
            let c = self.walk();
            if c == None || c == Some('\n') {
                return;
            }
        }
    }
    fn multi_line_comment(&mut self) {
        loop {
            let c = match self.walk() {
                None => panic!("ET: Unmatched /*"),
                Some(a) => a,
            };
            if c == '*' && self.source_code.peek() == Some(&'/') {
                self.walk();
                break;
            }
        }
    }
}

impl<I: Iterator<Item = char>> Iterator for Tokenizer<I> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(t) = self.pending.pop() {
            return Some(t);
        }
        let source = self.position;
        let (brk, chunk) = self.gather_to(DEFAULT_STOPPERS);
        if brk == 0 as char && chunk.is_empty() {
            return None;
        }
        match RawToken::try_from(chunk.as_str()) {
            Ok(rt) => {
                self.skip_until(|c| c != ' ');
                Some(Tracked::new(rt).set(source))
            }
            Err(_) => None,
        }
    }
}

impl<I: Iterator<Item = char>> TokenSource for Tokenizer<I> {
    fn peek(&mut self) -> Option<&Token> {
        let t = self.next()?;
        self.pending.push(t);
        self.pending.last()
        /*
        if self.pending.is_empty() {
        } else {
            self.pending.last()
        }
                */
    }
}

pub trait TokenSource: Iterator<Item = Token> {
    fn peek(&mut self) -> Option<&Token>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{Identifier, IdentifierType, Inlinable};

    fn test(code: &str) -> Vec<RawToken> {
        let iter = code.chars().peekable();
        let tokenizer = Tokenizer::from(iter);
        tokenizer.map(|t| t.inner).collect()
    }
    fn num(n: i64) -> RawToken {
        RawToken::Inlined(Inlinable::Int(n))
    }
    fn func(path: &str) -> RawToken {
        ident(path, IdentifierType::Normal)
    }
    fn oper(path: &str) -> RawToken {
        ident(path, IdentifierType::Operator)
    }
    fn ident(path: &str, kind: IdentifierType) -> RawToken {
        let mut path = path
            .split(|c| c == ':')
            .map(|s| s.to_owned())
            .collect::<Vec<String>>();
        let name = path.pop().unwrap();
        RawToken::Identifier(Identifier {
            name,
            path,
            kind,
            anot: None,
        })
    }

    #[test]
    fn operator() {
        let result = test("4 + 4 + 4");
        assert_eq!(result, vec![num(4), oper("+"), num(4), oper("+"), num(4)]);
    }

    #[test]
    fn function() {
        let result = test("math:add 4 4");
        assert_eq!(result, vec![func("math:add"), num(4), num(4)]);
    }

    #[test]
    fn complicated() {
        let result = test("math:add (4 + 4) << (1 + 1) + << 1 + 1");
        assert_eq!(
            result,
            vec![
                func("math:add"),
                RawToken::Key(Key::ParenOpen),
                num(4),
                oper("+"),
                num(4),
                RawToken::Key(Key::ParenClose),
                RawToken::Key(Key::Pipe),
                RawToken::Key(Key::ParenOpen),
                num(1),
                oper("+"),
                num(1),
                RawToken::Key(Key::ParenClose),
                oper("+"),
                RawToken::Key(Key::Pipe),
                num(1),
                oper("+"),
                num(1),
            ]
        )
    }

    #[test]
    fn weird_spaces() {
        let result = test("math:add( 4 + 2)<< 3 + 3");
        assert_eq!(
            result,
            vec![
                func("math:add"),
                RawToken::Key(Key::ParenOpen),
                num(4),
                oper("+"),
                num(2),
                RawToken::Key(Key::ParenClose),
                RawToken::Key(Key::Pipe),
                num(3),
                oper("+"),
                num(3),
            ]
        )
    }

    #[test]
    fn lambda() {
        let result = test("\\n -> n + 1");
        assert_eq!(
            result,
            vec![
                RawToken::Key(Key::Lambda),
                func("n"),
                RawToken::Key(Key::Arrow),
                func("n"),
                oper("+"),
                num(1),
            ]
        )
    }

    #[test]
    fn closure() {
        let result = test("#(\\n -> n)");
        assert_eq!(
            result,
            vec![
                RawToken::Key(Key::ClosureMarker),
                RawToken::Key(Key::ParenOpen),
                RawToken::Key(Key::Lambda),
                func("n"),
                RawToken::Key(Key::Arrow),
                func("n"),
                RawToken::Key(Key::ParenClose),
            ]
        )
    }
}
