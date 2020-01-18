use std::convert::TryFrom;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Key {
    ParenOpen,
    ParenClose,
    Arrow,
    Pipe,
    Bar,
    ClosureMarker,
    ListOpen,
    ListClose,
    RecordOpen,
    RecordClose,
    Match,
    Comma,
    Lambda,
    If,
    Elif,
    Else,
    Then,
    And,
    Or,
    Dot,
    First,
    Colon,
    Where,
    PrimitiveExit,
    PrimitiveUnimplemented,
}

impl TryFrom<&str> for Key {
    type Error = ();

    fn try_from(bytes: &str) -> Result<Key, Self::Error> {
        let res = match bytes {
            "\\" => Key::Lambda,
            "(" => Key::ParenOpen,
            ")" => Key::ParenClose,
            "->" => Key::Arrow,
            "<<" => Key::Pipe,
            "|" => Key::Bar,
            "#" => Key::ClosureMarker,
            "[" => Key::ListOpen,
            "]" => Key::ListClose,
            "{" => Key::RecordOpen,
            "}" => Key::RecordClose,
            ":" => Key::Colon,
            "," => Key::Comma,
            "." => Key::Dot,
            "match" => Key::Match,
            "if" => Key::If,
            "elif" => Key::Elif,
            "else" => Key::Else,
            "then" => Key::Then,
            "where" => Key::Where,
            "exit" => Key::PrimitiveExit,
            "or" => Key::Or,
            "and" => Key::And,
            "first" => Key::First,
            "???" => Key::PrimitiveUnimplemented,
            _ => return Err(()),
        };
        Ok(res)
    }
}

impl Key {
    pub fn as_str(&self) -> &str {
        match self {
            Key::Lambda => "\\",
            Key::ParenOpen => "(",
            Key::ParenClose => ")",
            Key::Arrow => "->",
            Key::Pipe => "<<",
            Key::Bar => "|",
            Key::ClosureMarker => "#",
            Key::ListOpen => "[",
            Key::ListClose => "]",
            Key::RecordOpen => "{",
            Key::RecordClose => "}",
            Key::Colon => ":",
            Key::Comma => ",",
            Key::Dot => ".",
            Key::Match => "match",
            Key::If => "if",
            Key::Elif => "elif",
            Key::Else => "else",
            Key::Then => "then",
            Key::And => "and",
            Key::Or => "or",
            Key::First => "first",
            Key::Where => "where",
            Key::PrimitiveExit => "exit",
            Key::PrimitiveUnimplemented => "unimplemented",
        }
    }
}

impl fmt::Display for Key {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
