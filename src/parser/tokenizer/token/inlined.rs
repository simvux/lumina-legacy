use crate::ir::Value;
use std::convert::TryFrom;
use std::fmt;

#[derive(PartialEq, Clone)]
pub enum Inlinable {
    Int(i64),
    Float(f64),
    Bool(bool),
    Nothing,
}

impl TryFrom<&str> for Inlinable {
    type Error = ();

    fn try_from(bytes: &str) -> Result<Inlinable, ()> {
        if let Ok(int) = bytes.parse::<i64>() {
            return Ok(Inlinable::Int(int));
        };
        if let Ok(float) = bytes.parse::<f64>() {
            return Ok(Inlinable::Float(float));
        };
        if let Ok(boolean) = bytes.parse::<bool>() {
            return Ok(Inlinable::Bool(boolean));
        }
        if bytes == "_" {
            return Ok(Inlinable::Nothing);
        }
        let first = bytes.chars().next();
        if first == Some('"') {
            unimplemented!("string literals");
        }
        if first == Some('\'') {
            unimplemented!("byte literals");
        }
        Err(())
    }
}

impl fmt::Display for Inlinable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Inlinable::Int(n) => write!(f, "{}", n),
            Inlinable::Float(n) => write!(f, "{}", n),
            Inlinable::Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Inlinable::Nothing => f.write_str("_"),
        }
    }
}
impl fmt::Debug for Inlinable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl From<Inlinable> for Value {
    fn from(inlined: Inlinable) -> Value {
        match inlined {
            Inlinable::Int(n) => Value::Int(n),
            Inlinable::Float(n) => Value::Float(n),
            Inlinable::Nothing => Value::Nothing,
            Inlinable::Bool(b) => Value::Bool(b),
        }
    }
}
