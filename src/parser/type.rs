use super::ParseFault;
use super::{Identifier, IdentifierType, Inlinable};
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

pub mod r#enum;
pub use r#enum::Enum;
pub mod r#struct;
pub use r#struct::Struct;

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub enum Type {
    Nothing,
    Int,
    Float,
    Bool,
    Generic(u8),
    List(Box<Type>),
    Struct(i32, i32),
    Function(Box<(Vec<Type>, Type)>),

    External(Box<(String, Type)>), // This one is all wrong
    Custom(Identifier<Type>),
}

pub enum CustomType {
    Struct(Struct),
    Enum(Enum),
}
impl fmt::Display for CustomType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CustomType::Enum(a) => write!(f, "{}", a),
            CustomType::Struct(a) => write!(f, "{}", a),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MaybeType {
    Infer(Rc<RefCell<Option<Type>>>),
    Known(Type),
}
impl Default for MaybeType {
    fn default() -> Self {
        Self::new()
    }
}

impl MaybeType {
    pub fn new() -> Self {
        Self::Infer(Rc::default())
    }
    pub fn unwrap(self) -> Type {
        match self {
            MaybeType::Infer(t) => t.borrow().clone().unwrap(),
            MaybeType::Known(t) => t,
        }
    }
}
impl Hash for MaybeType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            MaybeType::Infer(t) => t.borrow().as_ref().unwrap_or(&Type::Nothing).hash(state),
            MaybeType::Known(t) => t.hash(state),
        }
    }
}

impl Type {
    pub fn decoded(self, generics: &HashMap<u8, Type>) -> Self {
        match self {
            Type::Generic(n) => generics[&n].clone(),
            Type::List(box t) => Type::List(Box::new(t.decoded(generics))),
            Type::Function(attr) => {
                // TODO: Clone can be avoided
                let (mut params, returns) = (attr.0, attr.1);
                params
                    .iter_mut()
                    .for_each(|t| *t = t.clone().decoded(generics));
                Type::Function(Box::new((params, returns.decoded(generics))))
            }
            _ => self,
        }
    }
}

impl std::default::Default for Type {
    fn default() -> Self {
        Type::Nothing
    }
}

impl TryFrom<&str> for Type {
    type Error = ParseFault;

    fn try_from(source: &str) -> Result<Type, Self::Error> {
        if let Some(first) = source.chars().next() {
            // Lists
            if first == '[' {
                if source.len() < 3 {
                    return Err(ParseFault::EmptyListType);
                }
                let inner = source[1..source.len() - 2].trim();
                return Ok(Type::List(Box::new(Type::try_from(inner)?)));
            }
            // Unbound Generics
            if (first as u8) > 96 && (first as u8) < 123 && source.len() == 1 {
                return Ok(Type::Generic(first as u8 - 97));
            }
        } else {
            panic!("Empty type");
        }

        let mut iter = source.chars();
        let mut tbuf = String::new();
        let has_anot = loop {
            match iter.next() {
                Some(c) => {
                    if c == '<' {
                        break true;
                    }
                    tbuf.push(c);
                }
                None => break false,
            };
        };
        let anot = if has_anot {
            annotation(&mut iter)
        } else {
            None
        };
        assert_eq!(iter.next(), None);
        let t = match tbuf.as_str() {
            "int" => Type::Int,
            "float" => Type::Float,
            "nothing" | "_" => Type::Nothing,
            "bool" => Type::Bool,
            _ => {
                let mut path = tbuf.split(':').map(|s| s.to_owned()).collect::<Vec<_>>();
                let name = path.pop().unwrap();
                Type::Custom(Identifier {
                    name,
                    anot,
                    path,
                    kind: IdentifierType::Normal,
                })
            }
        };
        Ok(t)
    }
}

pub fn splice_to<I: Iterator<Item = char>>(iter: &mut I, points: &str) -> Option<(char, Type)> {
    let mut s = String::new();
    while let Some(c) = iter.next() {
        if points.contains(|a| a == c) {
            let t = Type::try_from(s.trim()).expect("ET");
            return Some((c, t));
        }
        match c {
            '[' => {
                if !s.is_empty() {
                    panic!("ET: Unexpected [");
                }
                let (a, t) = splice_to(iter, "]")?;
                assert_eq!(a, ']');
                let after = iter.next();
                return Some((after.unwrap_or(a), Type::List(Box::new(t))));
            }
            '<' => {
                let anot = annotation(iter).expect("ET");
                return Some((
                    '>',
                    (Type::Custom(Identifier {
                        path: Vec::new(),
                        name: s,
                        anot: Some(anot),
                        kind: IdentifierType::Normal,
                    })),
                ));
            }
            _ => {}
        }
        s.push(c);
    }
    None
}

pub fn annotation<I: Iterator<Item = char>>(iter: &mut I) -> Option<Vec<Type>> {
    let mut annotations = Vec::new();
    loop {
        match splice_to(iter, ",>") {
            Some((was, t)) => match was {
                ',' => {
                    annotations.push(t);
                }
                '>' => {
                    annotations.push(t);
                    if let Some(c) = iter.next() {
                        panic!("ET: Unexpected {}", c);
                    }
                    return Some(annotations);
                }
                _ => unreachable!(),
            },
            None => {
                if annotations.is_empty() {
                    return Some(Vec::new());
                } else {
                    panic!("ET: Annotation missing `>`")
                }
            }
        }
    }
}

impl From<&Inlinable> for Type {
    fn from(v: &Inlinable) -> Type {
        match v {
            Inlinable::Int(_) => Type::Int,
            Inlinable::Float(_) => Type::Float,
            Inlinable::Bool(_) => Type::Bool,
            Inlinable::Nothing => Type::Nothing,
        }
    }
}

impl fmt::Display for MaybeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MaybeType::Infer(t) => match t.borrow().as_ref() {
                Some(known) => known.fmt(f),
                None => write!(f, "?"),
            },
            MaybeType::Known(known) => known.fmt(f),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Nothing => f.write_str("nothing"),
            Type::Int => f.write_str("int"),
            Type::Float => f.write_str("float"),
            Type::Bool => f.write_str("bool"),
            Type::Generic(gid) => write!(f, "{}", (gid + 97) as char),
            Type::Function(box (takes, gives)) => write!(
                f,
                "({} -> {})",
                takes
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<String>>()
                    .join(" "),
                gives
            ),
            Type::List(inner) => write!(f, "[{}]", inner.to_string()),
            Type::Struct(fid, tid) => write!(f, "Struct({}:{})", fid, tid),
            Type::External(box (modname, t)) => write!(f, "{}:{}", modname, t.to_string()),
            Type::Custom(name) => write!(f, "unevaluated type {}", name),
        }
    }
}
