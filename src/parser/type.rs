use super::ast::{Identifier, IdentifierType};
use super::{tokenizer::TokenSource, ParseError, ParseFault, RawToken, Tokenizer};
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

macro_rules! assume {
    ($path:path, $v:expr) => {
        if let Some(($path(a), pos)) = $v.map(|a| a.sep()) {
            (a, pos)
        } else {
            unreachable!()
        }
    };
}

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub enum Type {
    Nothing,
    Int,
    Float,
    Bool,
    External(Box<(String, Type)>),
    Generic(u8),
    List(Box<Type>),
    Struct(i32, i32),
    Function(Box<(Vec<Type>, Type)>),
    Custom(Identifier),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MaybeType {
    Infer(Rc<RefCell<Option<Type>>>),
    Known(Type),
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

pub fn parse_type_decl<I: Iterator<Item = char>>(
    tokenizer: &mut Tokenizer<I>,
) -> Result<(Identifier, Vec<(String, Type)>), ParseError> {
    let first = tokenizer.next().ok_or_else(|| panic!("ET"))?;
    let type_ident = if let RawToken::Identifier(ident) = first.inner {
        ident
    } else {
        panic!("ET");
    };
    let (after, _after_pos) = tokenizer
        .next()
        .ok_or_else(|| panic!("ET: Wanted newline after type header and identifier"))?
        .sep();
    if after != RawToken::NewLine {
        panic!(
            "ET: Wanted newline after type header and identifier, got {:?}",
            after
        );
    }

    let mut fields = Vec::new();
    loop {
        match parse_field(tokenizer)? {
            Some(field) => {
                fields.push(field);
                if let Some(a) = tokenizer.next() {
                    let (rt, _pos) = a.sep();
                    if rt != RawToken::NewLine {
                        panic!("ET: Expected newline, got {:?}", rt);
                    }
                } else {
                    // This field line was the last in the file
                    return Ok((type_ident, fields));
                }
            }
            None => return Ok((type_ident, fields)),
        }
    }
}

fn parse_field<I: Iterator<Item = char>>(
    tokenizer: &mut Tokenizer<I>,
) -> Result<Option<(String, Type)>, ParseError> {
    let first = tokenizer.peek();

    match first.map(|a| &a.inner) {
        Some(RawToken::Identifier(_)) => {
            let (field_name_ident, _field_pos) = assume!(RawToken::Identifier, tokenizer.next());
            let (second, pos) = tokenizer.next().ok_or_else(|| panic!("ET"))?.sep();
            if let RawToken::Identifier(field_type_ident) = second {
                let t =
                    Type::try_from(field_type_ident.name.as_str()).map_err(|e| e.to_err(pos))?;
                Ok(Some((field_name_ident.name, t)))
            } else {
                panic!("ET {:?} cannot be used as field type", second);
            }
        }
        Some(RawToken::NewLine) => {
            tokenizer.next();
            parse_field(tokenizer)
        }
        Some(RawToken::Header(_)) | None => Ok(None),
        Some(other) => panic!("ET: Unexpected stuff here: {:?}", other),
    }
}

impl TryFrom<&str> for Type {
    type Error = ParseFault;

    fn try_from(source: &str) -> Result<Type, Self::Error> {
        if source.bytes().next() == Some(b'[') {
            if source.len() < 3 {
                return Err(ParseFault::EmptyListType);
            }
            let inner = source[1..source.len() - 2].trim();
            return Ok(Type::List(Box::new(Type::try_from(inner)?)));
        }
        if source.len() == 1
            && source.bytes().next() > Some(96)
            && source.bytes().next() < Some(123)
        {
            return Ok(Type::Generic(source.bytes().next().unwrap() - 97));
        }
        let r = match source {
            "int" => Type::Int,
            "float" => Type::Float,
            "nothing" => Type::Nothing,
            "bool" => Type::Bool,
            "_" => Type::Nothing,
            // TODO: Custom types need to be passed as okay!
            _ => return Err(ParseFault::NotValidType(source.into())),
        };
        Ok(r)
    }
}

impl TryFrom<Vec<String>> for Type {
    type Error = ParseFault;

    fn try_from(mut entries: Vec<String>) -> Result<Type, Self::Error> {
        let r#type = Type::try_from(entries.last().unwrap().as_str())?;
        if entries.len() == 1 {
            Ok(r#type)
        } else {
            Ok(Type::External(Box::new((entries.remove(0), r#type))))
        }
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
            None => panic!("ET: Annotation missing `>`"),
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
