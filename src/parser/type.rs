use super::{BodySource, ParseError, ParseFault, RawToken, Tokenizer};
use std::convert::TryFrom;
use std::fmt;

pub trait Typeable {
    fn of_type(&self) -> Type;
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
    Custom(String),
}

impl std::default::Default for Type {
    fn default() -> Self {
        Type::Nothing
    }
}

pub fn parse_type_decl(
    tokenizer: &mut Tokenizer,
) -> Result<(String, Vec<(String, Type)>), ParseError> {
    let first = match tokenizer.next() {
        None => {
            return ParseFault::EndedWhileExpecting(vec![RawToken::Identifier(
                vec!["custom type name".into()],
                None,
            )])
            .to_err(0)
            .into()
        }
        Some(t) => t,
    };
    let type_name = match first.inner {
        RawToken::Identifier(mut name, anot) => {
            if name.len() != 1 {
                panic!("ET: Type name cannot be external");
            } else {
                name.remove(0)
            }
        }
        _ => panic!("ERROR_TODO: Wanted type name, got {:?}", first),
    };
    let mut fields = Vec::new();
    loop {
        if tokenizer.next().map(|t| t.inner) != Some(RawToken::NewLine) {
            panic!("Expected newline")
        }
        tokenizer.skip_spaces_and_newlines();

        let next = tokenizer.next().expect("ERROR_TODO: File ended");
        let field_name = match next.inner {
            RawToken::Identifier(mut field_name, anot) => {
                if field_name.len() != 1 {
                    panic!("ET: Custom Type name cannot be external");
                } else {
                    field_name.remove(0)
                }
            }
            RawToken::Header(h) => {
                tokenizer.regress(h.as_str().len() + 1);
                break;
            }
            _ => panic!("ERROR_TODO: Unexpected thingy in field decl, {:?}", next),
        };
        let next = tokenizer.next().expect("ERROR_TODO");
        match next.inner {
            RawToken::Identifier(type_name, anot) => {
                fields.push((field_name.to_owned(), Type::try_from(type_name).unwrap()))
            }
            _ => panic!(
                "ERROR_TODO: Invalid syntax in field decleration, got {:?}",
                next
            ),
        }
    }
    Ok((type_name, fields))
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
