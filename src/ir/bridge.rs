use crate::parser::{Identifier, ParseFault, Type};
use std::convert::TryFrom;
use std::fmt;

pub fn try_rust_builtin(entries: &[String]) -> Result<Option<(u16, NaiveType)>, ParseFault> {
    if &entries[0] == "rust" {
        Ok(Some(get_funcid(&entries[1])?))
    } else {
        Ok(None)
    }
}

pub enum NaiveType {
    Known(Type),
    Matching(u16),
    ListedMatching(u16),
    UnlistedMatching(u16),
}

pub fn get_funcid(ident: &str) -> Result<(u16, NaiveType), ParseFault> {
    let id = match ident {
        "add" => (0, NaiveType::Matching(0)),
        "sub" => (1, NaiveType::Matching(0)),
        "mul" => (2, NaiveType::Matching(0)),
        "div" => (3, NaiveType::Matching(0)),
        "push_back" => (4, NaiveType::Matching(1)),
        "push_front" => (5, NaiveType::Matching(1)),
        "get" => (6, NaiveType::UnlistedMatching(1)),
        "len" => (7, NaiveType::Known(Type::Int)),
        "eq" => (8, NaiveType::Known(Type::Bool)),
        "lt" => (9, NaiveType::Known(Type::Bool)),
        "steal" => (10, NaiveType::Matching(1)),
        "remove" => (11, NaiveType::Matching(1)),
        _ => {
            return Err(ParseFault::BridgedFunctionNotFound(
                Identifier::try_from(ident).unwrap(),
            ))
        }
    };
    Ok(id)
}

pub fn name_from_funcid(f: &mut fmt::Formatter, n: u16) -> fmt::Result {
    match n {
        0 => write!(f, "add"),
        1 => write!(f, "sub"),
        2 => write!(f, "mul"),
        3 => write!(f, "div"),
        4 => write!(f, "push_back"),
        5 => write!(f, "push_front"),
        6 => write!(f, "get"),
        7 => write!(f, "len"),
        8 => write!(f, "eq"),
        9 => write!(f, "lt"),
        10 => write!(f, "steal"),
        11 => write!(f, "remove"),
        _ => write!(f, "ERROR_UNEXISTENT_BUILTIN_{}", n),
    }
}
