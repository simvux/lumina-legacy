use crate::parser::{Identifier, ParseFault, Type};
use std::convert::TryFrom;

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
        "remove" => (10, NaiveType::Matching(1)),
        _ => {
            return Err(ParseFault::BridgedFunctionNotFound(
                Identifier::try_from(ident).unwrap(),
            ))
        }
    };
    Ok(id)
}

pub fn name_from_funcid<'a>(n: u16) -> &'a str {
    match n {
        0 => "add",
        1 => "sub",
        2 => "mul",
        3 => "div",
        4 => "push_back",
        5 => "push_front",
        6 => "get",
        7 => "len",
        8 => "eq",
        9 => "lt",
        10 => "remove",
        _ => "ERROR_UNEXISTENT_BUILTIN",
    }
}
