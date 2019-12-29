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
}

pub fn get_funcid(ident: &str) -> Result<(u16, NaiveType), ParseFault> {
    let id = match ident {
        "add" => (0, NaiveType::Known(Type::Int)),
        "sub" => (1, NaiveType::Known(Type::Int)),
        "mul" => (2, NaiveType::Known(Type::Int)),
        "div" => (3, NaiveType::Known(Type::Int)),
        "push_back" => (4, NaiveType::Matching(1)),
        "push_front" => (5, NaiveType::Matching(1)),
        _ => {
            return Err(ParseFault::BridgedFunctionNotFound(
                Identifier::try_from(ident).unwrap(),
            ))
        }
    };
    Ok(id)
}
