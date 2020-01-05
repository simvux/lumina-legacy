use crate::parser::{Identifier, ParseFault, Type};
use std::convert::TryFrom;
use std::fmt;
use strum_macros::{AsRefStr, EnumString};

pub fn try_rust_builtin(entries: &[String]) -> Result<Option<(Bridged, NaiveType)>, ParseFault> {
    if &entries[0] == "rust" {
        Ok(Some(get_funcid(&entries[1])?))
    } else {
        Ok(None)
    }
}

// This is used to determine the return type of the builtins in the checker
// Matching: the return type is the same as a previous parameter
// ListedMatching: The return type is the same as a previous parameter but wrapped in list
// UnlistedMatching: The return type is the same as the inner value of a previous list parameter
//
// Most of the builtin functions are generic. Which is why this is required.
pub enum NaiveType {
    Known(Type),
    Matching(u16),
    ListedMatching(u16),
    UnlistedMatching(u16),
}

#[allow(non_camel_case_types)]
#[repr(u16)]
#[derive(EnumString, AsRefStr, Debug, Clone, Copy)]
pub enum Bridged {
    add,
    sub,
    mul,
    div,
    push_back,
    push_front,
    get,
    len,
    eq,
    lt,
    steal,
    remove,
    print_any,
    map_overwrite,
    append,
}
use Bridged::*;

pub fn get_funcid(ident: &str) -> Result<(Bridged, NaiveType), ParseFault> {
    let id = match ident {
        "add" => (add, NaiveType::Matching(0)),
        "sub" => (sub, NaiveType::Matching(0)),
        "mul" => (mul, NaiveType::Matching(0)),
        "div" => (div, NaiveType::Matching(0)),
        "push_back" => (push_back, NaiveType::Matching(1)),
        "push_front" => (push_front, NaiveType::Matching(1)),
        "get" => (get, NaiveType::UnlistedMatching(1)),
        "len" => (len, NaiveType::Known(Type::Int)),
        "eq" => (eq, NaiveType::Known(Type::Bool)),
        "lt" => (lt, NaiveType::Known(Type::Bool)),
        "steal" => (steal, NaiveType::Matching(1)),
        "remove" => (remove, NaiveType::Matching(1)),
        "print_any" => (print_any, NaiveType::Known(Type::Nothing)),
        "map_overwrite" => (map_overwrite, NaiveType::Matching(1)),
        "append" => (append, NaiveType::Matching(0)),
        _ => {
            return Err(ParseFault::BridgedFunctionNotFound(
                Identifier::try_from(ident).unwrap(),
            ))
        }
    };
    Ok(id)
}

pub fn name_from_funcid(f: &mut fmt::Formatter, func: Bridged) -> fmt::Result {
    write!(f, "{}", func.as_ref())
}
