use crate::parser::{ParseFault, Type};

pub fn try_rust_builtin(entries: &[String]) -> Result<Option<(u16, Type)>, ParseFault> {
    if &entries[0] == "rust" {
        Ok(Some(get_funcid(&entries[1])?))
    } else {
        Ok(None)
    }
}

pub fn get_funcid(ident: &str) -> Result<(u16, Type), ParseFault> {
    let id = match ident {
        "add" => (0, Type::Int),
        "sub" => (1, Type::Int),
        "mul" => (2, Type::Int),
        "div" => (3, Type::Int),
        _ => return Err(ParseFault::BridgedFunctionNotFound(ident.into())),
    };
    Ok(id)
}
