use super::Type;
use crate::parser::{tokenizer::TokenSource, Identifier, ParseError, RawToken, Tokenizer};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;

pub struct Enum {
    pub fields: HashMap<String, Vec<Type>>,

    pub type_args: Vec<()>,
}

pub fn parse<I: Iterator<Item = char>>(
    tokenizer: &mut Tokenizer<I>,
) -> Result<(Identifier<Type>, HashMap<String, Vec<Type>>), ParseError> {
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

    let mut fields = HashMap::new();
    loop {
        match parse_field(tokenizer)? {
            Some((name, type_arguments)) => {
                if fields.insert(name, type_arguments).is_some() {
                    panic!("ET: Duplicates of field");
                }
            }
            None => return Ok((type_ident, fields)),
        }
    }
}

fn parse_field<I: Iterator<Item = char>>(
    tokenizer: &mut Tokenizer<I>,
) -> Result<Option<(String, Vec<Type>)>, ParseError> {
    let first = tokenizer.peek();

    match first.map(|a| &a.inner) {
        Some(RawToken::Identifier(_)) => {
            let (field_name_ident, _field_pos) = assume!(RawToken::Identifier, tokenizer.next());
            let type_arguments = parse_type_arguments(tokenizer)?;
            Ok(Some((field_name_ident.name, type_arguments)))
        }
        Some(RawToken::NewLine) => {
            tokenizer.next();
            parse_field(tokenizer)
        }
        Some(RawToken::Header(_)) | None => Ok(None),
        Some(other) => panic!("ET: Unexpected stuff here: {:?}", other),
    }
}
fn parse_type_arguments<I: Iterator<Item = char>>(
    tokenizer: &mut Tokenizer<I>,
) -> Result<Vec<Type>, ParseError> {
    let mut buf = Vec::new();
    loop {
        let t = tokenizer.peek().map(|t| &t.inner);
        match t {
            Some(RawToken::Header(_)) | None | Some(RawToken::NewLine) => return Ok(buf),
            Some(RawToken::Identifier(_)) => {
                let (type_param_type_name, pos) = assume!(RawToken::Identifier, tokenizer.next());
                let t = Type::try_from(type_param_type_name.name.as_str())
                    .map_err(|e| e.into_err(pos))?;
                buf.push(t);
            }
            Some(other) => panic!("ET: Unexpected {:?}", other),
        }
    }
}

impl fmt::Display for Enum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<TODO>")?;
        for (name, types) in self.fields.iter() {
            write!(
                f,
                "\n    {} {}",
                name,
                types
                    .iter()
                    .map(|t| fmt::Display::to_string(t))
                    .collect::<Vec<_>>()
                    .join(" ")
            )?;
        }
        Ok(())
    }
}
