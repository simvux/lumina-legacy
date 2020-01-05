use super::Type;
use crate::parser::{tokenizer::TokenSource, Identifier, ParseError, RawToken, Tokenizer};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;

pub struct Struct {
    pub fields: HashMap<String, Type>,

    // type_args: Vec<super::Trait>,
    pub type_args: Vec<()>,
}

impl fmt::Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<TODO>")?;
        for (name, types) in self.fields.iter() {
            write!(f, "\n    {} {}", name, types,)?;
        }
        Ok(())
    }
}

pub fn parse<I: Iterator<Item = char>>(
    tokenizer: &mut Tokenizer<I>,
) -> Result<(Identifier<Type>, HashMap<String, Type>), ParseError> {
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
            Some((name, t)) => {
                fields.insert(name, t);
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
                    Type::try_from(field_type_ident.name.as_str()).map_err(|e| e.into_err(pos))?;
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
