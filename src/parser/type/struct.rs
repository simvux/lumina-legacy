use super::Type;
use crate::parser::{
    ast, tokenizer::TokenSource, Anot, Identifier, ParseError, ParseFault, RawToken, Tokenizer,
    Tracked,
};
use std::convert::TryFrom;
use std::fmt;

pub struct Struct {
    pub fields: Vec<(String, Type)>,

    // type_args: Vec<super::Trait>,
    pub type_args: Vec<()>,
}

impl Struct {
    pub fn verify_record_fields(&self, got: &[(String, Type)]) -> Result<(), ParseFault> {
        unimplemented!();
    }

    // We don't actually use the names from the struct fields anymore. We actually
    // just rely on the order they're written in
    //
    // We also grab the type here out of convenience
    pub fn copy_order_for(
        &self,
        other: &[(String, Tracked<ast::Entity>)],
    ) -> Vec<(Tracked<ast::Entity>, Type)> {
        let mut actual = Vec::with_capacity(self.fields.len());
        for (name, t) in self.fields.iter() {
            actual.push((
                other
                    .iter()
                    .find_map(|(n, e)| if n == name { Some(e.clone()) } else { None })
                    .unwrap_or_else(|| panic!("TODO: Default missing fields")),
                t.clone(),
            ))
        }
        if actual.len() != other.len() {
            panic!("ET: This record has a field that doesn't exist on the type");
        }
        actual
    }
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
) -> Result<(Anot<Identifier, Type>, Vec<(String, Type)>), ParseError> {
    let first = tokenizer.next().ok_or_else(|| panic!("ET"))?;
    let type_ident_pos = first.pos();
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
            Some((name, t)) => {
                fields.push((name, t));
                if let Some(a) = tokenizer.next() {
                    let (rt, _pos) = a.sep();
                    if rt != RawToken::NewLine {
                        panic!("ET: Expected newline, got {:?}", rt);
                    }
                } else {
                    // This field line was the last in the file
                    let type_ident = type_ident
                        .try_map_anot(|s| Type::try_from(s.as_str()))
                        .map_err(|e| e.into_err(type_ident_pos))?;
                    return Ok((type_ident, fields));
                }
            }
            None => {
                let type_ident = type_ident
                    .try_map_anot(|s| Type::try_from(s.as_str()))
                    .map_err(|e| e.into_err(type_ident_pos))?;
                return Ok((type_ident, fields));
            }
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
                let t = Type::try_from(field_type_ident.inner.name.as_str())
                    .map_err(|e| e.into_err(pos))?;
                Ok(Some((field_name_ident.inner.name, t)))
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
