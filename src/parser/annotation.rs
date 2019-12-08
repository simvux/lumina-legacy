use super::{ParseFault, RawToken, Token, Type};
use std::convert::TryFrom;

pub fn into_annotated_str(
    mut path: Vec<String>,
) -> Result<(Vec<String>, Option<Vec<Type>>), ParseFault> {
    // TODO: bruh what even is this code, no idea what mood I was in when writing this but it certainly
    // doesn't look great now does. it
    let s = path.as_slice().last().unwrap();
    for (i, c) in s.bytes().enumerate() {
        if c == b'<' {
            for (i2, c2) in s[i..].bytes().enumerate() {
                if c2 == b'>' {
                    let ident = &s[..i];
                    let anot = &s[i + 1..i + i2];
                    let mut anot_buf = Vec::new();
                    for ent in anot.split(',') {
                        anot_buf.push(Type::try_from(ent)?)
                    }
                    let ident = ident.to_owned();
                    path.pop();
                    path.push(ident);
                    return Ok((path, Some(anot_buf)));
                }
            }
            panic!("Unmatched >")
        }
        if c == b'>' {
            panic!("Unmatched < (<> are used for type annotations, if you intend to use an operator then put a space between the words)");
        }
    }
    Ok((path, None))
}

pub fn into_annotated(path: Vec<String>) -> Result<RawToken, ParseFault> {
    let (name, anot) = into_annotated_str(path)?;
    Ok(RawToken::Identifier(name, anot))
}

pub fn annotated(t: Token) -> Result<Token, ParseFault> {
    if let RawToken::Identifier(ident, _anot) = t.inner {
        let source = t.source_index;
        into_annotated(ident).map(|a| Token::new(a, source))
    } else {
        Ok(t)
    }
}
