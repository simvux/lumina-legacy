use super::{BodySource, Key, ParseError, ParseFault, RawToken, Token, Tokenizer, Type};
use std::convert::TryFrom;

pub fn into_annotated(mut path: Vec<String>) -> Result<RawToken, ParseFault> {
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
                    return Ok(RawToken::Identifier(path, Some(anot_buf)));
                }
            }
            panic!("Unmatched >")
        }
        if c == b'>' {
            panic!("Unmatched < (<> are used for type annotations, if you intend to use an operator then put a space between the words)");
        }
    }
    Ok(RawToken::Identifier(path, None))
}

pub fn annotated(t: Token) -> Result<Token, ParseFault> {
    if let RawToken::Identifier(ident, _anot) = t.inner {
        let source = t.source_index;
        into_annotated(ident).map(|a| Token::new(a, source))
    } else {
        Ok(t)
    }
}
