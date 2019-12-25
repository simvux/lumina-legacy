use super::{
    ast::{Identifier, IdentifierType},
    FunctionBuilder, Key, ParseError, ParseFault, RawToken, Tokenizer, Type,
};
use std::convert::TryFrom;

impl FunctionBuilder {
    pub fn with_header_operator<I: Iterator<Item = char>>(
        mut self,
        tokenizer: &mut Tokenizer<I>,
    ) -> Result<Self, ParseError> {
        let first = match tokenizer.next() {
            None => return ParseFault::OpNoIdent.to_err(0).into(),
            Some(t) => t,
        };
        match first.inner {
            RawToken::Identifier(ident) => {
                if ident.kind != IdentifierType::Operator {
                    panic!("ET: *this* is not a valid operator name");
                }
                self.name = ident;
            }
            _ => {
                let source_index = first.pos();
                return ParseFault::OpWantedIdent(first.inner)
                    .to_err(source_index)
                    .into();
            }
        };
        self.with_types_operator(tokenizer)
    }

    fn with_types_operator<I: Iterator<Item = char>>(
        mut self,
        tokenizer: &mut Tokenizer<I>,
    ) -> Result<Self, ParseError> {
        let t = match tokenizer.next() {
            None => {
                return ParseFault::EndedWhileExpecting(vec![RawToken::Key(Key::ParenOpen)])
                    .to_err(0)
                    .into()
            }
            Some(t) => t,
        };
        match t.inner {
            RawToken::Key(Key::ParenOpen) => {}
            _ => {
                let source_index = t.pos();
                return ParseFault::GotButExpected(t.inner, vec![RawToken::Key(Key::ParenOpen)])
                    .to_err(source_index)
                    .into();
            }
        }
        let next_ident = |tokenizer: &mut Tokenizer<I>| -> Result<(Type, usize), ParseError> {
            let next = match tokenizer.next() {
                None => {
                    return ParseFault::EndedWhileExpecting(vec![RawToken::Identifier(
                        Identifier::raw("type"),
                    )])
                    .to_err(0)
                    .into();
                }
                Some(t) => t,
            };
            let source_index = next.pos();
            if let RawToken::Identifier(ident) = next.inner {
                Ok((
                    Type::try_from(ident.name.as_str()).map_err(|e| e.to_err(source_index))?,
                    source_index,
                ))
            } else {
                ParseFault::GotButExpected(
                    next.inner,
                    vec![RawToken::Identifier(Identifier::raw("type"))],
                )
                .to_err(source_index)
                .into()
            }
        };

        let (left, _) = next_ident(tokenizer)?;
        let (right, right_source_index) = next_ident(tokenizer)?;
        {
            let then = tokenizer.next();
            match then.map(|t| t.sep()) {
                Some((RawToken::Key(Key::Arrow), _)) => {}
                Some((other, pos)) => {
                    return Err(
                        ParseFault::GotButExpected(other, vec![RawToken::Key(Key::Arrow)])
                            .to_err(pos),
                    )
                }
                None => {
                    return Err(
                        ParseFault::EndedWhileExpecting(vec![RawToken::Key(Key::Arrow)])
                            .to_err(right_source_index),
                    )
                }
            }
        }
        let (returns, _) = next_ident(tokenizer)?;
        let t = match tokenizer.next() {
            Some(t) => t,
            None => {
                return ParseFault::EndedWhileExpecting(vec![RawToken::Key(Key::ParenClose)])
                    .to_err(right_source_index)
                    .into()
            }
        };
        match t.inner {
            RawToken::Key(Key::ParenClose) => {}
            _ => {
                return ParseFault::GotButExpected(t.inner, vec![RawToken::Key(Key::ParenClose)])
                    .to_err(right_source_index)
                    .into()
            }
        }

        self.parameter_types = vec![left, right];
        self.parameter_names = vec!["left".into(), "right".into()];
        self.returns = returns;
        Ok(self)
    }
}
