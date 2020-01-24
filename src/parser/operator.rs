use super::{
    tokenizer::TokenSource, Attr, FunctionBuilder, IdentifierType, Key, ParseError, ParseFault,
    RawToken, Tokenizer,
};
use std::convert::TryFrom;

// Operator reuses most of the function-construction methods

impl FunctionBuilder {
    pub fn with_header_operator<I: Iterator<Item = char>>(
        mut self,
        tokenizer: &mut Tokenizer<I>,
    ) -> Result<Self, ParseError> {
        let first = match tokenizer.next() {
            None => return ParseFault::OpNoIdent.into_err(0).into(),
            Some(t) => t,
        };
        let first_pos = first.pos();
        match first.inner {
            RawToken::Identifier(ident) => {
                if ident.inner.kind != IdentifierType::Operator {
                    panic!("ET: *this* is not a valid operator name");
                }
                self.name = ident
                    .try_map_anot(|s| Attr::try_from(s.as_str()))
                    .map_err(|e| e.into_err(first_pos))?;
            }
            _ => {
                return ParseFault::OpWantedIdent(first.inner)
                    .into_err(first_pos)
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
                return ParseFault::EndedWhileExpecting(vec!["(".into()])
                    .into_err(0)
                    .into()
            }
            Some(t) => t,
        };
        match t.inner {
            RawToken::Key(Key::ParenOpen) => {}
            _ => {
                let source_index = t.pos();
                return ParseFault::GotButExpected(t.inner, vec!["(".into()])
                    .into_err(source_index)
                    .into();
            }
        }
        self = self.with_parameter_types(tokenizer)?;
        if self.parameter_types.len() != 2 {
            panic!("ET: Operator needs two parameters")
        }
        self.parameter_names = vec!["left".into(), "right".into()];
        Ok(self)
    }
}
