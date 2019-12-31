use super::{
    ast::IdentifierType, FunctionBuilder, Key, ParseError, ParseFault, RawToken, Tokenizer,
};

// Operator reuses most of the function-construction methods

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
        self = self.with_parameter_types(tokenizer)?;
        if self.parameter_types.len() != 2 {
            panic!("ET: Operator needs two parameters")
        }
        self.parameter_names = vec!["left".into(), "right".into()];
        Ok(self)
    }
}
