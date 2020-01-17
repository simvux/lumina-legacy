use super::{
    ast, ast::AstBuilder, tokenizer::TokenSource, Attr, Identifier, Key, ParseError, ParseFault,
    RawToken, Tokenizer, Tracked, Type,
};
use std::convert::TryFrom;
use std::convert::TryInto;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;

// FunctionBuilder represents the AST-stage of a function or operator
// (operators are turned into functions)
#[derive(Default, Clone)]
pub struct FunctionBuilder {
    pub name: Identifier<Attr>,
    pub parameter_names: Vec<String>,
    pub parameter_types: Vec<Type>,
    pub returns: Type,
    pub body: Tracked<ast::Entity>,
    pub wheres: Vec<(String, Tracked<ast::Entity>)>,
}

impl PartialEq for FunctionBuilder {
    fn eq(&self, other: &FunctionBuilder) -> bool {
        self.name == other.name
            && self.parameter_types == other.parameter_types
            && self.returns == other.returns
    }
}
impl Eq for FunctionBuilder {}

// The same function can be either *already built* or *pending to be built*.
// therefore we cannot rely on the body/wheres when hashing.
impl Hash for FunctionBuilder {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.parameter_types.hash(state);
        self.returns.hash(state);
    }
}

impl FunctionBuilder {
    pub fn new() -> Self {
        FunctionBuilder {
            name: Identifier::default(),
            parameter_names: Vec::new(),
            parameter_types: Vec::new(),
            returns: Type::default(),
            body: Tracked::default(),
            wheres: Vec::new(),
        }
    }

    pub fn with_header<I: Iterator<Item = char>>(
        mut self,
        tokenizer: &mut Tokenizer<I>,
    ) -> Result<Self, ParseError> {
        let first = match tokenizer.next() {
            None => {
                return ParseFault::EndedWhileExpecting(vec!["function name".into()])
                    .into_err(0)
                    .into()
            }
            Some(t) => t,
        };
        self.name = match first.sep() {
            (RawToken::Identifier(ident), pos) => {
                if !ident.path.is_empty() {
                    panic!("ET: Path in function name");
                }
                ident.try_into().map_err(|e: ParseFault| e.into_err(pos))?
            }
            (other, pos) => {
                return ParseFault::GotButExpected(other, vec!["function name".into()])
                    .into_err(pos)
                    .into();
            }
        };

        self.with_parameter_names(tokenizer)
    }

    // TODO: Parameter names can be operator identifiers
    fn with_parameter_names<I: Iterator<Item = char>>(
        mut self,
        tokenizer: &mut Tokenizer<I>,
    ) -> Result<Self, ParseError> {
        loop {
            let next = match tokenizer.next() {
                Some(t) => t,
                None => {
                    return ParseFault::EndedWhileExpecting(vec![
                        "(".into(),
                        "parameter name".into(),
                    ])
                    .into_err(tokenizer.position - 1)
                    .into();
                }
            };
            match next.inner {
                RawToken::NewLine => return Ok(self),
                RawToken::Identifier(ident) => self.parameter_names.push(ident.name),
                RawToken::Key(Key::ParenOpen) => return self.with_parameter_types(tokenizer),
                _ => {
                    let source_index = next.pos();
                    return ParseFault::GotButExpected(
                        next.inner,
                        vec!["parameter name".into(), "(".into()],
                    )
                    .into_err(source_index)
                    .into();
                }
            }
        }
    }

    fn err_type_expecting(&self) -> Vec<String> {
        if self.parameter_types.is_empty() {
            vec!["parameter type".into()]
        } else {
            vec!["parameter type".into(), "->".into()]
        }
    }

    pub fn with_parameter_types<I: Iterator<Item = char>>(
        mut self,
        tokenizer: &mut Tokenizer<I>,
    ) -> Result<Self, ParseError> {
        loop {
            let next = match tokenizer.next() {
                Some(t) => t,
                None => {
                    return ParseFault::EndedWhileExpecting(self.err_type_expecting())
                        .into_err(tokenizer.position - 1)
                        .into();
                }
            };
            let source_index = next.pos();
            match next.inner {
                RawToken::Identifier(ident) => self.parameter_types.push(
                    Type::try_from(ident.name.as_str()).map_err(|e| e.into_err(source_index))?,
                ),
                RawToken::Key(Key::ListOpen) => self
                    .parameter_types
                    .push(Type::List(Box::new(self.parse_list_type(tokenizer)?))),
                RawToken::Key(Key::ParenClose) => {
                    if self.parameter_types.len() == 1 {
                        self.returns = self.parameter_types.pop().unwrap();
                        assert_eq!(self.parameter_types.len(), self.parameter_names.len());
                        return Ok(self);
                    }
                }
                RawToken::Key(Key::ParenOpen) => self
                    .parameter_types
                    .push(Type::Function(Box::new(self.parse_param_type(tokenizer)?))),
                RawToken::Key(Key::Arrow) => return self.with_return(tokenizer),
                _ => {
                    return ParseFault::GotButExpected(next.inner, self.err_type_expecting())
                        .into_err(source_index)
                        .into()
                }
            }
        }
    }

    fn parse_list_type<I: Iterator<Item = char>>(
        &self,
        tokenizer: &mut Tokenizer<I>,
    ) -> Result<Type, ParseError> {
        let next = match tokenizer.next() {
            None => {
                return ParseFault::Unmatched(Key::ParenOpen)
                    .into_err(tokenizer.position - 1)
                    .into()
            }
            Some(t) => t,
        };
        let source_index = next.pos();
        let r#type = match next.inner {
            RawToken::Key(Key::ListOpen) => Type::List(Box::new(self.parse_list_type(tokenizer)?)),
            RawToken::Identifier(ident) => {
                Type::try_from(ident.name.as_str()).map_err(|e| e.into_err(source_index))?
            }
            _ => {
                return ParseFault::Unmatched(Key::ParenOpen)
                    .into_err(source_index)
                    .into()
            }
        };
        if let Some(t) = tokenizer.next() {
            let source_index = t.pos();
            match t.inner {
                RawToken::Key(Key::ListClose) => Ok(r#type),
                _ => ParseFault::GotButExpected(t.inner, vec!["]".into()])
                    .into_err(source_index)
                    .into(),
            }
        } else {
            ParseFault::Unmatched(Key::ListOpen)
                .into_err(source_index)
                .into()
        }
    }

    fn parse_param_type<I: Iterator<Item = char>>(
        &self,
        tokenizer: &mut Tokenizer<I>,
    ) -> Result<(Vec<Type>, Type), ParseError> {
        let mut buf = Vec::new();
        loop {
            let next = match tokenizer.next() {
                Some(t) => t,
                None => {
                    return ParseFault::EndedWhileExpecting(self.err_type_expecting())
                        .into_err(tokenizer.position - 1)
                        .into();
                }
            };
            let source_index = next.pos();
            match next.inner {
                RawToken::Identifier(ident) => buf.push(
                    Type::try_from(ident.name.as_str()).map_err(|e| e.into_err(source_index))?,
                ),
                RawToken::Key(Key::ListOpen) => {
                    buf.push(Type::List(Box::new(self.parse_list_type(tokenizer)?)))
                }
                RawToken::Key(Key::ParenOpen) => {
                    buf.push(Type::Function(Box::new(self.parse_param_type(tokenizer)?)))
                }
                RawToken::Key(Key::Arrow) => {
                    let returns = (buf, self.parse_return_type(tokenizer)?);
                    return Ok(returns);
                }
                RawToken::Key(Key::ParenClose) => {
                    if buf.len() > 1 {
                        panic!("ET: Malformed parameter type");
                    }
                    match buf.pop() {
                        Some(returns) => return Ok((buf, returns)),
                        None => panic!("ET: Empty parameter type (no return type is not allowed)"),
                    }
                }
                _ => {
                    return ParseFault::GotButExpected(next.inner, self.err_type_expecting())
                        .into_err(source_index)
                        .into();
                }
            }
        }
    }

    fn parse_return_type<I: Iterator<Item = char>>(
        &self,
        tokenizer: &mut Tokenizer<I>,
    ) -> Result<Type, ParseError> {
        let next = match tokenizer.next() {
            Some(t) => t,
            None => {
                return ParseFault::EndedWhileExpecting(vec!["return type".into()])
                    .into_err(tokenizer.position - 1)
                    .into()
            }
        };
        let source_index = next.pos();
        let r#type = match next.inner {
            RawToken::Identifier(ident) => {
                Type::try_from(ident.name.as_str()).map_err(|e| e.into_err(source_index))?
            }
            RawToken::Key(Key::ParenOpen) => {
                Type::Function(Box::new(self.parse_param_type(tokenizer)?))
            }
            RawToken::Key(Key::ListOpen) => Type::List(Box::new(self.parse_list_type(tokenizer)?)),
            _ => {
                return ParseFault::GotButExpected(next.inner, vec!["return type".into()])
                    .into_err(source_index)
                    .into()
            }
        };
        let after = match tokenizer.next() {
            None => {
                return ParseFault::EndedWhileExpecting(vec![")".into()])
                    .into_err(source_index)
                    .into()
            }
            Some(t) => t,
        };
        let source_index = after.pos();
        match after.inner {
            RawToken::Key(Key::ParenClose) => Ok(r#type),
            _ => ParseFault::GotButExpected(after.inner, vec![")".into()])
                .into_err(source_index)
                .into(),
        }
    }

    fn with_return<I: Iterator<Item = char>>(
        mut self,
        tokenizer: &mut Tokenizer<I>,
    ) -> Result<Self, ParseError> {
        self.returns = self.parse_return_type(tokenizer)?;
        Ok(self)
    }

    pub fn parse_body<I: Iterator<Item = char>>(
        &mut self,
        tokenizer: &mut Tokenizer<I>,
    ) -> Result<(), ParseError> {
        self.body = AstBuilder::new(tokenizer).run_chunk()?;
        while let Some(t) = tokenizer.peek() {
            match &t.inner {
                RawToken::Header(_) => break,
                RawToken::NewLine => {
                    tokenizer.next();
                    continue;
                }
                RawToken::Key(Key::Where) => {
                    tokenizer.next();
                    return self.parse_where(tokenizer);
                }
                other => return Err(ParseFault::Unexpected(other.clone()).into_err(t.pos())),
            }
        }
        Ok(())
    }

    fn parse_where<I: Iterator<Item = char>>(
        &mut self,
        tokenizer: &mut Tokenizer<I>,
    ) -> Result<(), ParseError> {
        tokenizer.skip_spaces_and_newlines();
        // Having a bar here is optional
        if let Some(RawToken::Key(Key::Bar)) = tokenizer.peek().map(|t| &t.inner) {
            tokenizer.next();
        }

        let (first, pos) = match tokenizer.next() {
            None => panic!("ET"),
            Some(f) => f.sep(),
        };
        let name = if let RawToken::Identifier(ident) = first {
            ident
        } else {
            return Err(
                ParseFault::GotButExpected(first, vec!["where identifier".into()]).into_err(pos),
            );
        };

        match tokenizer.next() {
            None => return Err(ParseFault::EndedWhileExpecting(vec!["=".into()]).into_err(pos)),
            Some(after) => {
                if match after.inner {
                    RawToken::Identifier(ident) => ident.name != "=",
                    _ => true,
                } {
                    unimplemented!()
                }
            }
        }

        let mut builder = ast::AstBuilder::new(tokenizer);
        let entity = builder.run_chunk()?;
        self.wheres.push((name.name, entity));

        tokenizer.skip_spaces_and_newlines();
        if let Some(RawToken::Key(Key::Bar)) = tokenizer.peek().map(|t| &t.inner) {
            return self.parse_where(tokenizer);
        }

        Ok(())
    }

    pub fn get_where(&self, ident: &str) -> Option<usize> {
        for (i, (s, _)) in self.wheres.iter().enumerate() {
            if ident == s {
                return Some(i);
            }
        }
        None
    }
    pub fn get_where_from_ident(&self, ident: &[String]) -> Option<usize> {
        if ident.len() == 1 {
            return self.get_where(&ident[0]);
        }
        None
    }
    pub fn get_parameter(&self, ident: &str) -> Option<usize> {
        for (i, n) in self.parameter_names.iter().enumerate() {
            if n == ident {
                return Some(i);
            }
        }
        None
    }
    pub fn get_parameter_from_ident(&self, ident: &[String]) -> Option<usize> {
        if ident.len() == 1 {
            return self.get_parameter(&ident[0]);
        }
        None
    }
    pub fn get_parameter_type(&self, pid: usize) -> &Type {
        &self.parameter_types[pid]
    }

    pub fn number_of_generics(&self) -> usize {
        self.parameter_types
            .iter()
            .filter(|t| {
                if let Type::Generic(_) = t {
                    true
                } else {
                    false
                }
            })
            .count()
    }
}

impl fmt::Debug for FunctionBuilder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let types = self
            .parameter_types
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>();
        let annotation = if types.is_empty() {
            format!("{}", self.returns)
        } else {
            format!("{} -> {}", types.join(" "), self.returns)
        };
        let where_statements = self
            .wheres
            .iter()
            .map(|(name, entry)| format!("where {}: {:?}", name, entry))
            .collect::<Vec<String>>()
            .join("\n  ");
        write!(
            f,
            "fn {} {} ({})\n{}",
            self.name,
            self.parameter_names.join(" "),
            annotation,
            // self.body,
            where_statements,
        )
    }
}
