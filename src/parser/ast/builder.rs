use super::{Callable, Entity, Passable};
use crate::parser::tokenizer::TokenSource;
use crate::parser::{
    Identifier, IdentifierType, Key, ParseError, ParseFault, RawToken, Tokenizer, Tracked,
};
use std::convert::TryInto;

pub struct AstBuilder<'a, I: Iterator<Item = char>> {
    tokenizer: &'a mut Tokenizer<I>,
}

impl<'a, I: Iterator<Item = char>> AstBuilder<'a, I> {
    pub fn new(tokenizer: &'a mut Tokenizer<I>) -> Self {
        Self { tokenizer }
    }
}

macro_rules! assume {
    ($path:path, $v:expr) => {
        if let Some(($path(a), pos)) = $v.map(|a| a.sep()) {
            (a, pos)
        } else {
            unreachable!()
        }
    };
}

impl<I: Iterator<Item = char>> AstBuilder<'_, I> {
    // We run this on entrypoints. Such as the beginning of a function or the inbetweens of a (...)
    pub fn run_chunk(&mut self) -> Result<Tracked<Entity>, ParseError> {
        let t = match self.tokenizer.peek() {
            Some(t) => t,
            None => return Err(ParseFault::EmptyParen.to_err(0)),
        };
        match t.inner {
            RawToken::Header(_) => Err(ParseFault::EmptyParen.to_err(t.pos())),
            RawToken::Key(Key::ParenOpen) => {
                let paren_pos = t.pos();
                self.tokenizer.next();
                let v = self.run_chunk().map_err(|e| e.fallback(paren_pos))?;
                let after = self.tokenizer.next();
                match after.map(|a| a.inner) {
                    Some(RawToken::Key(Key::ParenClose)) => {
                        let pos = v.pos();

                        // Edge case where lambdas can have parameters while being surounded by ()
                        if let Entity::Lambda(params, body) = v.inner {
                            self.run_maybe_parameterized(
                                Tracked::new(Callable::Lambda(params, body)).set(pos),
                            )
                        } else {
                            self.run_maybe_operator(v)
                        }
                    }
                    _ => Err(ParseFault::Unmatched(Key::ParenOpen).to_err(paren_pos)),
                }
            }
            RawToken::Key(Key::PrimitiveUnimplemented) => {
                let pos = t.pos();
                self.tokenizer.next();
                Ok(Tracked::new(Entity::Unimplemented).set(pos))
            }
            RawToken::Inlined(_) => {
                let t = self.tokenizer.next();
                let (v, pos) = assume!(RawToken::Inlined, t);
                self.run_maybe_operator(Tracked::new(Entity::Inlined(v)).set(pos))
            }
            RawToken::Key(Key::ListOpen) => {
                let pos = t.pos();
                self.tokenizer.next();
                let v = self.run_list().map_err(|e| e.fallback(pos))?;
                self.run_maybe_operator(Tracked::new(v).set(pos))
            }
            RawToken::Key(Key::If) => {
                let pos = t.pos();
                self.tokenizer.next();
                let v = self.run_if_expression().map_err(|e| e.fallback(pos))?;
                self.run_maybe_operator(Tracked::new(v).set(pos))
            }
            RawToken::Key(Key::First) => {
                let pos = t.pos();
                self.tokenizer.next();
                let v = self.run_first_statement().map_err(|e| e.fallback(pos))?;
                self.run_maybe_operator(Tracked::new(v).set(pos))
            }
            RawToken::Identifier(_) => {
                let (mut ident, pos) = assume!(RawToken::Identifier, self.tokenizer.next());
                let callable = if ident.path.first().map(|s| s.as_str()) == Some("builtin") {
                    ident.path.remove(0);
                    Callable::Builtin(ident)
                } else {
                    Callable::Func(ident)
                };
                let v = self
                    .run_maybe_parameterized(Tracked::new(callable).set(pos))
                    .map_err(|e| e.fallback(pos))?;
                self.run_maybe_operator(v)
            }
            RawToken::Key(Key::Lambda) => {
                let pos = t.pos();
                self.tokenizer.next();
                let v = self.run_lambda().map_err(|e| e.fallback(pos))?;

                if self.lambda_should_consume_pipe() {
                    self.tokenizer.next();
                    let piped_param = self.run_chunk()?;
                    // This is a bit hacky

                    let pos = v.pos();
                    if let Entity::Lambda(params, body) = v.inner {
                        return Ok(Tracked::new(Entity::Call(
                            Callable::Lambda(params, body),
                            vec![piped_param],
                        ))
                        .set(pos));
                    } else {
                        unreachable!();
                    }
                }
                Ok(v)
            }
            RawToken::NewLine => {
                self.tokenizer.next();
                self.run_chunk()
            }
            _ => {
                let t = self.tokenizer.next().unwrap();
                let pos = t.pos();
                Err(ParseFault::Unexpected(t.inner).to_err(pos))
            }
        }
    }

    // edge-case where lambdas don't require () to take pipe after as parameter
    fn lambda_should_consume_pipe(&mut self) -> bool {
        match self.tokenizer.peek().map(|t| &t.inner) {
            Some(RawToken::Key(Key::Pipe)) => true,
            Some(RawToken::NewLine) => {
                self.tokenizer.next();
                self.lambda_should_consume_pipe()
            }
            _ => false,
        }
    }

    fn run_lambda(&mut self) -> Result<Tracked<Entity>, ParseError> {
        let mut params = Vec::new();
        let pos = loop {
            match self.tokenizer.next().map(|t| t.sep()) {
                Some((RawToken::Identifier(ident), _pos)) => params.push(ident),
                Some((RawToken::Key(Key::Arrow), pos)) => break pos,
                Some((other, pos)) => {
                    return Err(ParseFault::GotButExpected(
                        other,
                        vec![
                            RawToken::Identifier("lambda parameter".try_into().unwrap()),
                            RawToken::Key(Key::Arrow),
                        ],
                    )
                    .to_err(pos))
                }
                None => {
                    return Err(ParseFault::Unexpected(RawToken::Key(Key::Lambda)).to_err(0));
                }
            }
        };
        let v = self.run_chunk()?;

        Ok(Tracked::new(Entity::Lambda(params, Box::new(v))).set(pos))
    }

    // We run this when we're looking for parameters
    // TODO: We should probably be using VecDeque here
    fn run_parameterized(&mut self) -> Result<Vec<Tracked<Entity>>, ParseError> {
        let t = match self.tokenizer.peek() {
            Some(t) => t,
            None => return Ok(Vec::new()),
        };
        match &t.inner {
            RawToken::Inlined(_) => {
                let (inlinable, pos) = assume!(RawToken::Inlined, self.tokenizer.next());
                let v = Tracked::new(Entity::Inlined(inlinable)).set(pos);
                let mut params = self.run_parameterized()?;
                params.insert(0, v);
                Ok(params)
            }
            RawToken::Identifier(ident) => {
                if let IdentifierType::Operator = ident.kind {
                    return Ok(Vec::new());
                }
                let (ident, pos) = assume!(RawToken::Identifier, self.tokenizer.next());
                let v = Tracked::new(Entity::SingleIdent(ident)).set(pos);
                let mut params = self.run_parameterized()?;
                params.insert(0, v);
                Ok(params)
            }
            RawToken::Key(Key::ParenOpen) => {
                self.tokenizer.next();
                let v = self.run_chunk()?;
                match self.tokenizer.next().map(|a| a.sep()) {
                    Some((RawToken::Key(Key::ParenClose), _pos)) => {
                        let mut params = self.run_parameterized()?;
                        params.insert(0, v);
                        Ok(params)
                    }
                    Some((other, pos)) => Err(ParseFault::GotButExpected(
                        other,
                        vec![RawToken::Key(Key::ParenClose)],
                    )
                    .to_err(pos)),
                    None => Err(ParseFault::Unmatched(Key::ParenOpen).to_err(0)),
                }
            }
            RawToken::Key(Key::Pipe) => {
                self.tokenizer.next();
                let v = self.run_chunk()?;
                Ok(vec![v])
            }
            RawToken::Key(Key::ListOpen) => {
                let pos = t.pos();
                self.tokenizer.next();
                let v = self.run_list()?;
                let mut params = self.run_parameterized()?;
                params.insert(0, Tracked::new(v).set(pos));
                Ok(params)
            }
            RawToken::Key(Key::ClosureMarker) => {
                self.tokenizer.next();
                let (v, pos) = self.run_closure_conversion()?.sep();
                let mut params = self.run_parameterized()?;
                params.insert(0, Tracked::new(Entity::Pass(v)).set(pos));
                Ok(params)
            }
            RawToken::Header(_)
            | RawToken::Key(Key::ParenClose)
            | RawToken::Key(Key::Then)
            | RawToken::Key(Key::Else)
            | RawToken::Key(Key::ListClose)
            | RawToken::Key(Key::And)
            | RawToken::Key(Key::Elif) => Ok(Vec::new()),
            RawToken::NewLine => {
                self.tokenizer.next();
                self.run_parameterized()
            }
            _ => Err(ParseFault::UnexpectedWantedParameter(t.inner.clone()).to_err(t.pos())),
        }
    }

    fn run_closure_conversion(&mut self) -> Result<Tracked<Passable>, ParseError> {
        let (inner, pos) = match self.tokenizer.next() {
            Some(t) => t.sep(),
            None => {
                return Err(ParseFault::Unexpected(RawToken::Key(Key::ClosureMarker)).to_err(0));
            }
        };
        match inner {
            RawToken::Key(Key::ParenOpen) => {
                let (entity, pos) = self.run_chunk()?.sep();
                match self.tokenizer.next().map(|t| t.sep()) {
                    Some((RawToken::Key(Key::ParenClose), _)) => {}
                    Some((other, pos)) => {
                        return Err(ParseFault::Unexpected(other).to_err(pos));
                    }
                    None => return Err(ParseFault::Unmatched(Key::ParenOpen).to_err(0)),
                }
                // These are all the entities that are valid to pass as function parameter
                let passable = match entity {
                    Entity::Inlined(inlinable) => Passable::Value(inlinable),
                    Entity::Lambda(identifiers, body) => Passable::Lambda(identifiers, body),
                    Entity::SingleIdent(ident) => Passable::Func(ident),
                    Entity::Call(callable, params) => Passable::PartialFunc(callable, params),
                    _ => return Err(ParseFault::InvalidClosure(entity).to_err(pos)),
                };
                Ok(Tracked::new(passable).set(pos))
            }
            RawToken::Identifier(ident) => {
                // It's just a direct pass of a function
                let v = Tracked::new(Passable::Func(ident)).set(pos);
                Ok(v)
            }
            RawToken::Inlined(inlinable) => Ok(Tracked::new(Passable::Value(inlinable)).set(pos)),
            _ => Err(ParseFault::InvalidClosureT(inner).to_err(pos)),
        }
    }

    // We run this when there *might* be parameters coming to the previous entity.
    fn run_maybe_parameterized(
        &mut self,
        takes: Tracked<Callable>,
    ) -> Result<Tracked<Entity>, ParseError> {
        if self.next_can_be_parameter() {
            let params = self.run_parameterized()?;
            let (takes, pos) = takes.sep();
            let v = Tracked::new(Entity::Call(takes, params)).set(pos);
            self.run_maybe_operator(v)
        } else {
            Ok(takes.clone().swap(takes))
        }
    }

    fn next_can_be_parameter(&mut self) -> bool {
        match self.tokenizer.peek() {
            Some(a) => match &a.inner {
                RawToken::Inlined(_)
                | RawToken::Key(Key::Pipe)
                | RawToken::Key(Key::ParenOpen)
                | RawToken::Key(Key::ClosureMarker)
                | RawToken::Key(Key::ListOpen) => true,
                RawToken::Identifier(ident) => !ident.is_operator(),
                RawToken::NewLine => {
                    self.tokenizer.next();
                    self.next_can_be_parameter()
                }
                _ => false,
            },
            _ => false,
        }
    }

    // We run this when there *might* be an operator coming. If there isn't then we just return the
    // left argument for the nonexistant operator.
    fn run_maybe_operator(&mut self, left: Tracked<Entity>) -> Result<Tracked<Entity>, ParseError> {
        let t = match self.tokenizer.peek() {
            Some(t) => t,
            None => return Ok(left),
        };
        match &t.inner {
            RawToken::Identifier(ident) => {
                if ident.is_operator() {
                    let (ident, pos) = assume!(RawToken::Identifier, self.tokenizer.next());
                    // We don't need to run_maybe_operator here because run_operator already does that
                    self.run_operator(left, Tracked::new(ident).set(pos))
                } else {
                    Err(ParseFault::Unexpected(t.inner.clone()).to_err(t.pos()))
                }
            }
            _ => Ok(left),
        }
    }
    // We run this when we already know that there is an operator, and know which operator it is
    fn run_operator(
        &mut self,
        left: Tracked<Entity>,
        op: Tracked<Identifier>,
    ) -> Result<Tracked<Entity>, ParseError> {
        let right = self.run_chunk()?;
        assert!(op.inner.is_operator());
        let (op, pos) = op.sep();
        let v = Tracked::new(Entity::Call(Callable::Func(op), vec![left, right])).set(pos);
        self.run_maybe_operator(v)
    }

    fn run_if_expression(&mut self) -> Result<Entity, ParseError> {
        let mut branches = Vec::new();
        'outer: loop {
            let cond = self.run_chunk()?;
            '_inner: loop {
                let (after, pos) = match self.tokenizer.next() {
                    Some(v) => v.sep(),
                    None => return Err(ParseFault::IfMissingThen.to_err(0)),
                };
                match after {
                    RawToken::Key(Key::Then) => break '_inner,
                    RawToken::NewLine => continue '_inner,
                    _ => return Err(ParseFault::IfWantedThen(after).to_err(pos)),
                }
            }
            let eval = self.run_chunk()?;
            branches.push((cond, eval));

            'inner: loop {
                let (after, pos) = match self.tokenizer.next() {
                    None => return Err(ParseFault::IfMissingThen.to_err(0)),
                    Some(v) => v.sep(),
                };
                match after {
                    RawToken::Key(Key::Elif) => continue 'outer,
                    RawToken::NewLine => continue 'inner,
                    RawToken::Key(Key::Else) => {
                        let last = self.run_chunk()?;
                        return Ok(Entity::If(branches, Box::new(last)));
                    }
                    _ => {
                        return Err(ParseFault::GotButExpected(
                            after,
                            vec![RawToken::Key(Key::Elif), RawToken::Key(Key::Else)],
                        )
                        .to_err(pos));
                    }
                }
            }
        }
    }

    fn run_first_statement(&mut self) -> Result<Entity, ParseError> {
        let mut branches = Vec::new();
        let mut last = false;
        'outer: loop {
            let v = self.run_chunk()?;
            branches.push(v);
            if last {
                return Ok(Entity::First(branches));
            }

            // This loop is just for retrying on newlines. Hacky? Yes. Might split into smaller
            // function and use recursion instead like everywhere else
            'inner: loop {
                let (after, pos) = match self.tokenizer.next() {
                    Some(t) => t.sep(),
                    None => return Err(ParseFault::FirstMissingThen.to_err(0)),
                };
                match after {
                    RawToken::Key(Key::And) => continue 'outer,
                    RawToken::NewLine => continue 'inner,
                    RawToken::Key(Key::Then) => {
                        last = true;
                        break 'inner;
                    }
                    _ => return Err(ParseFault::FirstWantedThen(after).to_err(pos)),
                }
            }
        }
    }

    fn run_list(&mut self) -> Result<Entity, ParseError> {
        // forever loop while `next() == ,` then on `== ]` return. On other then error
        let mut buf = Vec::new();

        // edge-case for empty lists
        if let Some(t) = self.tokenizer.peek() {
            if t.inner == RawToken::Key(Key::ListClose) {
                self.tokenizer.next();
                return Ok(Entity::List(buf));
            }
        }

        loop {
            let v = self.run_chunk()?;
            buf.push(v);
            match self.tokenizer.next().map(|t| t.sep()) {
                Some((RawToken::Key(Key::ListClose), _)) => {
                    let v = Entity::List(buf);
                    return Ok(v);
                }
                Some((RawToken::Key(Key::Comma), _)) => continue,
                Some((other, pos)) => {
                    return Err(ParseFault::GotButExpected(
                        other,
                        vec![RawToken::Key(Key::Comma), RawToken::Key(Key::ListClose)],
                    )
                    .to_err(pos))
                }
                None => return Err(ParseFault::Unmatched(Key::ListOpen).to_err(0)),
            }
        }
    }
}
