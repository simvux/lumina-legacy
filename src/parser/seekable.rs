use super::{Identifier, MaybeType, ParseFault, Parser, Type, PRELUDE_FID};
use std::collections::HashMap;

pub trait Seekable {
    fn seek(&self, parser: &Parser) -> Result<(usize, usize), ParseFault>;
}

// TODO: We can infer here directly because of our new refcell strat
impl Seekable for (usize, &Identifier, &[MaybeType]) {
    fn seek(&self, parser: &Parser) -> Result<(usize, usize), ParseFault> {
        let (self_fid, ident, params) = (self.0, self.1, self.2);
        let fid = {
            match ident.path.len() {
                0 => self_fid,
                1 => {
                    let mod_name = &ident.path[0];
                    parser.modules[self_fid].get_import(mod_name)?
                }
                _ => return Err(ParseFault::InvalidPath(ident.path.clone())),
            }
        };

        let find = |fid, variants: &HashMap<Vec<Type>, usize>| {
            let mut matches = Vec::new();

            'list: for (want_types, funcid) in variants.iter() {
                let mut generics = HashMap::new();
                let mut gen_amount = 0;

                for (i, want_type) in want_types.iter().enumerate() {
                    if let Type::Generic(n) = want_type {
                        match &params[i] {
                            MaybeType::Known(known) => match generics.get(n) {
                                Some(generic_type) => {
                                    // This generic is already assigned to a type, and our given
                                    // type is statically known. So lets check if those
                                    // two match up, if not then this isn't a valid match so then
                                    // skip this variant.
                                    if generic_type != known {
                                        continue 'list;
                                    }
                                }
                                None => {
                                    // We know the type this generic needs to infer our statically known given type!
                                    // Lets insert it and continue
                                    generics.insert(n, known.clone());
                                }
                            },
                            MaybeType::Infer(might_know) => match might_know.borrow().as_ref() {
                                Some(known) => match generics.get(n) {
                                    Some(want_known) => {
                                        // This generic is already assigned to a type, and our given
                                        // type is already infered to be `known`. So lets check if those
                                        // two match up, if not then this isn't a valid match so then
                                        // skip this variant.
                                        if known != want_known {
                                            continue 'list;
                                        }
                                    }
                                    None => {
                                        // We know the type this generic needs to infer our already infered given type!
                                        // Lets insert it and continue
                                        generics.insert(n, known.clone());
                                    }
                                },
                                None => {
                                    // TODO: Should i throw "cannot infer" error?
                                    // I probably shouldn't but if no other variant is found I should
                                    // probably hint that an annotation could make this possible.
                                    continue 'list;
                                }
                            },
                        }
                        gen_amount += 1;
                    } else {
                        // The wanted type of this parameter is statically known
                        match &params[i] {
                            MaybeType::Known(t) => {
                                if t != want_type {
                                    continue 'list;
                                }
                            }
                            MaybeType::Infer(maybe_known) => match maybe_known.borrow().as_ref() {
                                Some(t) => {
                                    if t != want_type {
                                        continue 'list;
                                    }
                                }
                                None => {
                                    // TODO: This is still a match. Lets add it to valid matches but! We
                                    // need to somehow mark this as *to be infered*. Or do we? We
                                    // can just check the valid matches later and infer based of
                                    // the top one. There's an edge-case where there can be
                                    // multiple equally valid matches but that can be fixed later
                                    // with an *cannot infer* error.
                                }
                            },
                        }
                    }
                }
                // All types passed, so lets accept this variant
                matches.push((fid, *funcid, gen_amount));
            }
            matches
        };

        let variants = match parser.modules[fid].function_ids.get(ident) {
            Some(variants) => variants,
            None => {
                // Wasn't find, so lets try prelude variants
                if self_fid == fid && self_fid != PRELUDE_FID {
                    if let Some(variants) = parser
                        .modules
                        .get(PRELUDE_FID)
                        .and_then(|m| m.function_ids.get(ident))
                    {
                        let mut prelude_matches = find(PRELUDE_FID, variants);
                        if prelude_matches.is_empty() {
                            return Err(ParseFault::FunctionVariantNotFound(
                                ident.clone(),
                                params.to_vec(),
                                PRELUDE_FID,
                            ));
                        } else {
                            prelude_matches.sort_by(|i, j| i.2.cmp(&j.2));
                            let (fid, funcid) = (prelude_matches[0].0, prelude_matches[0].1);
                            parser.infer_types_for(params, fid, funcid);
                            return Ok((fid, funcid));
                        }
                    }
                };
                return Err(ParseFault::FunctionNotFound(ident.clone(), fid));
            }
        };
        /*
         * TODO: Might want to somehow renable this for performance reasons sometime
        // Is there an exact match?
        if let Some(funcid) = variants.get(params).cloned() {
            return Ok((fid, funcid));
        }
        */

        let mut matches = find(fid, variants);
        if self_fid == fid && self_fid != PRELUDE_FID {
            if let Some(variants) = parser
                .modules
                .get(PRELUDE_FID)
                .and_then(|m| m.function_ids.get(ident))
            {
                matches.append(&mut find(PRELUDE_FID, variants));
            }
        }
        if matches.is_empty() {
            return Err(ParseFault::FunctionVariantNotFound(
                ident.clone(),
                params.to_vec(),
                fid,
            ));
        }

        matches.sort_by(|i, j| i.2.cmp(&j.2));

        // TODO:
        // Instead of just returning the top match, I need to infer the posible Rc's
        // I also need this to not only short by amount of generics, but also sort by least
        // complexity of infers? I think? Or maybe I don't. If there's multiple equally matching
        // when not taking infers into account then I'll probably need to throw a conflict error of
        // sorts. Although we're pulling straws here, lets just get this working before delving
        // into such edge-cases.
        let (fid, funcid) = (matches[0].0, matches[0].1);
        parser.infer_types_for(params, fid, funcid);
        Ok((fid, funcid))
    }
}
impl Seekable for (usize, &str, Identifier, &[MaybeType]) {
    fn seek(&self, parser: &Parser) -> Result<(usize, usize), ParseFault> {
        let fid = parser.modules[self.0]
            .imports
            .get(self.1)
            .copied()
            .ok_or_else(|| ParseFault::Internal)?;
        Seekable::seek(&(fid, &self.2, self.3), parser)
    }
}

impl Parser {
    pub fn find_func<S: Seekable + std::fmt::Debug>(
        &self,
        source: S,
    ) -> Result<(usize, usize), ParseFault> {
        source.seek(self)
    }

    fn infer_types_for(&self, params: &[MaybeType], fid: usize, funcid: usize) {
        let actual_types = &self.modules[fid].functions[funcid].parameter_types;
        for (i, p) in params.iter().enumerate() {
            if let MaybeType::Infer(t) = p {
                *t.borrow_mut() = Some(actual_types[i].clone());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{FileSource, FunctionBuilder, Parser, Tracked};
    use crate::Environment;
    use std::convert::TryFrom;
    use std::rc::Rc;

    fn test_data() -> Parser {
        let mut parser = Parser::new(Rc::new(Environment::from(".".into())));
        let fid = parser.new_module(FileSource::Project(Vec::new()));
        parser.new_function(
            fid,
            FunctionBuilder {
                name: Identifier::try_from("simple").unwrap(),
                parameter_names: vec!["x".into(), "y".into()],
                parameter_types: vec![Type::Int, Type::Int],
                returns: Type::Int,
                body: Tracked::default(),
                wheres: Vec::new(),
            },
        );
        parser.new_function(
            fid,
            FunctionBuilder {
                name: Identifier::try_from("generic").unwrap(),
                parameter_names: vec!["x".into(), "y".into()],
                parameter_types: vec![
                    Type::Generic(0),
                    Type::Generic(0),
                    Type::Generic(1),
                    Type::Int,
                ],
                returns: Type::Int,
                body: Tracked::default(),
                wheres: Vec::new(),
            },
        );
        parser.new_function(
            fid,
            FunctionBuilder {
                name: Identifier::try_from("generic").unwrap(),
                parameter_names: vec!["x".into(), "y".into()],
                parameter_types: vec![Type::Generic(0), Type::Int, Type::Generic(1), Type::Int],
                returns: Type::Int,
                body: Tracked::default(),
                wheres: Vec::new(),
            },
        );
        parser
    }

    #[test]
    fn simple() {
        let parser = test_data();
        let params: &[MaybeType] = &[MaybeType::Known(Type::Int), MaybeType::Known(Type::Int)];
        parser
            .find_func((0, &Identifier::try_from("simple").unwrap(), params))
            .unwrap();
    }

    #[test]
    fn generic() {
        let parser = test_data();
        let params: &[MaybeType] = &[
            MaybeType::Known(Type::Int),
            MaybeType::Known(Type::Int),
            MaybeType::Known(Type::Float),
            MaybeType::Known(Type::Int),
        ];
        let (_, funcid) = parser
            .find_func((0, &Identifier::try_from("generic").unwrap(), params))
            .unwrap();
        assert_eq!(funcid, 2);
    }
}
