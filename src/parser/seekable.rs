use super::{
    ast, FunctionBuilder, Identifier, MaybeType, ParseFault, Parser, Tracked, Type, PRELUDE_FID,
};
use std::collections::HashMap;

pub trait Seekable<'a> {
    fn seek(&self, parser: &'a Parser)
        -> Result<(&'a Tracked<ast::Entity>, ast::Meta), ParseFault>;
}

fn deep_cmp(
    generics: &mut HashMap<u8, Type>,
    gen_amount: &mut usize,
    want: &Type,
    got: &MaybeType,
) -> bool {
    match want {
        Type::List(box want_list) => {
            return match got {
                MaybeType::Known(Type::List(box got_list)) => deep_cmp(
                    generics,
                    gen_amount,
                    want_list,
                    &MaybeType::Known(got_list.clone()),
                ),
                _ => false,
            };
            // return deep_cmp(generics, gen_amount, list, );
        }
        Type::Function(box (want_params, want_returns)) => {
            return match got {
                MaybeType::Known(Type::Function(box (got_params, got_returns))) => {
                    for (i, want) in want_params.iter().enumerate() {
                        if !deep_cmp(
                            generics,
                            gen_amount,
                            want,
                            &MaybeType::Known(got_params[i].clone()),
                        ) {
                            return false;
                        }
                    }
                    deep_cmp(
                        generics,
                        gen_amount,
                        want_returns,
                        &MaybeType::Known(got_returns.clone()),
                    )
                }
                _ => false,
            }
        }
        Type::Generic(n) => {
            match got {
                MaybeType::Known(known) => match generics.get(&n) {
                    Some(generic_type) => {
                        // This generic is already assigned to a type, and our given
                        // type is statically known. So lets check if those
                        // two match up, if not then this isn't a valid match so then
                        // skip this variant.
                        if generic_type != known {
                            return false;
                        }
                    }
                    None => {
                        // We know the type this generic needs to infer our statically known given type!
                        // Lets insert it and continue
                        generics.insert(*n, known.clone());
                    }
                },
                MaybeType::Infer(might_know) => match might_know.borrow().as_ref() {
                    Some(known) => match generics.get(&n) {
                        Some(want_known) => {
                            // This generic is already assigned to a type, and our given type is already infered to be `known`. So lets check if those
                            // two match up, if not then this isn't a valid match so then
                            // skip this variant.
                            if known != want_known {
                                return false;
                            }
                        }
                        None => {
                            // We know the type this generic needs to infer our already infered given type!
                            // Lets insert it and continue
                            generics.insert(*n, known.clone());
                        }
                    },
                    None => {
                        // TODO: Should i throw "cannot infer" error?
                        // I probably shouldn't but if no other variant is found I should
                        // probably hint that an annotation could make this possible.
                        return false;
                    }
                },
            }
            *gen_amount += 1;
        }
        _ => {
            // The wanted type of this parameter is statically known
            match got {
                MaybeType::Known(t) => {
                    if t != want {
                        return false;
                    }
                }
                MaybeType::Infer(maybe_known) => match maybe_known.borrow().as_ref() {
                    Some(t) => {
                        if t != want {
                            return false;
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
    true
}

impl<'a> Seekable<'a> for (usize, &Identifier, &[MaybeType]) {
    fn seek(
        &self,
        parser: &'a Parser,
    ) -> Result<(&'a Tracked<ast::Entity>, ast::Meta), ParseFault> {
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

                for (i, want) in want_types.iter().enumerate() {
                    if !deep_cmp(&mut generics, &mut gen_amount, want, &params[i]) {
                        continue 'list;
                    }
                }

                // All types passed, so lets accept this variant
                matches.push((fid, *funcid, gen_amount, generics));
            }
            matches
        };

        let identifiers_from = |generics: &HashMap<_, _>, func: &FunctionBuilder| {
            let mut identifiers = HashMap::with_capacity(func.parameter_types.len());
            for (i, (n, t)) in func
                .parameter_names
                .iter()
                .cloned()
                .zip(func.parameter_types.iter().cloned())
                .enumerate()
            {
                let identmeta = ast::IdentMeta {
                    r#type: MaybeType::Known(t.decoded(&generics)),
                    use_counter: 0,
                    ident: ast::Identifiable::Param(i),
                };
                identifiers.insert(n, identmeta);
            }
            identifiers
        };

        dbg!(&parser.modules[fid], &parser.modules[fid].function_ids);
        dbg!(&ident);
        let variants = match parser.modules[fid].function_ids.get(&ident.name) {
            Some(variants) => variants,
            None => {
                // Wasn't find, so lets try prelude variants
                if self_fid == fid && self_fid != PRELUDE_FID {
                    if let Some(variants) = parser
                        .modules
                        .get(PRELUDE_FID)
                        .and_then(|m| m.function_ids.get(&ident.name))
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
                            let (fid, funcid, generics) = (
                                prelude_matches[0].0,
                                prelude_matches[0].1,
                                &prelude_matches[0].3,
                            );
                            parser.infer_types_for(params, fid, funcid);
                            let func = &parser.modules[fid].functions[funcid];
                            let meta = ast::Meta {
                                fid,
                                ident: func.name.clone(),
                                return_type: func.returns.clone().decoded(&generics),
                                identifiers: identifiers_from(&generics, func),
                            };
                            dbg!(&meta);

                            return Ok((&func.body, meta));
                        }
                    }
                };
                return Err(ParseFault::FunctionNotFound(ident.clone(), fid));
            }
        };

        let mut matches = find(fid, variants);
        if self_fid == fid && self_fid != PRELUDE_FID {
            if let Some(variants) = parser
                .modules
                .get(PRELUDE_FID)
                .and_then(|m| m.function_ids.get(&ident.name))
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

        // Sort by least amount of generics
        matches.sort_by(|i, j| i.2.cmp(&j.2));

        let (fid, funcid, generics) = (matches[0].0, matches[0].1, &matches[0].3);
        parser.infer_types_for(params, fid, funcid);
        let func = &parser.modules[fid].functions[funcid];
        let meta = ast::Meta {
            fid,
            ident: func.name.clone(),
            return_type: func.returns.clone().decoded(&generics),
            identifiers: identifiers_from(generics, func),
        };
        dbg!(&meta);
        Ok((&func.body, meta))
    }
}
impl<'a> Seekable<'a> for (usize, &str, Identifier, &[MaybeType]) {
    fn seek(
        &self,
        parser: &'a Parser,
    ) -> Result<(&'a Tracked<ast::Entity>, ast::Meta), ParseFault> {
        let fid = parser.modules[self.0]
            .imports
            .get(self.1)
            .copied()
            .ok_or_else(|| ParseFault::Internal)?;
        Seekable::seek(&(fid, &self.2, self.3), parser)
    }
}

impl Parser {
    pub fn find_func<'a, S: Seekable<'a> + std::fmt::Debug>(
        &'a self,
        source: S,
    ) -> Result<(&'a Tracked<ast::Entity>, ast::Meta), ParseFault> {
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

// TODO: Fix these tests (broke after seek() -> Meta refactor)
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
        let _meta = parser
            .find_func((0, &Identifier::try_from("generic").unwrap(), params))
            .unwrap();
        // assert_eq!(funcid, 2);
    }
}
