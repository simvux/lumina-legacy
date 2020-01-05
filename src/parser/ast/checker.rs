use super::IrBuilder;
use crate::ir;
use crate::parser::{ast, Identifier, Inlinable, MaybeType, ParseError, ParseFault, Tracked, Type};

use super::{Identifiable, Meta};

impl<'a> IrBuilder {
    pub fn find_and_build_function(
        &'a self,
        self_fid: usize,
        name: &Identifier<Type>,
        params: &mut Vec<MaybeType>,
    ) -> Result<(Type, usize), ParseError> {
        let (entry, meta) = self
            .parser
            .find_func((self_fid, name, params.as_slice()))
            .map_err(|e| e.into_err(0))?;
        if self.is_completed(&meta) {
            let findex = self.gen_id(&meta);
            return Ok((meta.return_type.clone(), findex));
        }

        self.build_function(meta, entry)
    }

    // If we only want *one* singular variant and error with "annotation required" whenever we get
    // multiple variants.
    pub fn find_and_build_only_suitable(
        &'a self,
        self_fid: usize,
        ident: &Identifier<Type>,
    ) -> Result<(Type, &[Type], usize), ParseError> {
        let (variants, fid) = self
            .parser
            .variants_including_prelude(self_fid, ident)
            .map_err(|e| e.into_err(0))?;
        let funcid = match variants.len() {
            1 => match &ident.anot {
                Some(anot) => variants.get(anot.as_slice()).ok_or_else(|| {
                    ParseFault::FunctionVariantNotFound(
                        ident.clone(),
                        anot.iter()
                            .cloned()
                            .map(MaybeType::Known)
                            .collect::<Vec<_>>(),
                        fid,
                    )
                    .into_err(0)
                })?,
                None => variants.values().next().unwrap(),
            },
            _ => match &ident.anot {
                Some(anot) => variants.get(anot.as_slice()).ok_or_else(|| {
                    ParseFault::FunctionVariantNotFound(
                        ident.clone(),
                        anot.iter()
                            .cloned()
                            .map(MaybeType::Known)
                            .collect::<Vec<_>>(),
                        fid,
                    )
                    .into_err(0)
                })?,
                None => {
                    return Err(ParseFault::FunctionConversionRequiresAnnotation(
                        ident.clone(),
                        variants.clone(),
                    )
                    .into_err(0))
                }
            },
        };
        let func = &self.parser.modules[fid].functions[*funcid];
        let meta = ast::Meta {
            fid,
            ident: func.name.clone(),
            return_type: func.returns.clone(),
            identifiers: func
                .parameter_names
                .iter()
                .cloned()
                .zip(
                    func.parameter_types
                        .iter()
                        .cloned()
                        .enumerate()
                        .map(|(i, t)| ast::IdentMeta {
                            use_counter: 0,
                            r#type: MaybeType::Known(t),
                            ident: Identifiable::Param(i),
                        }),
                )
                .collect(),
        };
        let (t, findex) = self.build_function(meta, &func.body)?;
        Ok((t, &func.parameter_types, findex))
    }

    pub fn build_function(
        &'a self,
        mut meta: Meta,
        entry: &Tracked<ast::Entity>,
    ) -> Result<(Type, usize), ParseError> {
        let findex = self.gen_id(&meta);

        let (t, ir) = self.build(entry, &mut meta)?;
        let t = t.unwrap();
        if meta.return_type != Type::Nothing && t != meta.return_type {
            return Err(ParseFault::FnTypeReturnMismatch(Box::new(meta), t).into_err(entry.pos()));
        }

        self.complete(findex, ir);
        Ok((t, findex))
    }

    fn build(
        &'a self,
        token: &'a Tracked<ast::Entity>,
        meta: &mut Meta,
    ) -> Result<(MaybeType, ir::Entity), ParseError> {
        match &token.inner {
            ast::Entity::Unimplemented => Ok((
                MaybeType::Known(meta.return_type.clone()),
                ir::Entity::Unimplemented,
            )),
            ast::Entity::Inlined(inlined) => {
                let v = inlined.clone();
                let t = match inlined {
                    Inlinable::Int(_) => Type::Int,
                    Inlinable::Float(_) => Type::Float,
                    Inlinable::Bool(_) => Type::Bool,
                    Inlinable::Nothing => Type::Nothing,
                };
                Ok((MaybeType::Known(t), ir::Entity::Inlined(v.into())))
            }
            ast::Entity::Call(call, params) => {
                let mut evaluated_params = Vec::with_capacity(params.len());
                let mut param_types = Vec::with_capacity(params.len());
                for param in params.iter() {
                    let (t, v) = self.build(param, meta)?;
                    param_types.push(t);
                    evaluated_params.push(v);
                }

                match call {
                    ast::Callable::Func(ident) => {
                        match meta.try_use(&ident.name) {
                            Some(identmeta) => match identmeta.ident {
                                Identifiable::Param(id) => {
                                    let r#type = match &identmeta.r#type {
                                        MaybeType::Infer(mt) => mt.borrow().clone().unwrap(),
                                        MaybeType::Known(t) => t.clone(),
                                    };
                                    if let Type::Function(box (takes, gives)) = r#type {
                                        if takes.len() != param_types.len() {
                                            return Err(ParseFault::ParamCallAmountMismatch(
                                                Box::new((takes, gives, param_types)),
                                            )
                                            .into_err(token.pos())
                                            .fallback_fid(meta.fid));
                                        }
                                        if takes.len() != param_types.len() {
                                            return Err(ParseFault::ParamCallAmountMismatch(
                                                Box::new((takes, gives, param_types)),
                                            )
                                            .into_err(token.pos())
                                            .fallback_fid(meta.fid));
                                        }
                                        for (i, take) in takes.iter().enumerate() {
                                            if *take != param_types[i].clone().unwrap() {
                                                return Err(ParseFault::ParamCallMismatch(
                                                    Box::new((takes, gives, param_types)),
                                                )
                                                .into_err(token.pos())
                                                .fallback_fid(meta.fid));
                                            }
                                        }
                                        Ok((
                                            MaybeType::Known(gives.clone()),
                                            ir::Entity::ParameterCall(id as u32, evaluated_params),
                                        ))
                                    } else {
                                        Err(ParseFault::ParamCannotTakeParameters(Box::new((
                                            r#type,
                                            param_types,
                                        )))
                                        .into_err(token.pos())
                                        .fallback_fid(meta.fid))
                                    }
                                }
                                _ => unimplemented!("{:?}", identmeta),
                            },
                            None => {
                                let (t, findex) = self
                                    .find_and_build_function(meta.fid, ident, &mut param_types)
                                    .map_err(|e| {
                                        e.fallback_index(token.pos()).fallback_fid(meta.fid)
                                    })?;
                                Ok((
                                    MaybeType::Known(t),
                                    ir::Entity::FunctionCall(findex as u32, evaluated_params),
                                ))
                            }
                        }
                    }
                    ast::Callable::Builtin(ident) => {
                        let (id, nt) = ir::bridge::get_funcid(&ident.name)
                            .map_err(|e| e.into_err(token.pos()).fallback_fid(meta.fid))?;
                        let mt = match nt {
                            ir::bridge::NaiveType::Known(t) => MaybeType::Known(t),
                            ir::bridge::NaiveType::Matching(i) => param_types[i as usize].clone(),
                            ir::bridge::NaiveType::ListedMatching(i) => MaybeType::Known(
                                Type::List(Box::new(param_types[i as usize].clone().unwrap())),
                            ),
                            ir::bridge::NaiveType::UnlistedMatching(i) => {
                                if let MaybeType::Known(Type::List(box inner)) =
                                    &param_types[i as usize]
                                {
                                    MaybeType::Known(inner.clone())
                                } else {
                                    panic!(
                                        "UnlistedMatching grab wasn't a list, it was a {:?}",
                                        &param_types[i as usize]
                                    );
                                }
                            }
                        };
                        Ok((mt, ir::Entity::RustCall(id, evaluated_params)))
                    }
                    ast::Callable::Lambda(param_names, lambda_token) => {
                        let mut new_meta = meta.clone();
                        new_meta.lambda_swap(param_names, &param_types);

                        let (t, v) = self.build(lambda_token, &mut new_meta)?;
                        // The lambda entity expects the first parameter to be the actual body
                        // itself instead.
                        evaluated_params.insert(0, v);
                        let to_capture = meta.was_used(&new_meta);
                        Ok((t, ir::Entity::Lambda(evaluated_params, to_capture)))
                    }
                }
            }
            ast::Entity::Pass(pass) => match pass {
                // `map #func [1,2,3]`
                // is actually just turned into
                // `map #(\n -> func n) [1,2,3]`
                // the ir optimizer should take care of indirection if that becomes a problem
                ast::Passable::Func(ident) => match meta.try_use(&ident.name) {
                    Some(im) => match im.ident {
                        Identifiable::Param(n) => {
                            Ok((im.r#type.clone(), ir::Entity::Parameter(n as u16)))
                        }
                        _ => unimplemented!(),
                    },
                    None => {
                        let (t, param_types, findex) = self
                            .find_and_build_only_suitable(meta.fid, ident)
                            .map_err(|e| e.fallback_index(token.pos()).fallback_fid(meta.fid))?;
                        Ok((
                            MaybeType::Known(Type::Function(Box::new((param_types.to_vec(), t)))),
                            ir::Entity::LambdaPointer(Box::new((
                                ir::Entity::FunctionCall(
                                    findex as u32,
                                    vec![ir::Entity::Parameter(0)],
                                ),
                                vec![],
                            ))),
                        ))
                    }
                },
                ast::Passable::PartialFunc(ident, pre_given) => {
                    unimplemented!("{:?}", token);
                }
                ast::Passable::Lambda(param_names, lambda_token) => {
                    let mut new_meta = meta.clone();
                    let mut infered_param_types = param_names
                        .iter()
                        .map(|ident| {
                            if let Some(anots) = &ident.anot {
                                if let Some(anot) = anots.get(0) {
                                    MaybeType::Known(anot.clone())
                                } else {
                                    MaybeType::new()
                                }
                            } else {
                                MaybeType::new()
                            }
                        })
                        .collect::<Vec<_>>();
                    new_meta.lambda_swap(param_names, infered_param_types.as_slice());

                    let (t, v) = self.build(lambda_token, &mut new_meta)?;
                    let to_capture = meta.was_used(&new_meta);
                    Ok((
                        MaybeType::Known(Type::Function(Box::new((
                            infered_param_types
                                .drain(0..)
                                .map(|t| t.unwrap())
                                .collect::<Vec<_>>(),
                            t.unwrap(),
                        )))),
                        ir::Entity::LambdaPointer(Box::new((v, to_capture))),
                    ))
                }
                ast::Passable::Value(inlinable) => {
                    unimplemented!("{:?}", token);
                }
            },
            ast::Entity::If(branches, else_do) => self
                .if_expression(branches, else_do, meta)
                .map_err(|e| e.fallback_index(token.pos()).fallback_fid(meta.fid)),
            ast::Entity::First(branches) => self.first_statement(branches, meta),
            ast::Entity::List(branches) => self.list(branches, meta),
            ast::Entity::SingleIdent(ident) => match meta.try_use(&ident.name) {
                Some(found) => match found.ident {
                    Identifiable::Param(id) => {
                        if let MaybeType::Known(Type::Function(box (takes, gives))) = &found.r#type
                        {
                            if !takes.is_empty() {
                                return Err(ParseFault::ParamCallAmountMismatch(Box::new((
                                    takes.clone(),
                                    gives.clone(),
                                    Vec::new(),
                                )))
                                .into_err(token.pos())
                                .fallback_fid(meta.fid));
                            }
                            return Ok((
                                MaybeType::Known(gives.clone()),
                                ir::Entity::ParameterCall(id as u32, Vec::new()),
                            ));
                        }
                        Ok((found.r#type.clone(), ir::Entity::Parameter(id as u16)))
                    }
                    Identifiable::Captured(id) => {
                        Ok((found.r#type.clone(), ir::Entity::Captured(id as u16)))
                    }
                    _ => unimplemented!("{:?}", found),
                },
                None => {
                    // Lets see if it's a constant
                    const NO_PARAMS: &[MaybeType] = &[];
                    let (entry, meta) = self
                        .parser
                        .find_func((meta.fid, ident, NO_PARAMS))
                        .map_err(|e| {
                            if let ParseFault::FunctionNotFound(_, _) = e {
                                ParseFault::IdentifierNotFound(ident.name.clone())
                            } else {
                                e
                            }
                            .into_err(token.pos())
                            .fallback_fid(meta.fid)
                        })?;
                    let (t, findex) = self.build_function(meta, &entry)?;
                    Ok((
                        MaybeType::Known(t),
                        ir::Entity::FunctionCall(findex as u32, Vec::new()),
                    ))
                }
            },
            ast::Entity::Lambda(_, _) => Err(ParseFault::ParameterlessLambda
                .into_err(token.pos())
                .fallback_fid(meta.fid)),
        }
    }

    fn list(
        &'a self,
        branches: &'a [Tracked<ast::Entity>],
        meta: &mut Meta,
    ) -> Result<(MaybeType, ir::Entity), ParseError> {
        let mut expected_t = None;
        let mut buf = Vec::with_capacity(branches.len());
        for (i, branch) in branches.iter().enumerate() {
            let (t, v) = self.build(branch, meta)?;
            let t = t.unwrap();
            match &expected_t {
                None => expected_t = Some(t),
                Some(expected) => {
                    if t != *expected {
                        return Err(ParseFault::ListEntryTypeMismatch(t, expected.clone(), i)
                            .into_err(branch.pos())
                            .fallback_fid(meta.fid));
                    }
                }
            }
            buf.push(v);
        }
        Ok((
            MaybeType::Known(
                expected_t
                    .map(|t| Type::List(Box::new(t)))
                    .unwrap_or_else(|| meta.return_type.clone()),
            ),
            ir::Entity::List(buf),
        ))
    }

    fn if_expression(
        &'a self,
        branches: &'a [(Tracked<ast::Entity>, Tracked<ast::Entity>)],
        else_do: &'a Tracked<ast::Entity>,
        meta: &mut Meta,
    ) -> Result<(MaybeType, ir::Entity), ParseError> {
        let mut buf = Vec::with_capacity((branches.len() * 2) + 1);
        let mut branch_types = Vec::new();
        let (else_t, else_v) = self.build(else_do, meta)?;
        buf.push(else_v);
        for (cond, eval) in branches.iter() {
            let (t, v) = self.build(cond, meta)?;
            let t = t.unwrap();

            // The conditional needs to be a bool
            if t != Type::Bool {
                return Err(
                    ParseFault::IfConditionNotBoolean(Box::new((cond.clone().inner, t)))
                        .into_err(cond.pos())
                        .fallback_fid(meta.fid),
                );
            }
            buf.push(v);
            let (t, v) = self.build(eval, meta)?;
            branch_types.push(t.unwrap());
            buf.push(v);
        }
        branch_types.push(else_t.unwrap());

        // Verify that all the branches are the same type
        if branch_types.iter().all(|t| *t == branch_types[0]) {
            Ok((
                MaybeType::Known(branch_types.remove(0)),
                ir::Entity::IfExpression(ir::If::from(buf)),
            ))
        } else {
            Err(ParseFault::IfBranchTypeMismatch(Box::new((
                branch_types,
                (branches.to_vec(), else_do.clone()),
            )))
            .into_err(0)
            .fallback_fid(meta.fid))
        }
    }

    fn first_statement(
        &'a self,
        branches: &'a [Tracked<ast::Entity>],
        meta: &mut Meta,
    ) -> Result<(MaybeType, ir::Entity), ParseError> {
        let mut buf = Vec::with_capacity(branches.len());
        let mut last_t = Type::Nothing;
        for entity in branches.iter() {
            let (t, v) = self.build(&entity, meta)?;
            last_t = t.unwrap();
            buf.push(v);
        }
        Ok((
            MaybeType::Known(last_t),
            ir::Entity::FirstStatement(ir::First::from(buf)),
        ))
    }
}
