use super::IrBuilder;
use crate::ir;
use crate::parser::{
    ast, r#type::CustomType, Anot, Identifier, MaybeType, ParseError, ParseFault, Tracked, Type,
};

use super::{Identifiable, Meta};

impl<'a> IrBuilder {
    pub fn find_and_build_function(
        &'a self,
        self_fid: usize,
        name: &Anot<Identifier, Type>,
        params: &mut Vec<MaybeType>,
    ) -> Result<(Type, usize), ParseError> {
        let (entry, meta) = self
            .parser
            .find_func_meta(self_fid, name, params.as_slice())
            .map_err(|e| e.into_err(0))?;
        if self.is_completed(&meta) {
            let findex = self.gen_id(&meta);
            return Ok((meta.return_type, findex));
        }

        self.build_function(meta, entry)
    }

    pub fn find_only_suitable(
        &'a self,
        self_fid: usize,
        ident: &Anot<Identifier, Type>,
        atleast_params: usize,
    ) -> Result<(usize, usize), ParseError> {
        let (raw_variants, fid) = self
            .parser
            .variants_including_prelude(self_fid, ident)
            .map_err(|e| e.into_err(0))?;
        let variants = raw_variants
            .iter()
            .filter(|(params, _)| params.len() >= atleast_params)
            .collect::<Vec<_>>();

        let funcid = match ident.anot.len() {
            0 => {
                if variants.len() > 1 {
                    return Err(ParseFault::FunctionConversionRequiresAnnotation(
                        ident.clone(),
                        raw_variants.clone(),
                    )
                    .into_err(0));
                } else {
                    variants.iter().next().map(|(_, funcid)| funcid).unwrap()
                }
            }
            _ => variants
                .iter()
                .find(|(k, _v)| k.as_slice() == ident.anot.as_slice())
                .map(|(_, v)| v)
                .ok_or_else(|| {
                    ParseFault::FunctionVariantNotFound(
                        ident.clone(),
                        ident
                            .anot
                            .iter()
                            .cloned()
                            .map(MaybeType::Known)
                            .collect::<Vec<_>>(),
                        fid,
                    )
                    .into_err(0)
                })?,
        };
        Ok((fid, **funcid))
    }

    pub fn build_function(
        &'a self,
        mut meta: Meta,
        entry: &Tracked<ast::Entity>,
    ) -> Result<(Type, usize), ParseError> {
        let findex = self.gen_id(&meta);

        let (t, ir) = self
            .build(entry, &mut meta)
            .map_err(|e| e.fallback_fid(meta.fid))?;
        let t = t.unwrap();

        let expected = self
            .parser
            .destruct_custom_type(meta.fid, meta.return_type.clone());
        if expected != Type::Nothing && t != expected {
            meta.return_type = expected;
            return Err(ParseFault::FnTypeReturnMismatch(Box::new(meta), t).into_err(entry.pos()));
        }
        println!("{} fi{} -> {}", &meta.ident, findex, &ir);

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
                let t = inlined.into();
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
                    ast::Callable::Func(ident) => match meta.try_use(&ident.inner.name) {
                        Some(identmeta) => {
                            let ir = match identmeta.ident {
                                Identifiable::Param(id) => {
                                    ir::Entity::ParameterCall(id as u32, evaluated_params)
                                }
                                Identifiable::Captured(id) => {
                                    ir::Entity::CapturedCall(id as u32, evaluated_params)
                                }
                                Identifiable::Where((fid, funcid), whereid) => {
                                    let func = &self.parser.modules[fid].functions[funcid];
                                    if let (ast::Entity::Lambda(param_names, body), _where_pos) =
                                        func.wheres[whereid].1.clone().sep()
                                    {
                                        // We're evaluating parameters twice here. Although I
                                        // suppose it's quite a rare edge-case so should we even
                                        // care?
                                        let new_ast = ast::Entity::Call(
                                            ast::Callable::Lambda(param_names, body),
                                            params.to_vec(),
                                        );
                                        let pos = token.pos();
                                        return self.build(&Tracked::new(new_ast).set(pos), meta);
                                    } else {
                                        panic!("ET: This `where` identifier cannot be used as a function");
                                    }
                                }
                            };
                            let (_takes, gives) =
                                destruct_callable_ident(identmeta.r#type.clone(), param_types)
                                    .map_err(|e| e.into_err(token.pos()))?;
                            Ok((MaybeType::Known(gives), ir))
                        }
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
                    },
                    ast::Callable::Builtin(ident) => {
                        let (id, nt) = ir::bridge::get_funcid(&ident.inner.name)
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
            ast::Entity::Pass(pass) => {
                match pass {
                    // `map #func [1,2,3]`
                    // is actually just turned into
                    // `map #(\n -> func n) [1,2,3]`
                    // the ir optimizer should take care of indirection if that becomes a problem
                    ast::Passable::Func(ident) => {
                        let lambda = self.wrap_into_lambda(ident.clone(), Vec::new(), meta)?;
                        self.build(&Tracked::new(lambda).set(token.pos()), meta)
                    }
                    ast::Passable::PartialFunc(callable, pre_given) => match callable {
                        ast::Callable::Func(ident) => {
                            // We turn #(f x) into #(\...p -> f x ...p)
                            let lambda =
                                self.wrap_into_lambda(ident.clone(), pre_given.clone(), meta)?;
                            self.build(&Tracked::new(lambda).set(token.pos()), meta)
                        }
                        _ => unimplemented!(),
                    },
                    ast::Passable::Lambda(param_names, lambda_token) => {
                        let mut new_meta = meta.clone();
                        let mut infered_param_types = param_names
                            .iter()
                            .map(|ident| {
                                if let Some(anot) = ident.anot.get(0) {
                                    MaybeType::Known(anot.clone())
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
                    ast::Passable::Value(inlinable) => Ok((
                        (MaybeType::Known(Type::Function(Box::new((vec![], inlinable.into()))))),
                        ir::Entity::LambdaPointer(Box::new((
                            ir::Entity::Inlined(inlinable.clone().into()),
                            vec![],
                        ))),
                    )),
                }
            }
            ast::Entity::If(branches, else_do) => self
                .if_expression(branches, else_do, meta)
                .map_err(|e| e.fallback_index(token.pos()).fallback_fid(meta.fid)),
            ast::Entity::First(branches) => self.first_statement(branches, meta),
            ast::Entity::Record(ident, fields) => self
                .record(ident, fields, meta)
                .map_err(|e| e.fallback_fid(token.pos())),
            ast::Entity::List(branches) => self.list(branches, meta),
            ast::Entity::SingleIdent(ident) => match meta.try_use(&ident.inner.name) {
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
                    Identifiable::Where((fid, funcid), whereid) => {
                        let func = &self.parser.modules[fid].functions[funcid];
                        self.build(&func.wheres[whereid].1.clone().set(token.pos()), meta)
                    }
                },
                None => {
                    // Lets see if it's a constant
                    const NO_PARAMS: &[MaybeType] = &[];
                    let (entry, meta) = self
                        .parser
                        .find_func_meta(meta.fid, ident, NO_PARAMS)
                        .map_err(|e| {
                            if let ParseFault::FunctionNotFound(_, _) = e {
                                ParseFault::IdentifierNotFound(ident.inner.name.clone())
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
            ast::Entity::Lambda(param_names, body) => {
                if param_names.is_empty() {
                    self.build(&body.clone(), meta)
                } else {
                    Err(ParseFault::ParameterlessLambda
                        .into_err(token.pos())
                        .fallback_fid(meta.fid))
                }
            }
        }
    }

    fn record(
        &'a self,
        ident: &Anot<Identifier, Type>,
        fields: &[(String, Tracked<ast::Entity>)],
        meta: &mut Meta,
    ) -> Result<(MaybeType, ir::Entity), ParseError> {
        match meta.try_use(&ident.inner.name) {
            Some(local) => {
                // We're modifying a struct in the current scope
                unimplemented!();
            }
            None => {
                // We're constructing a new struct
                let (fid, tid) = self
                    .parser
                    .find_type(meta.fid, &ident)
                    .map_err(|e| e.into_err(0))?;
                // TODO:
                // I think I'm gonna just create code for creating a new record
                // and then reuse that code for modyfing records by just reusing the scoped fields
                // in the new one.
                match &self.parser.modules[fid].types[tid] {
                    CustomType::Struct(r#struct) => {
                        let sorted = r#struct.copy_order_for(fields);
                        let mut evaluated_fields = Vec::with_capacity(sorted.len());
                        for (entity, expected_t) in sorted.iter() {
                            let (t, ir) = self.build(entity, meta)?;
                            if t.unwrap() != *expected_t {
                                panic!("ET");
                            }
                            evaluated_fields.push(ir);
                        }
                        let ir = ir::Entity::ConstructRecord(evaluated_fields);
                        let t = MaybeType::Known(Type::KnownCustom(fid, tid));
                        Ok((t, ir))
                    }
                    CustomType::Enum(r#enum) => {
                        Err(ParseFault::RecordWithEnum(fid, ident.clone()).into_err(0))
                    }
                }
            }
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

    fn wrap_into_lambda(
        &'a self,
        ident: Anot<Identifier, Type>,
        mut parameters: Vec<Tracked<ast::Entity>>,
        meta: &mut Meta,
    ) -> Result<ast::Entity, ParseError> {
        let (want, _returns): (&[Type], _) = match meta.try_use(&ident.inner.name) {
            Some(im) => match &im.r#type {
                MaybeType::Known(Type::Function(box (takes, gives))) => (&takes, gives),
                MaybeType::Known(other) => panic!("ET: This cannot take parameters: {:?}", other),
                MaybeType::Infer(t) => unimplemented!(),
            },
            None => {
                let (fid, funcid) = self.find_only_suitable(meta.fid, &ident, parameters.len())?;
                let func = &self.parser.modules[fid].functions[funcid];
                (&func.parameter_types, &func.returns)
            }
        };

        let mut fake_params = Vec::new();
        for i in 0..(want.len() - parameters.len()) {
            let param = vec![digit_to_letter(i)];
            let body = Anot::new(Identifier::from(String::from_utf8(param).unwrap()));

            fake_params.push(body.clone());
            parameters.push(Tracked::new(ast::Entity::SingleIdent(body)));
        }

        let call = ast::Callable::Func(ident);
        let lambda_body = Tracked::new(ast::Entity::Call(call, parameters));

        let generated_ast =
            ast::Entity::Pass(ast::Passable::Lambda(fake_params, Box::new(lambda_body)));
        Ok(generated_ast)
    }
}

// 1 2 3 -> a b c
// used for generated faking lambda parameters for turning #(f x) into #(\a b -> f x a b)
fn digit_to_letter(digit: usize) -> u8 {
    (digit + 97) as u8
}

fn destruct_callable_ident(
    mt: MaybeType,
    param_types: Vec<MaybeType>,
) -> Result<(Vec<Type>, Type), ParseFault> {
    let t = match mt {
        MaybeType::Infer(mt) => mt.borrow().clone().unwrap(),
        MaybeType::Known(t) => t,
    };
    if let Type::Function(box (takes, gives)) = t {
        if takes.len() != param_types.len() {
            return Err(ParseFault::ParamCallAmountMismatch(Box::new((
                takes,
                gives,
                param_types,
            ))));
        }
        if takes.len() != param_types.len() {
            return Err(ParseFault::ParamCallAmountMismatch(Box::new((
                takes,
                gives,
                param_types,
            ))));
        }
        for (i, take) in takes.iter().enumerate() {
            match &param_types[i] {
                MaybeType::Known(t) => {
                    if *t != *take {
                        return Err(ParseFault::ParamCallMismatch(Box::new((
                            takes,
                            gives,
                            param_types,
                        ))));
                    }
                }
                MaybeType::Infer(t) => {
                    if let Some(t) = t.borrow().as_ref() {
                        if *t != *take {
                            return Err(ParseFault::ParamCallMismatch(Box::new((
                                takes,
                                gives,
                                param_types.clone(),
                            ))));
                        }
                        continue;
                    }
                    *t.borrow_mut() = Some(take.clone());
                }
            }
        }
        Ok((takes, gives))
    } else {
        Err(ParseFault::ParamCannotTakeParameters(Box::new((
            t,
            param_types,
        ))))
    }
}
