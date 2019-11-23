use super::fsource::FunctionSource;
use super::generics::*;
use super::IrBuilder;
use crate::parser::{Inlined, ParseError, ParseFault, RawToken, Token, Type, PRELUDE_FID};

impl IrBuilder {
    // pub fn type_check(&self, token: &Token, fid: usize, funcid: usize) -> Result<Type, ParseError> {
    pub fn type_check(&self, token: &Token, source: &FunctionSource) -> Result<Type, ParseError> {
        let r#type = match &token.inner {
            RawToken::Inlined(inlined) => {
                debug!("Handing type of inlined value {}\n", inlined);
                match inlined {
                    Inlined::Int(_) => Type::Int,
                    Inlined::Float(_) => Type::Float,
                    Inlined::Bool(_) => Type::Bool,
                    Inlined::Nothing => Type::Nothing,
                }
            }
            RawToken::Unimplemented => source.returns(&self.parser).clone(),
            RawToken::ByPointer(box t) => {
                match &t.inner {
                    RawToken::Identifier(ident, anot) => {
                        // let func = &self.parser.modules[fid].functions[funcid];
                        let func = source.func(&self.parser);

                        if let Some(paramid) = func.get_parameter_from_ident(ident) {}
                        unimplemented!();
                    }
                    _ => unimplemented!(),
                }
            }
            RawToken::RustCall(_bridged_id, r#type) => {
                debug!("Handing type of rustcall constant {}\n", r#type);
                r#type.clone()
            }
            RawToken::FirstStatement(entries) => {
                for entry in entries[0..entries.len() - 1].iter() {
                    self.type_check(entry, source)?;
                }
                self.type_check(entries.last().unwrap(), source)?
            }
            RawToken::Parameterized(box entry, params, p_types) => {
                let mut param_types = match p_types.try_borrow_mut() {
                    Ok(a) => a,
                    Err(_) => {
                        debug!("Skipping type check of {:?}", entry);
                        // If it's already borrowed then that means that this is a recursive call.
                        // Therefore we can assume that it's already being type checked!
                        return Ok(self.find_return_type(
                            source.fid(),
                            &p_types.borrow(),
                            &entry.inner,
                        ));
                    }
                };
                if param_types.is_empty() {
                    for param in params.iter() {
                        param_types.push(self.type_check(param, source)?)
                    }
                    debug!("Gathered new type-checked parameters {:?}\n", param_types);
                } else {
                    debug!("Using existing type-checked parameters {:?}\n", param_types);
                }
                drop(param_types);
                match &entry.inner {
                    RawToken::Identifier(ident, anot) => {
                        let param_types = p_types.borrow();
                        debug!("Calling {} with {:?}\n", ident.join(":"), param_types);
                        self.type_check_function_source(source.fid(), ident, &param_types)
                            .map_err(|e| e.fallback(token.source_index))?
                    }
                    RawToken::RustCall(bridged_id, r#type) => {
                        debug!("Handing type of rustcall {}\n", bridged_id);
                        r#type.clone()
                    }
                    _ => panic!("{:#?} cannot take parameters", entry.inner),
                }
            }
            RawToken::Identifier(ident, anot) => {
                if ident.len() == 1 {
                    let func = source.func(&self.parser);
                    if let Some(paramid) = func.get_parameter_from_ident(ident) {
                        let r#type = func.get_parameter_type(paramid).clone();
                        debug!(
                            "{} was identified as parameter of type {}\n",
                            ident.join(":"),
                            r#type
                        );
                        return Ok(r#type);
                    };
                }

                // This is only for leaf constants. Since other functions will be RawToken::Parameterized
                debug!("Checking if {} is a constant\n", ident.join(":"));
                self.type_check_function_source(source.fid(), ident, &[])?
            }
            RawToken::IfExpression(expr) => {
                let mut expect_type = None;
                for (cond, eval) in expr.branches.iter() {
                    let cv = self.type_check(cond, source)?;
                    if cv != Type::Bool {
                        panic!(
                            "ET: Condition must result in true or false, but I got {:?}",
                            cv
                        );
                    }
                    let ev = self.type_check(eval, source)?;
                    if let Some(expected) = &expect_type {
                        if ev != *expected {
                            panic!(
                                "ET: Branches have different types. Wanted {} got {}",
                                expected, ev
                            );
                        }
                    } else {
                        expect_type = Some(ev);
                    }
                }
                let ev = self.type_check(&expr.else_branch, source)?;
                if let Some(expected) = &expect_type {
                    if ev != *expected {
                        panic!(
                            "ET: Branches have different types. Wanted {} got {}",
                            expected, ev
                        );
                    }
                }
                expect_type.unwrap()
            }
            RawToken::List(entries) => {
                let mut of_t: Option<Type> = None;
                for (i, entry) in entries.iter().enumerate() {
                    let r#type = self.type_check(entry, source)?;
                    match &of_t {
                        Some(t) => {
                            if *t != r#type {
                                return ParseFault::ListEntryTypeMismatch(r#type, t.clone(), i)
                                    .to_err(entry.source_index)
                                    .into();
                            }
                        }
                        None => of_t = Some(r#type),
                    }
                }
                Type::List(Box::new(of_t.unwrap_or_else(|| Type::Generic(0))))
            }
            _ => panic!("Cannot discover type of {:#?}", token),
        };
        Ok(r#type)
    }

    fn find_return_type(&self, fid: usize, params: &[Type], t: &RawToken) -> Type {
        let me = &self.parser.modules[fid];
        match t {
            RawToken::Identifier(ident, anot) => self
                .parser
                .find_func((fid, ident.as_slice(), params))
                .unwrap()
                .func(&self.parser)
                .returns
                .clone(),
            _ => unimplemented!(),
        }
    }
}
