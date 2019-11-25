use super::fsource::FunctionSource;
use super::generics::*;
use super::IrBuilder;
use crate::parser::{Inlined, ParseError, ParseFault, RawToken, Token, Type};
use std::collections::HashMap;

#[derive(Clone)]
pub enum Identifiable {
    Param(usize),
    Function(FunctionSource),
    Lambda(usize),
}
use Identifiable::*;

impl IrBuilder {
    fn discover_ident(
        &self,
        source: &FunctionSource,
        ident: &[String],
        params: &[Type],
        identpool: &HashMap<&str, usize>,
    ) -> Result<Identifiable, ParseFault> {
        if ident.len() == 1 {
            let func = source.func(&self.parser);
            if let Some(paramid) = func.get_parameter_from_ident(ident) {
                return Ok(Identifiable::Param(paramid));
            };

            if let Some(bufid) = identpool.get::<str>(&ident[0]) {
                return Ok(Identifiable::Lambda(*bufid));
            };
        }
        Ok(Identifiable::Function(self.parser.find_func((
            source.fid(),
            ident,
            params,
        ))?))
    }

    pub fn type_check(
        &self,
        token: &Token,
        source: &FunctionSource,
        identpool: HashMap<&str, usize>,
    ) -> Result<Type, ParseError> {
        macro_rules! discover_ident {
            ($ident:expr, $params:expr) => {{
                self.discover_ident(source, $ident, $params, &identpool)
                    .map_err(|e| e.to_err(token.source_index))?
            }};
        };
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
                    RawToken::Identifier(ident, _) => {
                        // let func = &self.parser.modules[fid].functions[funcid];
                        let func = source.func(&self.parser);

                        // if let Some(paramid) = func.get_parameter_from_ident(ident) {}
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
                    self.type_check(entry, source, identpool.clone())?;
                }
                self.type_check(entries.last().unwrap(), source, identpool)?
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
                    debug!("Filling in parameters for {:?}\n", entry);
                    for param in params.iter() {
                        param_types.push(self.type_check(param, source, identpool.clone())?)
                    }
                } else {
                    debug!("Using existing type-checked parameters {:?}\n", param_types);
                }
                drop(param_types);
                match &entry.inner {
                    RawToken::Identifier(ident, _) => {
                        let param_types = p_types.borrow();
                        match discover_ident!(ident, &param_types) {
                            Param(_) => {
                                // TODO: `f x` where f: (a -> b)
                                unimplemented!();
                            }
                            Lambda(_) => unimplemented!(),
                            Function(source) => self.type_check_function(source)?,
                        }
                    }
                    RawToken::RustCall(bridged_id, r#type) => {
                        debug!("Handing type of rustcall {}\n", bridged_id);
                        r#type.clone()
                    }
                    _ => panic!("{:#?} cannot take parameters", entry.inner),
                }
            }
            RawToken::Identifier(ident, _) => match discover_ident!(ident, &[]) {
                Lambda(_) => unimplemented!(),
                Param(id) => source.func(&self.parser).get_parameter_type(id).clone(),
                Function(source) => self.type_check_function(source)?,
            },
            RawToken::IfExpression(expr) => {
                let mut expect_type = None;
                for (cond, eval) in expr.branches.iter() {
                    let cv = self.type_check(cond, source, identpool.clone())?;
                    if cv != Type::Bool {
                        panic!(
                            "ET: Condition must result in true or false, but I got {:?}",
                            cv
                        );
                    }
                    let ev = self.type_check(eval, source, identpool.clone())?;
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
                let ev = self.type_check(&expr.else_branch, source, identpool)?;
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
                    let r#type = self.type_check(entry, source, identpool.clone())?;
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
        match t {
            RawToken::Identifier(ident, _) => self
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
