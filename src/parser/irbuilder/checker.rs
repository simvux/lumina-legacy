use super::fsource::FunctionSource;
use super::IrBuilder;
use crate::ir;
use crate::parser::{Inlined, ParseError, ParseFault, RawToken, Token, Type};
use std::borrow::Cow;
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
    ) -> Result<(Type, ir::Entity), ParseError> {
        macro_rules! discover_ident {
            ($ident:expr, $params:expr) => {{
                self.discover_ident(source, $ident, $params, &identpool)
                    .map_err(|e| e.to_err(token.source_index))?
            }};
        };
        match &token.inner {
            RawToken::Inlined(inlined) => {
                debug!("Handing type of inlined value {}", inlined);
                let v = inlined.clone();
                let t = match inlined {
                    Inlined::Int(_) => Type::Int,
                    Inlined::Float(_) => Type::Float,
                    Inlined::Bool(_) => Type::Bool,
                    Inlined::Nothing => Type::Nothing,
                };
                Ok((t, ir::Entity::Inlined(v.into())))
            }
            RawToken::Unimplemented => Ok((
                source.returns(&self.parser).clone(),
                ir::Entity::Unimplemented,
            )),
            RawToken::ByPointer(box t) => {
                unimplemented!();
            }
            RawToken::RustCall(bridged_id, r#type) => {
                debug!("Handing type of rustcall constant {}", r#type);
                Ok((
                    r#type.clone(),
                    ir::Entity::RustCall(*bridged_id, Vec::new()),
                ))
            }
            RawToken::FirstStatement(entries) => {
                let mut buf = Vec::with_capacity(entries.len());
                for entry in entries[0..entries.len() - 1].iter() {
                    let (_, v) = self.type_check(entry, source, identpool.clone())?;
                    buf.push(v);
                }
                let (t, v) = self.type_check(entries.last().unwrap(), source, identpool)?;
                buf.push(v);
                Ok((t, ir::Entity::FirstStatement(ir::First::from(buf))))
            }
            RawToken::Parameterized(box entry, params, p_types) => {
                let mut param_entities = Vec::with_capacity(params.len());
                let mut param_types = p_types.borrow_mut();
                let save_types = param_types.is_empty();
                for param in params.iter() {
                    let (t, v) = self.type_check(param, source, identpool.clone())?;
                    if save_types {
                        param_types.push(t);
                    }
                    param_entities.push(v);
                }
                drop(param_types);
                match &entry.inner {
                    RawToken::Identifier(ident, _) => {
                        let param_types = p_types.borrow();
                        match discover_ident!(ident, &param_types) {
                            Param(_) => {
                                // TODO: `f x` where f: param<(a -> b)>
                                unimplemented!();
                            }
                            Lambda(_) => unimplemented!(),
                            Function(newsource) => {
                                // We don't want to type check forever in recursion
                                if newsource == *source {
                                    let findex = self.gen_id(Cow::Borrowed(&newsource));
                                    return Ok((
                                        source.func(&self.parser).returns.clone(),
                                        ir::Entity::FunctionCall(findex as u32, param_entities),
                                    ));
                                }
                                drop(param_types);
                                let (t, v) = self.type_check(
                                    &newsource.func(&self.parser).body,
                                    &newsource,
                                    HashMap::new(),
                                )?;
                                let findex = self.gen_id(Cow::Borrowed(&newsource));
                                self.complete(findex, v);
                                debug!("Completed function {}", findex);
                                Ok((t, ir::Entity::FunctionCall(findex as u32, param_entities)))
                            }
                        }
                    }
                    RawToken::RustCall(bridged_id, r#type) => {
                        debug!("Handing type of rustcall {}", bridged_id);
                        Ok((
                            r#type.clone(),
                            ir::Entity::RustCall(*bridged_id, param_entities),
                        ))
                    }
                    _ => panic!("{:#?} cannot take parameters", entry.inner),
                }
            }
            RawToken::Identifier(ident, _) => match discover_ident!(ident, &[]) {
                Lambda(_) => unimplemented!(),
                Param(id) => Ok((
                    source.func(&self.parser).get_parameter_type(id).clone(),
                    ir::Entity::Parameter(id as u16),
                )),
                Function(source) => {
                    const NO_ARGS: &[Type] = &[];
                    let newsource = self
                        .parser
                        .find_func((source.fid(), ident.as_slice(), NO_ARGS))
                        .map_err(|e| e.to_err(token.source_index))?;
                    let (t, v) = self.type_check(
                        &newsource.func(&self.parser).body,
                        &newsource,
                        HashMap::new(),
                    )?;
                    let findex = self.gen_id(Cow::Borrowed(&newsource));
                    self.complete(findex, v);
                    Ok((t, ir::Entity::FunctionCall(findex as u32, Vec::new())))
                }
            },
            RawToken::IfExpression(expr) => {
                let mut branch_buf = Vec::with_capacity((expr.branches.len() * 2) + 1);
                let mut expect_type = None;
                for (cond, eval) in expr.branches.iter() {
                    let (ct, cv) = self.type_check(cond, source, identpool.clone())?;
                    if ct != Type::Bool {
                        panic!(
                            "ET: Condition must result in true or false, but I got {:?}",
                            ct
                        );
                    }
                    branch_buf.push(cv);
                    let (et, ev) = self.type_check(eval, source, identpool.clone())?;
                    if let Some(expected) = &expect_type {
                        if et != *expected {
                            panic!(
                                "ET: Branches have different types. Wanted {} got {}",
                                expected, et
                            );
                        }
                    } else {
                        expect_type = Some(et);
                    }
                    branch_buf.push(ev);
                }
                let (et, ev) = self.type_check(&expr.else_branch, source, identpool)?;
                if let Some(expected) = &expect_type {
                    if et != *expected {
                        panic!(
                            "ET: Branches have different types. Wanted {} got {}",
                            expected, et
                        );
                    }
                }
                branch_buf.push(ev);
                Ok((
                    expect_type.unwrap(),
                    ir::Entity::IfExpression(ir::If::from(branch_buf)),
                ))
            }
            RawToken::List(entries) => {
                let mut of_t: Option<Type> = None;
                let mut list_buf = Vec::with_capacity(entries.len());
                for (i, entry) in entries.iter().enumerate() {
                    let (r#type, entity) = self.type_check(entry, source, identpool.clone())?;
                    list_buf.push(entity);
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
                Ok((
                    Type::List(Box::new(of_t.unwrap_or_else(|| Type::Generic(0)))),
                    ir::Entity::List(list_buf),
                ))
            }
            _ => panic!("Cannot discover type of {:#?}", token),
        }
    }
}
