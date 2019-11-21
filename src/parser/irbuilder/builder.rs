use super::{FunctionSource, IrBuilder};
use crate::ir;
use crate::parser::{Inlined, RawToken, Type, PRELUDE_FID};
use std::borrow::Cow;

impl IrBuilder {
    pub fn token_to_ir(&self, source: &FunctionSource, t: &RawToken) -> ir::Entity {
        match t {
            RawToken::Parameterized(box takes, has, param_types) => {
                let params = has
                    .iter()
                    .map(|t| self.token_to_ir(source, &t.inner))
                    .collect::<Vec<ir::Entity>>();
                match &takes.inner {
                    RawToken::Identifier(ident, anot) => {
                        let ptypes: &[Type] = &param_types.borrow();
                        let (newfid, (newfuncid, generics)) = {
                            let fid = if ident.len() == 1 {
                                source.fid()
                            } else {
                                self.parser.modules[source.fid()].imports[&ident[0]]
                            };
                            self.parser.modules[fid]
                                .function_ids
                                .get(ident.last().unwrap())
                                .map(|a| (fid, super::generics::generic_search(a, ptypes).unwrap()))
                                .unwrap_or_else(|| {
                                    (
                                        PRELUDE_FID,
                                        super::generics::generic_search(
                                            &self.parser.modules[PRELUDE_FID].function_ids
                                                [ident.last().unwrap()],
                                            ptypes,
                                        )
                                        .unwrap_or_else(
                                            || {
                                                panic!(
                                                    "{:#?} <=====> {:#?}\n{:#?}",
                                                    ident,
                                                    &self.parser.modules[PRELUDE_FID].function_ids,
                                                    ptypes,
                                                )
                                            },
                                        ),
                                    )
                                })
                        };

                        let source = if generics.has_generics() {
                            let mut new = self.parser.modules[newfid].functions[newfuncid].clone();
                            generics.replace_all(&mut new);
                            FunctionSource::Owned(newfid, new)
                        } else {
                            FunctionSource::from((newfid, newfuncid))
                        };

                        let entry = self.token_to_ir(&source, &source.body(&self.parser).inner);
                        let findex = self.gen_id(Cow::Owned(source));
                        self.complete(findex, entry);

                        ir::Entity::FunctionCall(findex as u32, params)
                    }
                    RawToken::RustCall(id, _returns_type) => ir::Entity::RustCall(*id, params),
                    _ => panic!("{:?} cannot take parameters", takes.inner),
                }
            }
            // Either constant or parameter
            RawToken::Identifier(ident, anot) => {
                const NO_PARAMS: &[Type] = &[];
                let fid = if ident.len() == 1 {
                    let func = source.func(&self.parser);
                    for (i, param) in func.parameter_names.iter().enumerate() {
                        if &ident[0] == param {
                            return ir::Entity::Parameter(i as u16);
                        }
                    }
                    source.fid()
                } else {
                    *self.parser.modules[source.fid()]
                        .imports
                        .get(&ident[0])
                        .expect("Module not imported")
                };
                let newfuncid =
                    self.parser.modules[fid].function_ids[ident.last().unwrap()][NO_PARAMS];
                self.token_to_ir(
                    &FunctionSource::from((source.fid(), newfuncid)),
                    &self.parser.modules[source.fid()].functions[newfuncid]
                        .body
                        .inner,
                )
            }
            RawToken::Inlined(inlined) => match &inlined {
                Inlined::Int(n) => ir::Entity::Inlined(ir::Value::Int(*n)),
                Inlined::Float(n) => ir::Entity::Inlined(ir::Value::Float(*n)),
                Inlined::Bool(b) => ir::Entity::Inlined(ir::Value::Bool(*b)),
                Inlined::Nothing => ir::Entity::Inlined(ir::Value::Nothing),
            },
            RawToken::FirstStatement(entries) => ir::Entity::FirstStatement(ir::First::from(
                entries
                    .iter()
                    .map(|e| self.token_to_ir(source, &e.inner))
                    .collect::<Vec<ir::Entity>>(),
            )),
            RawToken::IfExpression(expr) => {
                let mut buf = Vec::with_capacity((expr.branches.len() * 2) + 1);
                buf.push(self.token_to_ir(source, &expr.else_branch.inner));
                for (cond, eval) in expr.branches.iter() {
                    buf.push(self.token_to_ir(source, &cond.inner));
                    buf.push(self.token_to_ir(source, &eval.inner));
                }
                ir::Entity::IfExpression(ir::If::from(buf))
            }
            RawToken::Unimplemented => ir::Entity::Unimplemented,
            RawToken::List(entries) => ir::Entity::List(
                entries
                    .iter()
                    .map(|t| self.token_to_ir(source, &t.inner))
                    .collect(),
            ),
            _ => unimplemented!("{:?}", t),
        }
    }

    pub fn complete(&self, findex: usize, entity: ir::Entity) {
        debug!(" || {} -> {:?}\n", findex, entity);
        let mut stack = self.completed.borrow_mut();
        if findex > stack.len() {
            stack.resize(findex, ir::Entity::Unique)
        }
        match stack.get_mut(findex) {
            Some(a) => *a = entity,
            None => stack.insert(findex, entity),
        }
    }
}
