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
                        let source = self
                            .parser
                            .find_func((
                                source.fid(),
                                ident.as_slice(),
                                param_types.borrow().as_slice(),
                            ))
                            .unwrap();

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
                if let Some(paramid) = source.func(&self.parser).get_parameter_from_ident(ident) {
                    return ir::Entity::Parameter(paramid as u16);
                }
                const NO_PARAMS: &[Type] = &[];
                let source = self
                    .parser
                    .find_func((source.fid(), ident.as_slice(), NO_PARAMS))
                    .unwrap();
                self.token_to_ir(&source, &source.func(&self.parser).body.inner)
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
