use super::runtime::Runtime;
use crate::ir::{Capturable, Entity, First, If, Value};

mod bridge;
mod parambuffer;
use parambuffer::*;

pub struct Runner<'a> {
    runtime: &'a Runtime,
    entity: &'a Entity,
    params: ParamBuffer<'a>,
    lambda_buffer: Vec<Value>,
}

impl<'a> Runner<'a> {
    pub fn start(runtime: &'a Runtime, entrypoint: &'a Entity, params: ParamBuffer<'a>) -> Value {
        Self {
            runtime,
            entity: entrypoint,
            params,
            lambda_buffer: Vec::new(),
        }
        .run()
    }

    fn spawn(&self, entity: &'a Entity, params: ParamBuffer, lambda_buffer: Vec<Value>) -> Value {
        Runner {
            runtime: self.runtime,
            entity,
            params,
            lambda_buffer,
        }
        .run()
    }

    fn run(mut self) -> Value {
        loop {
            debug!("evaluating {:?}", &self.entity);
            match self.entity {
                Entity::RustCall(index, params) => return self.rust_call(*index, params),
                Entity::Parameter(n) => return self.params.param_consume(*n as usize),
                Entity::Inlined(v) => return v.clone(),
                Entity::IfExpression(expr) => return self.if_expression(expr),
                Entity::FirstStatement(stmt) => return self.first_statement(stmt),
                Entity::List(list) => return self.list(list),
                Entity::ParameterCall(paramid, params) => {
                    let evaluated_params = match params.len() {
                        0 => Vec::new(),
                        1 => vec![self.spawn(
                            &params[0],
                            self.params.borrow(),
                            self.lambda_buffer.clone(),
                        )],
                        _ => {
                            let mut buf = Vec::with_capacity(params.len());
                            for p in params[0..params.len() - 1].iter() {
                                buf.push(self.spawn(
                                    p,
                                    self.params.borrow(),
                                    self.lambda_buffer.clone(),
                                ))
                            }
                            let new_params = self.params.borrow();
                            buf.push(self.spawn(
                                &params[params.len() - 1],
                                new_params,
                                self.lambda_buffer.clone(),
                            ));
                            buf
                        }
                    };
                    if let Value::Function(box (entity, captured)) =
                        self.params.param_borrow(*paramid as usize)
                    {
                        return self.spawn(entity, evaluated_params.into(), captured.clone());
                    } else {
                        unreachable!();
                    }
                }
                Entity::LambdaParam(n) => {
                    return self.lambda_buffer[*n as usize].clone();
                }
                Entity::Lambda(all, to_capture) => {
                    let entries = &all[1..];
                    let evaluated_params = match entries.len() {
                        0 => Vec::new(),
                        1 => {
                            let new_params = self.params.borrow();
                            vec![self.spawn(&entries[0], new_params, self.lambda_buffer.clone())]
                        }
                        _ => {
                            let mut buf = Vec::with_capacity(entries.len());
                            for p in entries.iter() {
                                buf.push(self.spawn(
                                    p,
                                    self.params.borrow(),
                                    self.lambda_buffer.clone(),
                                ))
                            }
                            let new_params = self.params.consume();
                            buf.push(self.spawn(
                                &entries[entries.len() - 1],
                                new_params,
                                self.lambda_buffer.clone(),
                            ));
                            buf
                        }
                    };
                    let mut buf = Vec::with_capacity(to_capture.len());
                    for c in to_capture.iter() {
                        match c {
                            Capturable::ParentParam(n) => {
                                buf.push(self.params.param_borrow(*n).clone())
                            }
                            _ => unimplemented!(),
                        }
                    }
                    self.lambda_buffer = buf;
                    self.params = evaluated_params.into();
                    self.entity = &all[0];
                }
                Entity::FunctionCall(findex, params) => {
                    let evaluated_params = match params.len() {
                        0 => Vec::new(),
                        1 => {
                            let new_params = self.params.consume();
                            vec![self.spawn(&params[0], new_params, self.lambda_buffer.clone())]
                        }
                        _ => {
                            let mut buf = Vec::with_capacity(params.len());
                            for p in params[0..params.len() - 1].iter() {
                                buf.push(self.spawn(
                                    p,
                                    self.params.borrow(),
                                    self.lambda_buffer.clone(),
                                ))
                            }
                            let new_params = self.params.consume();
                            buf.push(self.spawn(
                                &params[params.len() - 1],
                                new_params,
                                self.lambda_buffer.clone(),
                            ));
                            buf
                        }
                    };
                    self.params = evaluated_params.into();
                    let entity = &self.runtime.instructions[*findex as usize];
                    self.entity = entity;
                }
                Entity::LambdaPointer(box (inner, to_capture)) => {
                    let mut captured = Vec::with_capacity(to_capture.len());
                    for capturable in to_capture.iter() {
                        match capturable {
                            Capturable::ParentParam(id) => {
                                captured.push(self.params.param_borrow(*id).clone())
                            }
                            Capturable::ParentLambda(id) => {
                                // TODO: Not sure about this
                                // captured.push(self.lambda_buffer[*id].clone())
                                unreachable!()
                            }
                            Capturable::ParentWhere(_) => unimplemented!("`where <identifier>:` values cannot be captured into closures (yet)"),
                        }
                    }
                    return Value::Function(Box::new((inner.clone(), captured)));
                }
                Entity::Unimplemented => panic!("TODO: Unimplemented escapes"),
                Entity::Unique => unreachable!(),
            }
        }
    }

    fn rust_call(mut self, index: u16, rust_params: &'a [Entity]) -> Value {
        match rust_params.len() {
            2 => {
                let func = bridge::get_func_two(index);

                let a = self.spawn(
                    &rust_params[0],
                    self.params.borrow(),
                    self.lambda_buffer.clone(),
                );

                self.entity = &rust_params[1];
                let b = self.run();

                func(a, b)
            }
            _ => unreachable!(),
        }
    }
    fn if_expression(mut self, expr: &'a If<Entity>) -> Value {
        for i in 0..expr.branches() {
            let cond = expr.condition(i);
            if let Value::Bool(true) =
                self.spawn(cond, self.params.borrow(), self.lambda_buffer.clone())
            {
                self.entity = expr.evaluation(i);
                return self.run();
            }
        }
        self.entity = expr.r#else();
        self.run()
    }
    fn first_statement(mut self, stmt: &'a First<Entity>) -> Value {
        for entity in stmt.to_void() {
            self.spawn(entity, self.params.borrow(), self.lambda_buffer.clone());
        }
        self.entity = stmt.to_eval();
        self.run()
    }
    fn list(mut self, list: &'a [Entity]) -> Value {
        let mut buf = Vec::with_capacity(list.len());
        for entity in list[0..list.len() - 1].iter() {
            buf.push(self.spawn(entity, self.params.borrow(), self.lambda_buffer.clone()))
        }
        buf.push({
            self.entity = &list[list.len() - 1];
            self.run()
        });
        Value::List(buf)
    }
}
