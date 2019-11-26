use super::runtime::Runtime;
use crate::ir::{Entity, First, If, Value};

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

    fn spawn(&self, entity: &'a Entity, params: ParamBuffer) -> Value {
        Runner {
            runtime: self.runtime,
            entity,
            params,
            lambda_buffer: self.lambda_buffer.clone(),
        }
        .run()
    }

    fn run(mut self) -> Value {
        loop {
            match self.entity {
                Entity::RustCall(index, params) => return self.rust_call(*index, params),
                Entity::Parameter(n) => return self.params.param_consume(*n as usize),
                Entity::Inlined(v) => return v.clone(),
                Entity::IfExpression(expr) => return self.if_expression(expr),
                Entity::FirstStatement(stmt) => return self.first_statement(stmt),
                Entity::List(list) => return self.list(list),
                Entity::LambdaParam(n) => {
                    return self.lambda_buffer[*n as usize].clone();
                }
                Entity::Lambda(entries) => {
                    let mut evaluated_params = match entries.len() - 1 {
                        0 => Vec::new(),
                        1 => {
                            let new_params = self.params.consume();
                            vec![self.spawn(&entries[1], new_params)]
                        }
                        _ => {
                            let mut buf = Vec::with_capacity(entries.len());
                            for p in entries[1..entries.len() - 1].iter() {
                                buf.push(self.spawn(p, self.params.borrow()))
                            }
                            let new_params = self.params.consume();
                            buf.push(self.spawn(&entries[entries.len() - 1], new_params));
                            buf
                        }
                    };
                    self.lambda_buffer.append(&mut evaluated_params);
                    self.entity = &entries[0];
                }
                Entity::FunctionCall(findex, params) => {
                    let evaluated_params = match params.len() {
                        0 => Vec::new(),
                        1 => {
                            let new_params = self.params.consume();
                            vec![self.spawn(&params[0], new_params)]
                        }
                        _ => {
                            let mut buf = Vec::with_capacity(params.len());
                            for p in params[0..params.len() - 1].iter() {
                                buf.push(self.spawn(p, self.params.borrow()))
                            }
                            let new_params = self.params.consume();
                            buf.push(self.spawn(&params[params.len() - 1], new_params));
                            buf
                        }
                    };

                    self.params = evaluated_params.into();
                    let entity = &self.runtime.instructions[*findex as usize];
                    self.entity = entity;
                }
                _ => unimplemented!("{:?}", self.entity),
            }
        }
    }

    fn rust_call(mut self, index: u16, rust_params: &'a [Entity]) -> Value {
        match rust_params.len() {
            2 => {
                let func = bridge::get_func_two(index);

                let a = self.spawn(&rust_params[0], self.params.borrow());

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
            if let Value::Bool(true) = self.spawn(cond, self.params.borrow()) {
                self.entity = expr.evaluation(i);
                return self.run();
            }
        }
        self.entity = expr.r#else();
        self.run()
    }
    fn first_statement(mut self, stmt: &'a First<Entity>) -> Value {
        for entity in stmt.to_void() {
            self.spawn(entity, self.params.borrow());
        }
        self.entity = stmt.to_eval();
        self.run()
    }
    fn list(mut self, list: &'a [Entity]) -> Value {
        let mut buf = Vec::with_capacity(list.len());
        for entity in list[0..list.len() - 1].iter() {
            buf.push(self.spawn(entity, self.params.borrow()))
        }
        buf.push({
            self.entity = &list[list.len() - 1];
            self.run()
        });
        Value::List(buf)
    }
}
