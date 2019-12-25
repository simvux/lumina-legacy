use super::runtime::Runtime;
use crate::ir::{Capturable, Entity, First, If, Value};

mod bridge;
mod parambuffer;
use parambuffer::*;
use termion::color::{Fg, Green, Reset, Yellow};

pub struct Runner<'a> {
    runtime: &'a Runtime,
    entity: &'a Entity,
    params: ParamBuffer<'a>,
    captured: Vec<Value>,
}

macro_rules! debug {
    ($($arg:tt)*) => (
        #[cfg(debug_assertions)]
        print!(" {}runner {}->{} ", Fg(Green), Fg(Yellow), Fg(Reset));
        #[cfg(debug_assertions)]
        println!($($arg)*);
    )
}

impl<'a> Runner<'a> {
    pub fn start(runtime: &'a Runtime, entrypoint: &'a Entity, params: ParamBuffer<'a>) -> Value {
        Self {
            runtime,
            entity: entrypoint,
            params,
            captured: Vec::new(),
        }
        .run()
    }

    fn spawn(&self, entity: &'a Entity, params: ParamBuffer, captured: Vec<Value>) -> Value {
        Runner {
            runtime: self.runtime,
            entity,
            params,
            captured,
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
                    let evaluated_params = self.eval_params(params);
                    if let Value::Function(box (entity, captured)) =
                        self.params.param_borrow(*paramid as usize)
                    {
                        // TODO: Fix memory management
                        return self.spawn(entity, evaluated_params, captured.clone());
                    } else {
                        unreachable!();
                    }
                }
                Entity::Captured(n) => return self.captured[*n as usize].clone(),
                Entity::Lambda(all, to_capture) => {
                    let entries = &all[1..];
                    let mut buf = Vec::with_capacity(to_capture.len());
                    for c in to_capture.iter() {
                        match c {
                            Capturable::ParentParam(n) => {
                                buf.push(self.params.param_borrow(*n).clone())
                            }
                            _ => unimplemented!(),
                        }
                    }

                    self.params = self.eval_params(entries);
                    self.captured = buf;
                    self.entity = &all[0];
                }
                Entity::FunctionCall(findex, params) => {
                    self.params = self.eval_params(params);
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

    fn eval_params(&mut self, params: &'a [Entity]) -> ParamBuffer<'a> {
        match params.len() {
            0 => ParamBuffer::Owned(Vec::new()),
            1 => {
                let new_params = self.params.borrow();
                ParamBuffer::SingleOwned(self.spawn(&params[0], new_params, self.captured.clone()))
            }
            _ => {
                let mut buf = Vec::with_capacity(params.len());
                for p in params[0..params.len() - 1].iter() {
                    buf.push(self.spawn(p, self.params.borrow(), self.captured.clone()))
                }
                let new_params = self.params.borrow();
                buf.push(self.spawn(&params[params.len() - 1], new_params, self.captured.clone()));
                ParamBuffer::from(buf)
            }
        }
    }

    fn rust_call(mut self, mut index: u16, rust_params: &'a [Entity]) -> Value {
        match rust_params.len() {
            2 => {
                let a = self.spawn(&rust_params[0], self.params.borrow(), self.captured.clone());

                self.entity = &rust_params[1];
                let mut b = self.run();

                if index >= 100 {
                    index -= 100;
                    let func = bridge::get_func_write(index);
                    func(&a, &mut b);
                    b
                } else {
                    let func = bridge::get_func_copy(index);
                    func(a, b)
                }
            }
            _ => unreachable!(),
        }
    }
    fn if_expression(mut self, expr: &'a If<Entity>) -> Value {
        for i in 0..expr.branches() {
            let cond = expr.condition(i);
            if let Value::Bool(true) = self.spawn(cond, self.params.borrow(), self.captured.clone())
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
            self.spawn(entity, self.params.borrow(), self.captured.clone());
        }
        self.entity = stmt.to_eval();
        self.run()
    }
    fn list(mut self, list: &'a [Entity]) -> Value {
        let mut buf = Vec::with_capacity(list.len());
        for entity in list[0..list.len() - 1].iter() {
            buf.push(self.spawn(entity, self.params.borrow(), self.captured.clone()))
        }
        buf.push({
            self.entity = &list[list.len() - 1];
            self.run()
        });
        Value::List(buf)
    }
}
