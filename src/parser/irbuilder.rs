use crate::env::Environment;
use crate::ir;
use crate::parser::{ParseError, ParseFault, Parser, Type};
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

mod checker;
mod fsource;
pub mod generics;
pub use fsource::FunctionSource;
pub mod lambda;
use lambda::IdentPool;

//#[derive(Debug)]
pub struct IrBuilder {
    parser: Parser,
    completed: RefCell<Vec<ir::Entity>>,
    environment: Rc<Environment>,
    assigned_indexes: RefCell<HashMap<FunctionSource, usize>>,
}

impl IrBuilder {
    pub fn new(parser: Parser, env: Rc<Environment>) -> Self {
        Self {
            parser,
            environment: env,
            assigned_indexes: RefCell::default(),
            completed: RefCell::new(Vec::with_capacity(5)),
        }
    }

    pub fn start_type_checker(
        self,
        fid: usize,
        funcname: &str,
        params: &[Type],
    ) -> Result<(Vec<ir::Entity>, usize), ParseError> {
        let source = match self.parser.find_func((fid, funcname, params)).map_err(|e| {
            e.to_err(0)
                .with_source_load(&self.environment, &self.parser.modules[fid].module_path)
        }) {
            Ok(a) => a,
            Err(e) => return Err(e.with_parser(self.parser)),
        };

        let func = source.func(&self.parser);
        let (actual_return_value, entry) =
            match self.type_check(&func.body, &source, &mut IdentPool::new()) {
                Ok(t) => t,
                Err(e) => return e.with_parser(self.parser).into(),
            };
        if actual_return_value != func.returns && func.returns != Type::Nothing {
            return ParseFault::FnTypeReturnMismatch(Box::new(func.clone()), actual_return_value)
                .to_err(func.body.source_index)
                .with_source_load(&self.environment, &self.parser.modules[fid].module_path)
                .with_parser(self.parser)
                .into();
        }
        let findex = self.gen_id(Cow::Borrowed(&source));
        self.complete(findex, entry);

        Ok((
            self.completed.into_inner(),
            self.assigned_indexes.borrow()[&source],
        ))
    }

    pub fn complete(&self, findex: usize, entity: ir::Entity) {
        debug!(" || {} -> {:?}", findex, entity);
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
