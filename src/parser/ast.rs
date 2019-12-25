use crate::env::Environment;
use crate::ir;
use crate::parser::{MaybeType, ParseError, Parser, Type};
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::rc::Rc;

mod checker;
mod entity;
pub use entity::{Callable, Entity, Identifier, IdentifierType, Passable};
mod builder;
pub use builder::AstBuilder;
mod metainfo;
pub use metainfo::{IdentMeta, Identifiable, Meta};

pub struct IrBuilder {
    parser: Parser,
    completed: RefCell<Vec<ir::Entity>>,
    environment: Rc<Environment>,
    assigned_indexes: RefCell<HashMap<Meta, usize>>,
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

    pub fn gen_id(&self, meta: &Meta) -> usize {
        let mut map = self.assigned_indexes.borrow_mut();
        match map.get_mut(meta) {
            Some(i) => *i,
            None => {
                let i = map.len();
                map.insert(meta.clone(), i);
                i
            }
        }
    }

    pub fn start_type_checker(
        self,
        fid: usize,
        name: &str,
        params: &[MaybeType],
    ) -> Result<(Vec<ir::Entity>, usize), ParseError> {
        let (_returns, assigned_index) = match self.find_and_build_function(
            fid,
            &Identifier::try_from(name).unwrap(),
            &mut params.to_vec(),
        ) {
            Ok(a) => a,
            Err(e) => return Err(e.with_parser(self.parser)),
        };
        Ok((self.completed.into_inner(), assigned_index))
    }

    pub fn complete(&self, findex: usize, entity: ir::Entity) {
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
