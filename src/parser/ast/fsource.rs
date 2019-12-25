/*
use super::IrBuilder;
use crate::parser::{ast, FunctionBuilder, Identifier, Parser, Tracked, Type};
use std::borrow::Cow;
use std::fmt;
use std::hash::Hash;
use std::rc::Rc;

#[derive(PartialEq, Hash, Eq, Clone)]
pub enum FunctionSource {
    Coordinate(usize, usize),
    Owned(usize, Rc<FunctionBuilder>),
}
impl fmt::Debug for FunctionSource {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FunctionSource::Coordinate(fid, funcid) => {
                write!(f, "FunctionSource({}:{})", fid, funcid)
            }
            FunctionSource::Owned(fid, funcb) => {
                write!(f, "FunctionSource({}:{})", fid, &funcb.name)
            }
        }
    }
}

impl From<(usize, usize)> for FunctionSource {
    fn from((fid, funcid): (usize, usize)) -> Self {
        Self::Coordinate(fid, funcid)
    }
}

impl From<(usize, FunctionBuilder)> for FunctionSource {
    fn from((fid, func): (usize, FunctionBuilder)) -> Self {
        Self::Owned(fid, Rc::new(func))
    }
}

impl From<(usize, String, Type)> for FunctionSource {
    fn from((fid, names, types): (usize, String, Type)) -> Self {
        let func = FunctionBuilder {
            name: Identifier::default(),
            parameter_names: vec![names],
            parameter_types: vec![types],
            returns: Type::Infer,
            body: Tracked::default(),
            wheres: Vec::new(),
        };
        FunctionSource::from((fid, func))
    }
}

impl FunctionSource {
    pub fn returns<'a>(&'a self, parser: &'a Parser) -> &'a Type {
        &self.func(parser).returns
    }
    pub fn body<'a>(&'a self, parser: &'a Parser) -> &'a Tracked<ast::Entity> {
        &self.func(parser).body
    }
    pub fn func<'a>(&'a self, parser: &'a Parser) -> &'a FunctionBuilder {
        match self {
            FunctionSource::Coordinate(fid, funcid) => &parser.modules[*fid].functions[*funcid],
            FunctionSource::Owned(_, func) => &func,
        }
    }
    pub fn fid(&self) -> usize {
        match self {
            FunctionSource::Coordinate(fid, _) => *fid,
            FunctionSource::Owned(fid, _) => *fid,
        }
    }
}

impl IrBuilder {
    pub fn gen_id(&self, func: Cow<FunctionSource>) -> usize {
        let mut indexes = self.assigned_indexes.borrow_mut();
        match indexes.get(&func) {
            None => {
                debug!("Generating new findex for {:?}", *func);
                let new_index = indexes.len();
                indexes.insert(func.into_owned(), new_index);
                new_index
            }
            Some(existing) => {
                debug!("Using existing findex for {:?}", *func);
                *existing
            }
        }
    }
}
*/
