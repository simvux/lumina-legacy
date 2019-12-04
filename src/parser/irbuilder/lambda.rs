use super::fsource::FunctionSource;
use super::Type;
use crate::ir::Capturable;
use std::collections::HashMap;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Identifiable {
    Param(usize),
    Function(FunctionSource),
    Lambda(usize, Option<Type>),
    Where(usize),
}

impl From<Identifiable> for Capturable {
    fn from(identifiable: Identifiable) -> Capturable {
        use Identifiable::*;
        match identifiable {
            Param(n) => Capturable::ParentParam(n),
            Where(n) => Capturable::ParentWhere(n),
            Lambda(n, _) => Capturable::ParentLambda(n),
            _ => unreachable!("{:?}", identifiable),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IdentPool<'a> {
    lambdas: HashMap<&'a str, (Option<Type>, usize)>, // usize = nesting when created

    // usize = usage_counter
    used: HashMap<Identifiable, usize>,
    nesting: usize,
}

impl<'a> IdentPool<'a> {
    pub fn new() -> Self {
        IdentPool {
            lambdas: HashMap::new(),
            used: HashMap::new(),
            nesting: 0,
        }
    }

    pub fn tag_usage(&mut self, index: Identifiable) {
        match self.used.get_mut(&index) {
            None => {
                self.used.insert(index, 1);
            }
            Some(used_before) => {
                *used_before += 1;
            }
        }
    }
    pub fn tag_lambda(&mut self, name: &'a str, r#type: Option<Type>) {
        self.lambdas.insert(name, (r#type, self.nesting));
    }

    pub fn lambda(&self, name: &str) -> Option<(Option<Type>, usize)> {
        self.lambdas.get(name).cloned()
    }

    pub fn captured(&self, previous: &Self) -> Vec<Capturable> {
        let mut buf = Vec::new();
        for (ident, used) in self.used.iter() {
            let should_capture = match previous.used.get(ident).copied() {
                Some(a) => *used > a,
                None => true,
            };
            if should_capture {
                buf.push(ident.clone().into());
            }
        }
        buf
    }
}
