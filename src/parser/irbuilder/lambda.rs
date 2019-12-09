use super::fsource::FunctionSource;
use super::Type;
use crate::ir::Capturable;
use crate::parser::FunctionBuilder;
use std::collections::HashMap;
use std::convert::TryFrom;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Identifiable {
    Param(usize),
    Function(FunctionSource),
    Lambda(usize, Type),
    Where(usize),
}

impl TryFrom<Identifiable> for Capturable {
    type Error = ();

    fn try_from(identifiable: Identifiable) -> Result<Capturable, Self::Error> {
        use Identifiable::*;
        match identifiable {
            Param(n) => Ok(Capturable::ParentParam(n)),
            Where(n) => Ok(Capturable::ParentWhere(n)),
            Lambda(n, _) => Err(()),
            _ => unreachable!("{:?}", identifiable),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IdentPool<'a> {
    //  lambdas: HashMap<&'a str, (Type, Identifiable)>, // usize = nesting when created

    // usize = usage_counter
    used: HashMap<Identifiable, usize>,
    types: HashMap<&'a str, (Identifiable, Type)>,
    nesting: usize,
}

impl<'a> IdentPool<'a> {
    pub fn new() -> Self {
        IdentPool {
            // lambdas: HashMap::new(),
            used: HashMap::new(),
            types: HashMap::new(),
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
    pub fn add(&mut self, name: &'a str, source: Identifiable, t: Type) {
        self.used.insert(source.clone(), 0);
        self.types.insert(name, (source, t));
    }
    pub fn get_captured<'b>(&'b self, name: &str) -> Option<&'b Identifiable> {
        self.types.get(name).map(|a| &a.0)
    }

    pub fn captured(&self, previous: &Self) -> Vec<Capturable> {
        let mut buf = Vec::new();
        for (ident, used) in self.used.iter() {
            let should_capture = match previous.used.get(ident).copied() {
                Some(a) => *used > a,
                None => true,
            };
            if should_capture {
                if let Ok(c) = Capturable::try_from(ident.clone()) {
                    buf.push(c);
                }
            }
        }
        buf
    }
    pub fn fill_from(&mut self, func: &'a FunctionBuilder, anot: &[Type], is_lambda: bool) {
        for (i, name) in func.parameter_names.iter().enumerate() {
            self.add(
                name,
                if is_lambda {
                    Identifiable::Lambda(i, anot.get(0).cloned().unwrap_or(Type::Infer))
                } else {
                    Identifiable::Param(i)
                },
                func.parameter_types[i].clone(),
            );
        }
    }
}
