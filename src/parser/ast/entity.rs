use crate::parser::{tokenizer::Inlinable, Anot, Identifier, Tracked, Type};
use std::fmt;

#[derive(Clone, Debug)]
pub enum Callable {
    Func(Anot<Identifier, Type>),
    Builtin(Anot<Identifier, Type>),
    Lambda(Vec<Anot<Identifier, Type>>, Box<Tracked<Entity>>),
}

impl fmt::Display for Callable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Callable::Func(ident) => write!(f, "{}", ident),
            Callable::Builtin(ident) => write!(f, "builtin:{}", ident),
            Callable::Lambda(param_names, body) => write!(
                f,
                "(\\{} -> {})",
                param_names
                    .iter()
                    .map(|t| fmt::Display::to_string(t))
                    .collect::<Vec<_>>()
                    .join(" "),
                body
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Passable {
    Func(Anot<Identifier, Type>),
    Value(Inlinable),
    PartialFunc(Callable, Vec<Tracked<Entity>>),
    Lambda(Vec<Anot<Identifier, Type>>, Box<Tracked<Entity>>),
}

impl fmt::Display for Passable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Passable::Func(ident) => write!(f, "#{}", ident),
            Passable::Value(v) => write!(f, "#{}", v),
            Passable::PartialFunc(c, params) => write!(
                f,
                "#({} {})",
                c,
                params
                    .iter()
                    .map(|t| fmt::Display::to_string(&t.inner))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Passable::Lambda(param_names, body) => write!(
                f,
                "#(\\{} -> {})",
                param_names
                    .iter()
                    .map(|t| fmt::Display::to_string(t))
                    .collect::<Vec<_>>()
                    .join(" "),
                body
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Entity {
    Call(Callable, Vec<Tracked<Entity>>),
    Pass(Passable),
    If(
        Vec<(Tracked<Entity>, Tracked<Entity>)>,
        Box<Tracked<Entity>>,
    ),
    First(Vec<Tracked<Entity>>),
    Record(Anot<Identifier, Type>, Vec<(String, Tracked<Entity>)>),
    Lambda(Vec<Anot<Identifier, Type>>, Box<Tracked<Entity>>),
    List(Vec<Tracked<Entity>>),
    Inlined(Inlinable),
    SingleIdent(Anot<Identifier, Type>),

    // TODO: I want to add some meta info here so I can actually decribe what's unimplemented.
    // Is it root of function? Print that the entire function is implemented. Is just an if branch?
    // Then that etc.
    Unimplemented,
}

impl Default for Entity {
    fn default() -> Self {
        Self::Unimplemented
    }
}

impl From<Callable> for Entity {
    fn from(c: Callable) -> Entity {
        match c {
            Callable::Func(ident) => Entity::SingleIdent(ident),
            Callable::Builtin(ident) => Entity::SingleIdent(ident),
            Callable::Lambda(params, body) => Entity::Lambda(params, body),
        }
    }
}

impl fmt::Display for Entity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Entity::Call(c, params) => write!(
                f,
                "({} {})",
                c,
                params
                    .iter()
                    .map(|t| fmt::Display::to_string(t))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Entity::Pass(passable) => write!(f, "{}", passable),
            Entity::If(branches, else_do) => write!(
                f,
                "(if {} else {})",
                branches
                    .iter()
                    .map(|(c, d)| format!("{} then {}", c, d))
                    .collect::<Vec<_>>()
                    .join(" elif "),
                else_do
            ),
            Entity::First(branches) => write!(
                f,
                "(first {} then {})",
                branches[0..branches.len() - 1]
                    .iter()
                    .map(|t| fmt::Display::to_string(t))
                    .collect::<Vec<_>>()
                    .join(" and "),
                branches.last().unwrap()
            ),
            Entity::Record(name, fields) => write!(
                f,
                "{{ {} . {} }}",
                name,
                fields
                    .iter()
                    .map(|entity| format!("{} {}", entity.0, entity.1))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Entity::Lambda(param_names, body) => write!(
                f,
                "(\\{} -> {})",
                param_names
                    .iter()
                    .map(|t| fmt::Display::to_string(t))
                    .collect::<Vec<_>>()
                    .join(" "),
                body
            ),
            Entity::List(entries) => write!(
                f,
                "[{}]",
                entries
                    .iter()
                    .map(|t| fmt::Display::to_string(t))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Entity::Inlined(v) => write!(f, "{}", v),
            Entity::SingleIdent(ident) => write!(f, "{}", ident),
            Entity::Unimplemented => write!(f, "???"),
        }
    }
}
