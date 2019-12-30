mod identifier;
use crate::parser::tokenizer::Inlinable;
use crate::parser::Tracked;
use std::fmt;

pub use identifier::{Identifier, IdentifierType, NAME_CHARS};

#[derive(Clone, Debug)]
pub enum Callable {
    Func(Identifier),
    Builtin(Identifier),
    Lambda(Vec<Identifier>, Box<Tracked<Entity>>),
}

#[derive(Clone, Debug)]
pub enum Passable {
    Func(Identifier),
    Value(Inlinable),
    PartialFunc(Callable, Vec<Tracked<Entity>>),
    Lambda(Vec<Identifier>, Box<Tracked<Entity>>),
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
    Lambda(Vec<Identifier>, Box<Tracked<Entity>>),
    List(Vec<Tracked<Entity>>),
    Inlined(Inlinable),
    SingleIdent(Identifier),

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
