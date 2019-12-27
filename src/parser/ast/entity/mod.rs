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

fn format_params(list: &[Tracked<Entity>]) -> String {
    list.iter()
        .map(|p| fmt::Display::to_string(p))
        .collect::<Vec<_>>()
        .join(" ")
}

impl fmt::Display for Entity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Entity::Call(callable, params) => match callable {
                Callable::Func(ident) => {
                    if ident.is_operator() {
                        let nothing = Tracked::new(Entity::Inlined(Inlinable::Nothing));
                        write!(
                            f,
                            "{} {} {}",
                            params.get(0).unwrap_or(&nothing),
                            ident.name,
                            params.get(1).unwrap_or(&nothing)
                        )
                    } else {
                        write!(f, "{} {}", ident.name, format_params(params))
                    }
                }
                Callable::Builtin(ident) => {
                    write!(f, "builtin:{} {}", ident.name, format_params(params))
                }
                Callable::Lambda(lambda_params, body) => write!(
                    f,
                    "(\\{} -> {}) {}",
                    lambda_params
                        .iter()
                        .cloned()
                        .map(|i| i.name)
                        .collect::<Vec<_>>()
                        .join(" "),
                    body,
                    format_params(params)
                ),
            },
            _ => unimplemented!(),
        }
    }
}
