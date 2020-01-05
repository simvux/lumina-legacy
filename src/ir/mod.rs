pub mod bridge;
use bridge::Bridged;
mod r#if;
pub use r#if::If;
mod first;
pub use first::First;
mod value;
pub use value::Value;

use std::fmt;

#[derive(Debug, Clone)]
pub enum Entity {
    RustCall(Bridged, Vec<Entity>),
    FunctionCall(u32, Vec<Entity>),
    ParameterCall(u32, Vec<Entity>),
    IfExpression(self::If<Entity>),
    FirstStatement(self::First<Entity>),
    Parameter(u16),
    Captured(u16),

    Inlined(Value),
    List(Vec<Entity>),
    Lambda(Vec<Entity>, Vec<Capturable>),
    LambdaPointer(Box<(Entity, Vec<Capturable>)>),

    Unimplemented,
    Unique,
}

impl Entity {
    pub fn non_forking(&self) -> bool {
        match self {
            Entity::Inlined(_) => true,
            Entity::Captured(_) => true,
            Entity::Parameter(_) => true,
            Entity::List(_) => true,
            _ => false,
        }
    }

    pub fn inline_param(self, parent_params: &[Entity]) -> Self {
        match self {
            Self::Parameter(n) => parent_params[n as usize].clone(),
            _ => self,
        }
    }
}

impl PartialEq for Entity {
    fn eq(&self, _: &Self) -> bool {
        panic!("ET: Invalid comparison between types. This is a temporary error for when we have a more powerfull leaf type system")
    }
}
impl PartialOrd for Entity {
    fn partial_cmp(&self, _: &Self) -> Option<std::cmp::Ordering> {
        panic!("ET: Invalid comparison between types. This is a temporary error for when we have a more powerfull leaf type system")
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Capturable {
    ParentParam(usize),
    ParentWhere(usize),
    ParentLambda(usize),
}

impl fmt::Display for Capturable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Capturable::ParentParam(i) => write!(f, "p{{{}}}", i),
            Capturable::ParentLambda(i) => write!(f, "l{{{}}}", i),
            Capturable::ParentWhere(i) => write!(f, "w{{{}}}", i),
        }
    }
}

impl fmt::Display for Entity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Entity::RustCall(id, params) => {
                write!(f, "(builtin-")?;
                bridge::name_from_funcid(f, *id)?;
                for p in params.iter() {
                    write!(f, " {}", p)?
                }
                write!(f, ")")
            }
            Entity::FunctionCall(findex, params) => {
                write!(f, "(call-{}", findex)?;
                for p in params.iter() {
                    write!(f, " {}", p)?
                }
                write!(f, ")")
            }
            Entity::ParameterCall(i, params) => {
                write!(f, "(pcall-{}", i)?;
                for p in params.iter() {
                    write!(f, " {}", p)?
                }
                write!(f, ")")
            }
            Entity::IfExpression(branches) => branches.fmt(f),
            Entity::FirstStatement(branches) => branches.fmt(f),
            Entity::Parameter(i) => write!(f, "p{}", i),
            Entity::Captured(i) => write!(f, "cb{}", i),
            Entity::Inlined(v) => write!(f, "{}", v),
            Entity::List(list) => {
                write!(f, "[")?;
                if list.is_empty() {
                    return write!(f, "]");
                }
                for entity in list[0..list.len() - 1].iter() {
                    write!(f, "{},", entity)?;
                }
                write!(f, "{}]", list.last().unwrap())
            }
            Entity::Lambda(params, captures) => {
                write!(f, "(lambda")?;
                for p in params[1..].iter() {
                    write!(f, " {}", p)?;
                }
                write!(f, " {{")?;
                for cap in captures.iter() {
                    write!(f, "{} ", cap)?;
                }
                write!(f, "}} ")?;
                write!(f, ">>")?;
                write!(f, " {})", params.first().unwrap())
            }
            Entity::LambdaPointer(box (body, captures)) => {
                write!(f, "(lambda")?;
                write!(f, " {{")?;
                for cap in captures.iter() {
                    write!(f, " {}", cap)?;
                }
                write!(f, "}} ")?;
                write!(f, ">>")?;
                write!(f, " {})", body)
            }
            Entity::Unimplemented => write!(f, "unimp"),
            Entity::Unique => write!(f, "unique"),
        }
    }
}
