use std::collections::VecDeque;
use std::fmt;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Nothing,
    Int(i64),
    Float(f64),
    Bool(bool),
    // I want *actually* captured here, not to be captured.
    Function(Box<(super::Entity, Vec<Value>)>),

    // TODO: This is a terrible way to handle lists.
    // For one, we're storing meta type information for each member of the list
    // rather than just once. So we're wasting a ton of space.
    // This also bloats the full Size of the Value enum, wasting even more memory
    //
    // We should instead expose raw pointers to leaf and write the List decl in leaf.
    //
    // Issues caused by handling this in leaf would be that it breaks our current
    // move/borrow/replace memory system in the runner. Although I suppose implementing
    // the clone/drop/whatever traits manually can fix that somewhat. We'll see
    List(Box<VecDeque<Value>>),
}

impl Default for Value {
    fn default() -> Value {
        Value::Nothing
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Nothing => write!(f, "_"),
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Function(box (body, _captured)) => write!(f, "f({})", body),
            Value::List(list) => {
                write!(f, "[")?;
                for (i, entity) in list.iter().enumerate() {
                    write!(f, "{}", entity)?;
                    if i + 1 != list.len() {
                        write!(f, ",")?;
                    }
                }
                write!(f, "]")?;
                Ok(())
            }
        }
    }
}
