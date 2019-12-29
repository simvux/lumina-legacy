use std::collections::VecDeque;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Nothing,
    Int(i64),
    Float(f64),
    Bool(bool),
    // I want *actually* captured here, not to be captured.
    // TODO: Lazy clone
    Function(Box<(super::Entity, Vec<Value>)>),

    // TODO: This is a terrible way to handle lists.
    // For one, we're storing meta type information for each member of the list
    // rather than just once. So we're wasting a ton of space.
    // This also bloats the full Size of the Value enum, wasting even more memory
    //
    // We should instead expose raw pointers to leaf and write the List decl in leaf.
    List(Box<VecDeque<Value>>),
}

impl Default for Value {
    fn default() -> Value {
        Value::Nothing
    }
}
/*

impl PartialEq for Value {
    // TODO: This is temporary. When we have a more powerful typesystem in leaf we'll implement
    // this there instead.
    fn eq(&self, other: &Self) -> bool {
        match self {
            Value::Nothing => {
                if let Value::Nothing = other {
                    true
                } else {
                    false
                }
            }
            Value::Int(x) => {
                if let Value::Int(y) = other {
                    x == y
                } else {
                    false
                }
            }
            Value::Float(x) => {
                if let Value::Float(y) = other {
                    x == y
                } else {
                    false
                }
            }
        }
    }
}
*/
