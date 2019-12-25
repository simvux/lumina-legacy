#[derive(Debug, Clone)]
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
    List(Vec<Value>),
}

impl Default for Value {
    fn default() -> Value {
        Value::Nothing
    }
}
