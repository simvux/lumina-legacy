pub mod bridge;
mod r#if;
pub use r#if::If;
mod first;
pub use first::First;
mod value;
pub use value::Value;

#[derive(Debug, Clone)]
pub enum Entity {
    RustCall(u16, Vec<Entity>),
    FunctionCall(u32, Vec<Entity>),
    ParameterCall(u32, Vec<Entity>),
    IfExpression(self::If<Entity>),
    FirstStatement(self::First<Entity>),
    Parameter(u16),
    LambdaParam(u16),

    Inlined(Value),
    List(Vec<Entity>),
    Lambda(Vec<Entity>),
    LambdaPointer(Box<(Entity, Vec<Capturable>)>),

    Unimplemented,
    Unique,
}

#[derive(Debug, Clone)]
pub enum Capturable {
    ParentParam(usize),
    ParentWhere(usize),
    ParentLambda(usize),
}
