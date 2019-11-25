use super::fsource::FunctionSource;
use super::Type;

#[derive(Clone)]
pub enum Identifiable<'a> {
    Param(usize),
    Function(FunctionSource),
    Lambda(usize, &'a [Type]),
    Where(usize),
}
