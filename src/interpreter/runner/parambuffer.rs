use crate::ir::Value;

#[derive(Debug)]
pub enum ParamBuffer<'p> {
    Borrowed(&'p [Value]),
    Owned(Vec<Value>),
    SingleBorrowed(&'p Value),
    SingleOwned(Value),
}
use ParamBuffer::*;

impl<'p> ParamBuffer<'p> {
    pub fn borrow(&'p self) -> ParamBuffer<'p> {
        match self {
            Borrowed(a) => ParamBuffer::Borrowed(a),
            Owned(a) => ParamBuffer::Borrowed(&a),
            SingleOwned(a) => ParamBuffer::SingleBorrowed(a),
            SingleBorrowed(a) => ParamBuffer::SingleBorrowed(a),
        }
    }
    pub fn consume(&mut self) -> ParamBuffer<'p> {
        match self {
            Borrowed(a) => ParamBuffer::Borrowed(a),
            Owned(a) => {
                let mut v = Vec::new();
                std::mem::swap(&mut v, a);
                ParamBuffer::Owned(v)
            }
            SingleBorrowed(a) => ParamBuffer::SingleBorrowed(a),
            SingleOwned(a) => ParamBuffer::SingleOwned(std::mem::take(a)),
        }
    }
    pub fn param_borrow(&'p self, i: usize) -> &'p Value {
        match self {
            Borrowed(a) => &a[i],
            Owned(a) => &a[i],
            SingleBorrowed(a) => a,
            SingleOwned(a) => &a,
        }
    }
    pub fn param_consume(&mut self, i: usize) -> Value {
        match self {
            Borrowed(a) => a[i].clone(),
            Owned(a) => std::mem::take(&mut a[i]),
            SingleBorrowed(a) => a.clone(),
            SingleOwned(a) => std::mem::take(a),
        }
    }
}

impl From<Vec<Value>> for ParamBuffer<'_> {
    fn from(v: Vec<Value>) -> Self {
        Self::Owned(v)
    }
}
