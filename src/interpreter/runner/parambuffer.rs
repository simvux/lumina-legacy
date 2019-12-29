use crate::ir::Value;
use smallvec::SmallVec;
use std::fmt;
use std::ops::{Index, IndexMut};

pub enum ParamBuffer<'p> {
    Borrowed(&'p SmallVec<[Value; 4]>),
    Owned(SmallVec<[Value; 4]>),
}

impl Default for ParamBuffer<'_> {
    fn default() -> Self {
        Self::Owned(SmallVec::new())
    }
}

impl<'p> ParamBuffer<'p> {
    pub fn clone_param(&self, n: usize) -> Value {
        match self {
            ParamBuffer::Owned(vec) => vec[n].clone(),
            ParamBuffer::Borrowed(vec) => vec[n].clone(),
        }
    }
    fn as_slice(&self) -> &[Value] {
        match self {
            Self::Borrowed(v) => v.as_slice(),
            Self::Owned(v) => v.as_slice(),
        }
    }
}
impl<'p> Index<usize> for ParamBuffer<'p> {
    type Output = Value;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            Self::Borrowed(vec) => &vec[index],
            Self::Owned(vec) => &vec[index],
        }
    }
}

impl<'p> IndexMut<usize> for ParamBuffer<'p> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        match self {
            Self::Borrowed(_) => unreachable!(),
            Self::Owned(vec) => &mut vec[index],
        }
    }
}

impl<I: Iterator<Item = Value>> From<I> for ParamBuffer<'_> {
    fn from(iter: I) -> Self {
        let mut vec = SmallVec::new();
        for v in iter {
            vec.push(v);
        }
        Self::Owned(vec)
    }
}

impl Clone for ParamBuffer<'_> {
    fn clone(&self) -> Self {
        match self {
            ParamBuffer::Owned(vec) => ParamBuffer::Owned(vec.clone()),
            ParamBuffer::Borrowed(vec) => ParamBuffer::Owned((*vec).to_owned()),
        }
    }
}

impl fmt::Debug for ParamBuffer<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.as_slice())
    }
}
