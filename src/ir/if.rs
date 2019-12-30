use std::fmt;

#[derive(Debug, Clone)]
pub struct If<T> {
    inner: Vec<T>,
}

impl<T> If<T> {
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            inner: Vec::with_capacity(cap),
        }
    }

    pub fn condition(&self, mut index: usize) -> &T {
        index = (index * 2) + 1;
        &self.inner[index]
    }
    pub fn evaluation(&self, mut index: usize) -> &T {
        index = (index * 2) + 2;
        &self.inner[index]
    }
    pub fn branches(&self) -> usize {
        (self.inner.len() - 1) / 2
    }

    pub fn r#else(&self) -> &T {
        &self.inner[0]
    }
}

impl<T> From<Vec<T>> for If<T> {
    fn from(v: Vec<T>) -> If<T> {
        If { inner: v }
    }
}

impl<T: fmt::Display> fmt::Display for If<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(if ")?;
        for i in 0..self.branches() {
            write!(f, "?{} :{}", self.condition(i), self.evaluation(i))?;
        }
        write!(f, " else {})", self.r#else())
    }
}
