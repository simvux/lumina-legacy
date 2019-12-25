use std::fmt;
use std::ops::Deref;

#[derive(Clone, Default)]
pub struct Tracked<T> {
    tracker: usize,
    pub inner: T,
}

impl<T: fmt::Debug> fmt::Debug for Tracked<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.inner.fmt(f)
    }
}
impl<T: fmt::Display> fmt::Display for Tracked<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl<T> Tracked<T> {
    pub fn new(inner: T) -> Self {
        Self { inner, tracker: 0 }
    }
    pub fn set(mut self, tracker: usize) -> Self {
        self.tracker = tracker;
        self
    }
    pub fn sep(self) -> (T, usize) {
        (self.inner, self.tracker)
    }
    pub fn untrack(self) -> T {
        self.inner
    }
    pub fn pos(&self) -> usize {
        self.tracker
    }
    pub fn swap<A: Into<B>, B>(self, other: Tracked<A>) -> Tracked<B> {
        let tracker = self.tracker;
        Tracked {
            tracker,
            inner: other.inner.into(),
        }
    }
}
