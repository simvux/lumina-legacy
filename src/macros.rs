#![macro_use]
macro_rules! assume {
    ($path:path, $v:expr) => {
        if let Some(($path(a), pos)) = $v.map(|a| a.sep()) {
            (a, pos)
        } else {
            unreachable!()
        }
    };
}
