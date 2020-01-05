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

#[allow(unused)]
macro_rules! debug {
    ($category:expr, $($arg:tt)*) => (
        #[allow(unused_imports)]
        use termion::color::{Fg, Green, Reset, Yellow};
        #[cfg(debug_assertions)]
        print!(" {}{} {}->{} ", Fg(Green), $category, Fg(Yellow), Fg(Reset));
        #[cfg(debug_assertions)]
        println!($($arg)*);
    )
}
