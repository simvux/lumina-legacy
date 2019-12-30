use std::default::Default;

#[derive(Debug)]
pub struct Output {
    pub ir: bool,
    pub ast: bool,
    pub run: bool,
    pub help: bool,
}

#[cfg(debug_assertions)]
impl Default for Output {
    fn default() -> Self {
        Output {
            ir: false,
            ast: false,
            run: true,
            help: false,
        }
    }
}

#[cfg(not(debug_assertions))]
impl Default for Output {
    fn default() -> Self {
        Output {
            ir: false,
            ast: false,
            run: true,
            help: false,
        }
    }
}
