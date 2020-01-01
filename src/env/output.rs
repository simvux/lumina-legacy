use std::default::Default;

#[derive(Debug)]
pub struct Output {
    pub ir: bool,
    pub ast_full: bool,
    pub ast_entry: bool,
    pub run: bool,
    pub help: bool,
}

#[cfg(debug_assertions)]
impl Default for Output {
    fn default() -> Self {
        Output {
            ir: false,
            ast_full: false,
            ast_entry: true,
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
            ast_full: false,
            ast_entry: false,
            run: true,
            help: false,
        }
    }
}
