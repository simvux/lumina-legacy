use super::{FunctionBuilder, Identifier, ParseFault, Type};
use crate::env::Environment;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::path::PathBuf;

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum FileSource {
    Project(Vec<String>),
    Leafpath(Vec<String>),
    Prelude,
}

pub struct ParseModule {
    //                     identifer       parameters
    pub function_ids: HashMap<String, HashMap<Vec<Type>, usize>>,
    pub functions: Vec<FunctionBuilder>,

    pub types: HashMap<String, usize>,
    pub type_fields: Vec<Vec<(String, Type)>>,
    pub imports: HashMap<String, usize>,

    pub module_path: FileSource,
}

impl ParseModule {
    pub fn new(module_path: FileSource) -> Self {
        Self {
            function_ids: HashMap::new(),
            functions: Vec::new(),
            types: HashMap::new(),
            type_fields: Vec::new(),
            imports: HashMap::new(),
            module_path,
        }
    }
    pub fn get_import(&self, name: &str) -> Result<usize, ParseFault> {
        self.imports
            .get(name)
            .cloned()
            .ok_or_else(|| ParseFault::ModuleNotImported(name.to_owned()))
    }
}

impl FileSource {
    pub fn join(self, next: String) -> Self {
        match self {
            FileSource::Project(mut levels) => {
                levels.push(next);
                FileSource::Project(levels)
            }
            FileSource::Leafpath(mut levels) => {
                levels.push(next);
                FileSource::Leafpath(levels)
            }
            FileSource::Prelude => panic!("Use statements in prelude unsupported"),
        }
    }
    pub fn pop(&mut self) -> Option<String> {
        match self {
            FileSource::Project(levels) => levels.pop(),
            FileSource::Leafpath(levels) => levels.pop(),
            FileSource::Prelude => panic!("Use statements in prelude unsupported"),
        }
    }

    pub fn to_pathbuf<'a>(&'a self, env: &Environment) -> PathBuf {
        match self {
            FileSource::Project(levels) => {
                let mut path = env.entrypoint.parent().unwrap().join(levels.join("/"));
                path.set_extension("lf");
                path
            }
            FileSource::Leafpath(levels) => {
                let mut path = env.leafpath.join("modules").join(levels.join("/"));
                path.set_extension("lf");
                path
            }
            FileSource::Prelude => panic!("Use statements in prelude unsupported"),
        }
    }
}

impl fmt::Display for FileSource {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FileSource::Project(levels) => write!(f, "project:{}", levels.join(":")),
            FileSource::Leafpath(levels) => write!(f, "leaf:{}", levels.join(":")),
            FileSource::Prelude => write!(f, "prelude"),
        }
    }
}

impl TryFrom<(&Identifier, &Environment)> for FileSource {
    type Error = ();

    fn try_from((ident, env): (&Identifier, &Environment)) -> Result<FileSource, Self::Error> {
        let mut from_project_path = env.entrypoint.parent().unwrap().to_owned();

        let mut file_postfix = ident.path.join("/");
        file_postfix.push('/');
        file_postfix.push_str(&ident.name);
        file_postfix.push_str(".lf");

        from_project_path.push(&file_postfix);

        if from_project_path.exists() {
            let mut buf = Vec::with_capacity(ident.path.len() + 1);
            for p in ident.path.iter().cloned() {
                buf.push(p);
            }
            buf.push(ident.name.clone());
            return Ok(FileSource::Project(buf));
        }

        let mut from_leaf_path = env.leafpath.clone();
        from_leaf_path.push("modules");
        from_leaf_path.push(file_postfix);

        if from_leaf_path.exists() {
            let mut buf = Vec::with_capacity(ident.path.len() + 1);
            for p in ident.path.iter().cloned() {
                buf.push(p);
            }
            buf.push(ident.name.clone());
            return Ok(FileSource::Leafpath(buf));
        }

        panic!("ET: File {:?} not found", ident.name);
    }
}
