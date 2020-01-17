use crate::env::Environment;
use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::rc::Rc;
use termion::{color, color::Fg};

mod tokenizer;
pub use tokenizer::{Header, Inlinable, Key, RawToken, Token, Tokenizer};
mod function;
mod leafmod;
pub use function::FunctionBuilder;

pub use leafmod::FileSource;
mod r#type;
pub use r#type::{CustomType, Enum, MaybeType, Struct, Type};
mod ast;
pub use ast::IrBuilder;
mod error;
mod operator;
pub use error::*;
pub use leafmod::ParseModule;
mod picker;
mod tracked;
pub use tracked::Tracked;
mod attribute;
pub use attribute::Attr;
mod identifier;
pub use identifier::{Identifier, IdentifierType, NAME_CHARS};

const PRELUDE_FID: usize = 0;

pub struct Parser {
    pub module_ids: HashMap<FileSource, usize>,
    pub modules: Vec<ParseModule>,
    environment: Rc<Environment>,
}

impl Parser {
    pub fn new(environment: Rc<Environment>) -> Self {
        Self {
            module_ids: HashMap::new(),
            modules: Vec::new(),
            environment,
        }
    }

    fn new_module(&mut self, source: FileSource) -> usize {
        match self.module_ids.get(&source) {
            Some(fid) => *fid,
            None => {
                let fid = self.module_ids.len();
                assert_eq!(fid, self.modules.len());
                self.module_ids.insert(source.clone(), fid);
                self.modules.push(ParseModule::new(source));
                fid
            }
        }
    }
    fn new_function(&mut self, fid: usize, funcb: FunctionBuilder) -> usize {
        let module = &mut self.modules[fid];
        let funcid = module.functions.len();
        match module.function_ids.get_mut(&funcb.name.name) {
            Some(existing) => {
                existing.insert(funcb.parameter_types.clone(), funcid);
                module.functions.push(funcb);
                funcid
            }
            None => {
                let mut hashmap = HashMap::with_capacity(1);
                let name = funcb.name.clone();
                hashmap.insert(funcb.parameter_types.clone(), funcid);
                module.function_ids.insert(name.name, hashmap);
                module.functions.push(funcb);
                funcid
            }
        };
        funcid
    }
    fn new_type(
        &mut self,
        fid: usize,
        ident: Identifier<Type>,
        fields: HashMap<String, Type>,
    ) -> usize {
        let module = &mut self.modules[fid];
        let typeid = module.types.len();
        let (name, type_args) = {
            // TODO: type_args
            (ident.name, vec![])
        };
        module.type_ids.insert(name, typeid);
        module
            .types
            .push(CustomType::Struct(Struct { type_args, fields }));
        typeid
    }
    fn new_enum(
        &mut self,
        fid: usize,
        ident: Identifier<Type>,
        fields: HashMap<String, Vec<Type>>,
    ) -> usize {
        let module = &mut self.modules[fid];
        let typeid = module.types.len();
        let (name, type_args) = {
            // TODO: type_args
            (ident.name, vec![])
        };
        if module.type_ids.insert(name, typeid).is_some() {
            panic!("ET: Type already exists");
        }
        module
            .types
            .push(CustomType::Enum(Enum { fields, type_args }));
        typeid
    }

    // Use leafpath and relative entrypoint path to find all `prelude` folders, and include those
    // in our global scope.
    pub fn read_prelude_source(&mut self) -> Result<(), ParseError> {
        // get the leafpath root directory
        let leafpath = &self.environment.leafpath;

        // get the directory containing our entrypoint .lf file
        let current_dir = self.environment.entrypoint.parent().unwrap();

        let leafpath_prelude = leafpath.join("prelude");
        let current_dir_prelude = current_dir.join("prelude");

        if leafpath_prelude.exists() {
            self.tokenize_prelude(leafpath_prelude.as_path())?;
        }

        if current_dir_prelude.exists() {
            self.tokenize_prelude(current_dir_prelude.as_path())?;
        }
        Ok(())
    }
    fn tokenize_prelude(&mut self, path: &Path) -> Result<(), ParseError> {
        for entry in path.read_dir().expect("Couldn't read prelude directory.") {
            let file_path = entry.expect("Prelude file path doesn't exist.").path();
            if file_path.extension() == Some(std::ffi::OsStr::new("lf")) {
                let mut source_code_buffer = String::new();
                std::fs::File::open(file_path)
                    .unwrap()
                    .read_to_string(&mut source_code_buffer)
                    .unwrap();

                self.tokenize(FileSource::Prelude, source_code_buffer.chars())
                    .map_err(|e| e.fallback_fid(PRELUDE_FID))?;
            }
        }
        Ok(())
    }

    // Turn a specified file into AST and load it into parser
    pub fn tokenize<I: Iterator<Item = char>>(
        &mut self,
        module_path: FileSource,
        source_code: I,
    ) -> Result<usize, ParseError> {
        let fid = self.new_module(module_path.clone());

        let mut tokenizer = Tokenizer::from(source_code.peekable());
        loop {
            let token = match tokenizer.next() {
                Some(t) => t,
                None => return Ok(fid),
            };
            let source_index = token.pos();
            match token.inner {
                RawToken::Header(h) => match h {
                    Header::Function => {
                        let mut funcb = FunctionBuilder::new().with_header(&mut tokenizer)?;
                        if funcb.name.is_targeted_sys() {
                            funcb
                                .parse_body(&mut tokenizer)
                                .map_err(|e| e.fallback_index(source_index).fallback_fid(fid))?;

                            self.new_function(fid, funcb);
                        } else {
                            tokenizer.skip_tokens_until(|t| match t {
                                RawToken::Header(_) => true,
                                _ => false,
                            });
                        }
                    }
                    Header::Operator => {
                        let mut funcb =
                            FunctionBuilder::new().with_header_operator(&mut tokenizer)?;
                        funcb
                            .parse_body(&mut tokenizer)
                            .map_err(|e| e.fallback_index(source_index).fallback_fid(fid))?;

                        self.new_function(fid, funcb);
                    }
                    Header::Type => {
                        let (type_name, fields) = r#type::r#struct::parse(&mut tokenizer)?;

                        self.new_type(fid, type_name, fields);
                    }
                    Header::Enum => {
                        let (type_name, variants) = r#type::r#enum::parse(&mut tokenizer)?;

                        self.new_enum(fid, type_name, variants);
                    }
                    Header::Use => {
                        let ident = match tokenizer.next().map(|t| t.inner) {
                            Some(RawToken::Identifier(ident)) => ident,
                            None => {
                                return ParseFault::EndedWhileExpecting(vec!["identifier".into()])
                                    .into_err(tokenizer.position - 1)
                                    .into()
                            }
                            Some(other) => {
                                panic!("ET: Unexpected thing after `use` keyword: {:?}", other)
                            }
                        };
                        let name = ident.name.clone();
                        let file_path = module_path.fork_from(
                            ident
                                .try_into()
                                .map_err(|e: ParseFault| e.into_err(source_index))?,
                            &*self.environment,
                        );

                        let usefid = self
                            .tokenize_import(file_path)
                            .map_err(|e| e.fallback_fid(fid).fallback_index(source_index))?;

                        // `usefid` is the ID which was assigned,
                        // it's already been inserted as a module in the parser
                        // but we need to add it to *this* modules imports.
                        self.modules[fid].imports.insert(name, usefid);
                    }
                },
                RawToken::NewLine => continue,
                _ => {
                    // We only want top-level headers here
                    return ParseError::new(
                        source_index,
                        ParseFault::GotButExpected(
                            token.inner,
                            vec!["fn".into(), "type".into(), "enum".into(), "where".into()],
                        ),
                    )
                    .fallback_fid(fid)
                    .into();
                }
            }
        }
    }

    fn tokenize_import(&mut self, file_path: FileSource) -> Result<usize, ParseError> {
        let pathbuf = file_path.to_pathbuf(&self.environment);
        let mut source_code = String::with_capacity(20);
        File::open(pathbuf.clone())
            .map_err(|e| ParseFault::ModuleLoadFailed(pathbuf.clone(), e.kind()).into_err(0))?
            .read_to_string(&mut source_code)
            .map_err(|e| ParseFault::ModuleLoadFailed(pathbuf, e.kind()).into_err(0))?;

        // Fork and tokenize this module first instead.
        let usefid = self.tokenize(file_path, source_code.chars())?;
        Ok(usefid)
    }

    pub fn variants_including_prelude(
        &self,
        self_fid: usize,
        ident: &Identifier<Type>,
    ) -> Result<(&HashMap<Vec<Type>, usize>, usize), ParseFault> {
        let fid = {
            match ident.path.len() {
                0 => self_fid,
                1 => {
                    let mod_name = &ident.path[0];
                    self.modules[self_fid].get_import(mod_name)?
                }
                _ => return Err(ParseFault::InvalidPath(ident.path.clone())),
            }
        };

        match self.modules[fid].function_ids.get(&ident.name) {
            Some(variants) => Ok((variants, fid)),
            None => {
                // Wasn't find, so lets try prelude variants
                if self_fid == fid && self_fid != PRELUDE_FID {
                    if let Some(variants) = self
                        .modules
                        .get(PRELUDE_FID)
                        .and_then(|m| m.function_ids.get(&ident.name))
                    {
                        return Ok((variants, fid));
                    }
                };
                Err(ParseFault::FunctionNotFound(ident.clone(), fid))
            }
        }
    }
}

impl fmt::Debug for Parser {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = self
            .module_ids
            .iter()
            .map(|(mod_name, fid)| {
                format!(
                    "{}#{} {} {}\n{:?}",
                    Fg(color::Green),
                    fid,
                    mod_name,
                    Fg(color::Reset),
                    &self.modules[*fid]
                )
                // }
            })
            .collect::<Vec<String>>()
            .join("\n ---\n\n");
        f.write_str(&s)
    }
}
