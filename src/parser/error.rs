use super::{
    ast, ast::Identifiable, tokenizer::Operator, FileSource, Identifier, IdentifierType, Key,
    MaybeType, Parser, RawToken, Tracked, Type,
};
use std::convert::Into;
use std::fmt;
use std::fs::File;
use std::io;
use std::io::Read;
use std::ops::Add;
use std::path::PathBuf;
use termion::color;

#[derive(Debug)]
pub struct ParseError {
    pub source_index: usize,
    pub variant: ParseFault,
    pub source_code: Option<String>,
    pub module_fid: Option<usize>,
    pub module_name: Option<FileSource>,
    pub parser: Option<Parser>,
}

// lul. No but seriously I forgot how to properly annotate `B` for the got format
const NO: Option<&bool> = None;

impl ParseError {
    pub fn new(i: usize, err: ParseFault) -> Self {
        Self {
            source_index: i,
            variant: err,
            source_code: None,
            module_fid: None,
            module_name: None,
            parser: None,
        }
    }

    pub fn fallback_index(mut self, fallback: usize) -> Self {
        if self.source_index == 0 {
            self.source_index = fallback;
        }
        self
    }
    pub fn fallback_fid(mut self, fid: usize) -> Self {
        if self.module_fid.is_none() {
            self.module_fid = Some(fid);
        }
        self
    }

    pub fn load_source_code(mut self) -> Self {
        let parser = match &self.parser {
            None => {
                println!(
                    "Parser not appended to load source file in final stage of error pipeline"
                );
                return self;
            }
            Some(p) => p,
        };
        if self.source_code.is_some() {
            self
        } else {
            let filesource = if let Some(fs) = &self.module_name {
                fs
            } else if let Some(fid) = self.module_fid {
                self.module_name = Some(parser.modules[fid].module_path.clone());
                &parser.modules[fid].module_path
            } else {
                println!(
                    "Insufficent information to load source file in final stage of error pipeline"
                );
                return self;
            };
            let mut source_code = String::with_capacity(20);
            let path = filesource.to_pathbuf(&parser.environment);
            match File::open(&path) {
                Err(e) => {
                    eprintln!("leaf: cannot open source code file `{:?}`: {}", path, e);
                    return self;
                }
                Ok(mut f) => f.read_to_string(&mut source_code).unwrap(),
            };
            self.source_code = Some(source_code);
            self
        }
    }

    pub fn with_parser(mut self, parser: Parser) -> Self {
        self.parser = Some(parser);
        self
    }
}

impl<'a, T> Into<Result<T, ParseError>> for ParseError {
    fn into(self) -> Result<T, ParseError> {
        Err(self)
    }
}

#[derive(Debug)]
pub enum ParseFault {
    EndedWhileExpecting(Vec<String>),
    GotButExpected(RawToken, Vec<String>),
    ModuleLoadNotFound(Vec<String>),
    ModuleLoadFailed(PathBuf, io::ErrorKind),
    NotValidType(String),
    MissingRightSideOperator(Box<(RawToken, Operator, RawToken)>),
    EndedMissingRightSideOperator(RawToken, Operator),
    InvalidIdentifier(String, IdentSource),
    InvalidPath(Vec<String>),
    BridgedWrongPathLen(Vec<String>),
    BridgedFunctionNotFound(Identifier<Type>),
    BridgedFunctionNoMode(u8),
    ParameterlessLambda,
    TypeNotFound(usize, Identifier<Type>),
    RecordWithEnum(usize, Identifier<Type>),
    Unexpected(RawToken),
    UnexpectedWantedParameter(RawToken),
    Unmatched(Key),
    ModuleNotImported(String),
    CannotInferType(char),
    InvalidClosure(ast::Entity),
    InvalidClosureT(RawToken),
    UnrecognizedAttribute(String),
    EmptyParen,
    FirstMissingThen,
    FirstWantedThen(RawToken),
    ParamCannotTakeParameters(Box<(Type, Vec<MaybeType>)>),
    IfMissingElse,
    IfMissingThen,
    IfWantedThen(RawToken),
    IfDoubleElse,
    IfConditionNotBoolean(Box<(ast::Entity, Type)>),
    ParamCallMismatch(Box<(Vec<Type>, Type, Vec<MaybeType>)>),
    ParamCallAmountMismatch(Box<(Vec<Type>, Type, Vec<MaybeType>)>),
    IfBranchTypeMismatch(
        Box<(
            Vec<Type>,
            (
                Vec<(Tracked<ast::Entity>, Tracked<ast::Entity>)>,
                Tracked<ast::Entity>,
            ),
        )>,
    ),
    ListMissingClose,
    OpNoIdent,
    OpWantedIdent(RawToken),
    InvalidParameterName(String),
    PipeIntoVoid,
    EmptyListType,
    ListEntryTypeMismatch(Type, Type, usize),
    FnTypeReturnMismatch(Box<ast::Meta>, Type),
    FunctionNotFound(Identifier<Type>, usize),
    FunctionVariantNotFound(Identifier<Type>, Vec<MaybeType>, usize),
    FunctionConversionRequiresAnnotation(
        Identifier<Type>,
        std::collections::HashMap<Vec<Type>, usize>,
    ),
    IdentifierNotFound(String),
    Internal,
}

#[derive(Debug)]
pub enum IdentSource {
    Module,
    FunctionDeclName,
    Ident,
}

impl<'a> ParseFault {
    pub fn into_err(self, i: usize) -> ParseError {
        ParseError::new(i, self)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let parser = if let Some(parser) = self.parser.as_ref() {
            if let Some(source) = self.source_code.as_ref() {
                if self.source_index != 0 {
                    let (raw_line, arrow, line_number) = locate_line(source, self.source_index);
                    let line = String::from_utf8(raw_line.to_vec()).unwrap();
                    let module_path = self
                        .module_name
                        .as_ref()
                        .expect("Module name not appended to error in final scope");

                    write!(
                        f,
                        "{g}leaf{n} {}{g}:{n}\n {y}{}{n} | {}\n{}\n",
                        module_path,
                        line_number,
                        line,
                        std::iter::repeat(' ')
                            .take(arrow + 3 + line_number.to_string().len())
                            .collect::<String>()
                            .add(color::Red.fg_str())
                            .add("-^-")
                            .add(color::Reset.fg_str()),
                        y = color::Yellow.fg_str(),
                        g = color::Green.fg_str(),
                        n = color::Reset.fg_str(),
                    )?;
                }
                parser
            } else {
                println!("{:?}", self.variant);
                return Ok(());
            }
        } else {
            println!("{:?}", self.variant);
            return Ok(());
        };

        use ParseFault::*;
        match &self.variant {
            ParameterlessLambda => write!(f, "This lambda wasn't given any parameters"),
            IdentifierNotFound(name) => write!(f, "Could not find a function, constant or parameter named `{}`", name),
            UnrecognizedAttribute(name) => write!(f, "`{}` isn't a known function attribute", name),
            InvalidPath(entries) => write!(f, "`{}` is not a valid module path", entries.join(":")),
            BridgedWrongPathLen(entries) => write!(f, "`{}` wrong length of path", entries.join(":")),
            BridgedFunctionNotFound(ident) => write!(f, "No bridged function named `{}`", ident),
            BridgedFunctionNoMode(c) => write!(f, "Bridged path mode doesn't exist, got `{}`", c),
            TypeNotFound(_fid, ident) => write!(f, "Type `{}` not found", ident.name),
            RecordWithEnum(_fid, ident) => write!(f, "You're trying to construct a record however, `{}` is an enum and not a struct", ident),
            FunctionConversionRequiresAnnotation(ident, variants) => {
                write!(f, "This function conversion requires a type annotation. I don't know which of these variants to use.\n  {}", variants.keys().map(|params| {
                    format_function_header(&ident.name, Some(params), NO)
                }).collect::<Vec<_>>().join("\n  "))
            }
            ParamCallMismatch(box (takes, _gives, got)) => {

                write!(f, "The function call originating from this parameter has the wrong types of arguments\n wanted  {}\n but got {}", 
                    format_function_parameter(Some(takes), NO),
                    format_function_parameter(Some(got), NO),
                    )
            }
            ParamCallAmountMismatch(box (takes, _gives, got)) => {
                write!(f, "The function call originating from this parameter wanted {} argument(s) but got {}\n wanted {}\n but got {}",
                    takes.len(),
                    got.len(),
                    format_function_parameter(Some(takes), NO),
                    format_function_parameter(Some(got), NO),
                    )
            }
            ParamCannotTakeParameters(box (got_t, params)) => {
                write!(f, "This parameter of type `{}` was given these parameters\n {}\nBut {}'s cannot take parameters", got_t, format_function_parameter(Some(params), NO), got_t)
            }
            Unexpected(t) => write!(f, "Unexpected `{}`", t),
            Unmatched(k) => write!(f, "Unmatched `{}`", k),
            CannotInferType(c) => write!(f, "Cannot infer type for `{}`", c),
            ListEntryTypeMismatch(got, wanted, entry_index) => {
                let num = match entry_index {
                    0 => "first".to_string(),
                    1 => "second".to_string(),
                    2 => "third".to_string(),
                    4 => "fourth".to_string(),
                    5 => "fifth".to_string(),
                    _ => entry_index.to_string().add("th"),
                };
                write!(f, "The {} entry of this list doesn't result in the same type as the previous ones\n Expected `{}`\n But got `{}`", num, wanted, got)
            },
            EndedWhileExpecting(expected) => match expected.len() {
                0 => panic!("None expected"),
                1 => write!(f, "I was expecting `{}` but the file ended here", expected[0]),
                2 => write!(
                    f,
                    "I was expecting `{}` or `{}` but the file ended here",
                    expected[0], expected[1]
                ),
                3 => write!(
                    f,
                    "I was expecting `{}` or `{}` or `{}` but the file ended here",
                    expected[0], expected[1], expected[2]
                ),
                _ => write!(
                    f,
                    "The file ended but I was expecting any of: \n`{}`",
                    expected
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join("\n  ")
                ),
            },
            GotButExpected(got, expected) => match expected.len() {
                0 => panic!("None expected"),
                1 => write!(f, "I was expecting `{}` but got `{}`", expected[0], got),
                2 => write!(
                    f,
                    "I was expecting `{}` or `{}` but got {}",
                    expected[0], expected[1], got
                ),
                _ => write!(
                    f,
                    "Got `{}` but I was expecting any of: \n {}",
                    got,
                    expected
                        .iter()
                        .map(|t| format!("| {}", t))
                        .collect::<Vec<String>>()
                        .join("\n ")
                ),
            },
            FunctionNotFound(ident, fid) => {
                let module = &parser.modules[*fid];
                write!(
                    f,
                    "Function `{}` not found in `{}` (or prelude)",
                    ident, module.module_path
                )
            }
            FunctionVariantNotFound(ident, params, fid) => {
                // TODO: This only shows from one module. 
                let (fid, variants) = parser.functions_named(*fid, ident).unwrap()[0];
                let module = &parser.modules[fid];
                
                match variants.len() {
                    1 => {
                        let (wanted_params, funcid) = {
                            variants.iter().next().unwrap()
                        };
                        let wfuncb = &module.functions[*funcid];
                        let mut mismatches = Vec::with_capacity(2);
                        for (i, param) in wanted_params.iter().enumerate() {
                            let got = match params.get(i) {
                                None => break, // TODO: What if `got` has less parmater then `wanted`?
                                Some(t) => t,
                            };
                            if let MaybeType::Known(got) = got {
                            if param != got {
                                mismatches.push(i);
                            } 
                            } else {
                                mismatches.push(i);
                            }
                        }
                        // TODO: Not hacking this
                        if mismatches.len() == 1 {
                            let i = mismatches[0];
                            write!(
                                f,
                                "Type mismatch. Wanted `{}` but got {}\n {}\n {}",
                                wanted_params[i],
                                params[i],
                                format_header(&ident.name, ident.kind.clone(), Some(params), None),
                                format_header(&wfuncb.name.name, ident.kind.clone(), Some(wanted_params.iter().cloned().map(MaybeType::Known).collect::<Vec<_>>().as_slice()) ,None),
                            )
                        } else {
                            write!(f, "No function named `{}` takes these parameters\n  {}\n perhaps you meant to use?\n  {}",
                                &ident.name,
                                format_header(&ident.name, ident.kind.clone(), Some(&params), None),
                                format_header(&wfuncb.name.name, ident.kind.clone(), Some(&wanted_params.iter().cloned().map(MaybeType::Known).collect::<Vec<_>>().as_slice()), None),
                                )
                        }
                    }
                    _ => {
                        write!(f, "No function named `{}` takes these parameters\n  {}\n i did however find these variants\n  {}",
                            &ident.name,
                            format_header(&ident.name, ident.kind.clone(), Some(params.as_slice()), None),
                            variants.keys().map(|params| format_header(&ident.name, ident.kind.clone(), Some(params.iter().cloned().map(MaybeType::Known).collect::<Vec<_>>().as_slice()), None)).collect::<Vec<String>>().join("\n  ")
                            )
                    },
                }
            }
            ModuleLoadNotFound(entries) => write!(
                f,
                "Module `{}` not found in project folder or leafpath",
                entries.join(":")
            ),
            ModuleLoadFailed(path, err) => write!(
                f,
                "Found but unable to read `{}`: {:?}",
                path.to_string_lossy(),
                err,
            ),
            NotValidType(ident) => write!(f, "`{}` is not a valid type identifier", ident),
            MissingRightSideOperator(box (_left, op, right)) => write!(
                f,
                "Missing the right value for the operator `{}`, instead got this `{}`",
                op, right
            ),
            EndedMissingRightSideOperator(_left, op) => write!(f, "The function ended but I was still looking for the right side value for the operator `{}`", op),
            InvalidIdentifier(ident, identsource) => {
                write!(f, "`{}` is not a valid identifier", ident)?;
                match identsource {
                    IdentSource::Module => write!(f, "for a module name"),
                    IdentSource::FunctionDeclName => write!(f, "for a function"),
                    IdentSource::Ident => Ok(()),
                }
            },
            FirstMissingThen => {
                write!(f, "This first statement doesn't have a `then` branch. I was looking for something ressembling\n first ...\n then  ...")
            },
            FirstWantedThen(got) => {
                write!(f, "I was expecting an `and` or `then` branch for this `first` statement but instead I got `{}`", got)
            }
            InvalidClosure(got) => {
                write!(f, "This `{}` cannot be converted into a closure", describe(&got)) 
            }
            InvalidClosureT(got) => {
                write!(f, "This `{}` cannot be converted into a closure", got)
            }
            EmptyParen => write!(f, "Empty parenthesis aren't allowed. For unit value use the type `nothing` and value `_`"),
            IfMissingThen => write!(f, "This if expression doesn't have a `then` branch, I was looking for something ressembling\n if ...\n  then ...\n  else ..."),
            IfWantedThen(got) => write!(f, "This if expression was expecting a `then` branch but instead it got `{}`\nI was looking for something ressembling\n if ...\n  then ...\n  else ...", got),
            IfMissingElse => write!(f, "This if expression doesn't have an `else` branch, I was looking for something ressembling\n if ...\n  then ...\n  else ..."),
            IfDoubleElse => write!(f, "This if expression has two `else` branches, how would I know which one to use?"),
            IfConditionNotBoolean(box (_got, gott)) => write!(f, "The condition for this if branch isn't an boolean\n Wanted `bool` but got `{}`", gott),
            IfBranchTypeMismatch(box (types, (branches, else_do))) => write!(f, "ERROR TODO: Properly display if statement typing.\n These branches don't return the same value\n{:?}", types),
            ListMissingClose => write!(f, "This list open is missing a matching `]` to close it"),
            OpNoIdent => write!(f, "You need to provide an identifier for this operator"),
            OpWantedIdent(a) => write!(f, "Wanted identifier for the operator but got `{}`", a),
            InvalidParameterName(name) => write!(f, "`{}` is not a valid identifier for a parmater", name),
            PipeIntoVoid => write!(f, "This pipe doesn't lead to anywhere, perhaps you need to remove it?"),
            EmptyListType => write!(f, "I know that this is a list but you need to say what type the contents of the list will be\n such as [a] or [int]"),
            UnexpectedWantedParameter(got) => write!(f, "I was expecting to see something to use as parameter but got `{}`", got),
            ModuleNotImported(mod_name) => write!(f, "Module `{}` is not imported", mod_name),
            FnTypeReturnMismatch(meta, got) => {
                let p_types = meta.identifiers.iter().filter_map(|(_name, im)| {
                    if let Identifiable::Param(_n) = im.ident {
                        Some(im.r#type.clone())
                    } else { None }
                }).collect::<Vec<MaybeType>>();
                // let p_types = funcb.parameter_types.iter().cloned().map(MaybeType::Known).collect::<Vec<_>>();
                write!(f, "This function returns the wrong value. Acording to its type signature it should return `{}`\n  {}\nbut instead it returns `{}`",
                meta.return_type,
                format_header(&meta.ident.name, meta.ident.kind.clone(),
                if p_types.is_empty() { 
                    None 
                } else { 
                    Some(p_types.as_slice()) 
                }, 
                Some(&meta.return_type)),
                got,
            )},
            Internal => write!(f, "Internal leaf error"),
        }
    }
}

fn format_function_parameter<A: fmt::Display, B: fmt::Display>(
    params: Option<&[A]>,
    returns: Option<&B>,
) -> String {
    format!(
        "({y}{}{r} -> {y}{}{r})",
        params
            .map(|a| a
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" "))
            .unwrap_or_else(|| String::from("...")),
        returns
            .map(|mt| mt.to_string())
            .unwrap_or_else(|| String::from("...")),
        y = color::Yellow.fg_str(),
        r = color::Reset.fg_str(),
    )
}

fn format_header(
    name: &str,
    kind: IdentifierType,
    params: Option<&[MaybeType]>,
    returns: Option<&Type>,
) -> String {
    match kind {
        IdentifierType::Normal => format_function_header(name, params, returns),
        IdentifierType::Operator => {
            let params = params.unwrap();
            format_operator_header(name, &params, returns)
        }
    }
}

fn format_function_header<A: fmt::Display, B: fmt::Display>(
    name: &str,
    params: Option<&[A]>,
    returns: Option<&B>,
) -> String {
    format!(
        "{y}fn{r} {} {}",
        name,
        format_function_parameter(params, returns),
        y = color::Yellow.fg_str(),
        r = color::Reset.fg_str()
    )
}

fn format_operator_header<A: fmt::Display, B: fmt::Display>(
    opname: &str,
    params: &[A],
    ret: Option<&B>,
) -> String {
    format!(
        "{y}operator{r} {} {}",
        opname,
        format_function_parameter(Some(params), ret),
        y = color::Yellow.fg_str(),
        r = color::Reset.fg_str(),
    )
}

// TODO: This breaks on non-ascii utf-8 characters
fn locate_line(source: &str, index: usize) -> (&[u8], usize, usize) {
    if index >= source.len() {
        return (b" ", 0, 1);
    }
    let mut line_number = 1;
    for (i, c) in source.chars().enumerate() {
        if i == index {
            break;
        }
        if c == '\n' {
            line_number += 1;
        }
    }

    // Pretty dirty hack but if the error is on the actual newline character itself we want to
    // technically search for the start of the previous line
    let mut i = if source.as_bytes()[index] == b'\n' {
        index - 1
    } else {
        index
    };
    let start_i = loop {
        let c = source.as_bytes()[i];
        if c == b'\n' || i == 0 {
            break i + 1;
        }
        i -= 1;
    };

    let mut i = index;
    let end_i = loop {
        let c = match source.as_bytes().get(i) {
            None => break i,
            Some(c) => *c,
        };
        if c == b'\n' {
            break i;
        }
        i += 1;
    };
    (
        &source.as_bytes()[start_i..end_i],
        index - start_i,
        line_number,
    )
}

fn describe(entity: &ast::Entity) -> &str {
    use ast::Entity;
    match entity {
        Entity::Call(_, _) => "function call",
        Entity::Pass(_) => "closure",
        Entity::If(_, _) => "if expression",
        Entity::First(_) => "first statement",
        Entity::Lambda(_, _) => "lambda",
        Entity::List(_) => "list",
        Entity::Inlined(_) => "value",
        Entity::SingleIdent(_) => "identifier",
        Entity::Unimplemented => "unimplemented",
        Entity::Record(_, _) => "record",
    }
}
