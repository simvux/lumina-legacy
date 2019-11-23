use super::generics::*;
use super::{irbuilder::FunctionSource, ParseFault, Parser, Type, PRELUDE_FID};
use std::collections::HashMap;

pub trait Seekable {
    fn seek(&self, parser: &Parser) -> Result<FunctionSource, ParseFault>;
}

// TODO: Generics
// TODO: From Anonotation
impl Seekable for (usize, &str, &[Type]) {
    fn seek(&self, parser: &Parser) -> Result<FunctionSource, ParseFault> {
        let variants = match parser
            .modules
            .get(self.0)
            .ok_or_else(|| ParseFault::ModuleNotImported(self.1.to_owned()))?
            .function_ids
            .get(self.1)
        {
            Some(variants) => variants,
            // Function name not found
            None => {
                if self.0 == PRELUDE_FID {
                    return Err(ParseFault::Internal);
                } else {
                    return Seekable::seek(&(PRELUDE_FID, self.1, self.2), parser).map_err(
                        |mut e| {
                            // Overwrite prelude fid with this fid
                            if let ParseFault::FunctionNotFound(_, fid) = &mut e {
                                *fid = self.0
                            }
                            e
                        },
                    );
                }
            }
        };
        let found = match variants.get(self.2) {
            Some(funcid) => {
                debug!("Found function {}:{} from {}\n", self.0, funcid, self.1);
                Ok(FunctionSource::from((self.0, *funcid)))
            }
            // Function variant not found
            None => {
                if self.0 == PRELUDE_FID {
                    Err(ParseFault::Internal)
                } else {
                    Seekable::seek(&(PRELUDE_FID, self.1, self.2), parser)
                }
            }
        };
        match found {
            Ok(source) => Ok(source),
            Err(_) => {
                // Lets try some generic matches
                if let Some((funcid, generics)) = generic_search(variants, self.2) {
                    let mut func = Seekable::seek(&(self.0, funcid), parser)
                        .unwrap()
                        .func(parser)
                        .clone();
                    generics.replace_all(&mut func);
                    Ok(FunctionSource::Owned(self.0, func))
                } else {
                    Err(ParseFault::FunctionVariantNotFound(
                        self.1.into(),
                        self.2.to_vec(),
                        self.0,
                    ))
                }
            }
        }
    }
}
impl Seekable for (usize, &[String], &[Type]) {
    fn seek(&self, parser: &Parser) -> Result<FunctionSource, ParseFault> {
        match self.1.len() {
            1 => Seekable::seek(&(self.0, self.1[0].as_str(), self.2), parser),
            2 => Seekable::seek(
                &(self.0, self.1[0].as_str(), self.1[1].as_str(), self.2),
                parser,
            ),
            _ => panic!("ET: Function path's can only be 1 or 2 part"),
        }
    }
}
impl Seekable for (usize, &str, &str, &[Type]) {
    fn seek(&self, parser: &Parser) -> Result<FunctionSource, ParseFault> {
        let fid = parser.modules[self.0]
            .imports
            .get(self.1)
            .copied()
            .ok_or_else(|| ParseFault::Internal)?;
        Seekable::seek(&(fid, self.2, self.3), parser)
    }
}
impl Seekable for (usize, usize) {
    fn seek(&self, _parser: &Parser) -> Result<FunctionSource, ParseFault> {
        Ok(FunctionSource::from(*self))
    }
}

impl Parser {
    pub fn find_func<S: Seekable + std::fmt::Debug>(
        &self,
        source: S,
    ) -> Result<FunctionSource, ParseFault> {
        debug!("Trying to find {:?}\n", &source);
        source.seek(self)
    }
}
