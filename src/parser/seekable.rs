use super::{irbuilder::FunctionSource, ParseFault, Parser, Type, PRELUDE_FID};

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
                    return Seekable::seek(&(PRELUDE_FID, self.1, self.2), parser)
                        .map_err(|_| ParseFault::FunctionNotFound(self.1.into(), self.0));
                }
            }
        };
        match variants.get(self.2) {
            Some(funcid) => {
                debug!("Found function {}:{} from {}", self.0, funcid, self.1);
                Ok(FunctionSource::from((self.0, *funcid)))
            }
            // Function variant not found
            None => {
                if self.0 == PRELUDE_FID {
                    Err(ParseFault::Internal)
                } else {
                    Seekable::seek(&(PRELUDE_FID, self.1, self.2), parser).map_err(|_| {
                        ParseFault::FunctionVariantNotFound(self.1.into(), self.2.to_vec(), self.0)
                    })
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
    pub fn find_func<S: Seekable>(&self, source: S) -> Result<FunctionSource, ParseFault> {
        source.seek(self)
    }
}
