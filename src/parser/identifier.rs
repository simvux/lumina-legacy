use super::IdentSource;
use super::ParseFault;
use std::convert::TryFrom;
use std::fmt;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Identifier {
    pub path: Vec<String>,
    pub name: String,
    pub kind: IdentifierType,
}
impl Default for Identifier {
    fn default() -> Self {
        Self {
            path: vec![],
            name: String::new(),
            kind: IdentifierType::Normal,
        }
    }
}
impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum IdentifierType {
    Normal,
    Operator,
}
impl Default for IdentifierType {
    fn default() -> Self {
        IdentifierType::Normal
    }
}

impl From<String> for Identifier {
    fn from(s: String) -> Identifier {
        let kind = IdentifierType::try_from(s.as_str()).unwrap();
        Identifier {
            kind,
            name: s,
            path: vec![],
        }
    }
}

const OP_CHARS: &str = "!@#$%-+*/&?{}=;<>|";
pub const NAME_CHARS: &str = "abcdefghijklmnopqrstuvwxyz123456789_-";

impl TryFrom<&str> for IdentifierType {
    type Error = ParseFault;

    fn try_from(s: &str) -> Result<Self, ParseFault> {
        let mut iter = s.chars();

        // Discover what kind of identifier using the first char
        let first = iter
            .next()
            .ok_or_else(|| ParseFault::InvalidIdentifier(String::new(), IdentSource::Ident))?;
        let kind = if OP_CHARS.contains(first) {
            IdentifierType::Operator
        } else if NAME_CHARS.contains(first) {
            IdentifierType::Normal
        } else {
            return Err(ParseFault::InvalidIdentifier(
                s.to_string(),
                IdentSource::Ident,
            ));
        };

        // Verifies if the other characters are allowed for that kind
        for other in iter {
            match kind {
                IdentifierType::Normal => {
                    if !NAME_CHARS.contains(other) {
                        return Err(ParseFault::InvalidIdentifier(
                            s.to_owned(),
                            IdentSource::Ident,
                        ));
                    }
                }
                IdentifierType::Operator => {
                    if !OP_CHARS.contains(other) {
                        return Err(ParseFault::InvalidIdentifier(
                            s.to_owned(),
                            IdentSource::Operator,
                        ));
                    }
                }
            }
        }

        Ok(kind)
    }
}

impl Identifier {
    pub fn raw(name: &str) -> Identifier {
        let fst = name.chars().next();
        let kind = if OP_CHARS.contains(|c| Some(c) == fst) {
            IdentifierType::Operator
        } else {
            IdentifierType::Normal
        };
        Identifier {
            path: Vec::new(),
            name: name.into(),
            kind,
        }
    }

    pub fn is_operator(&self) -> bool {
        self.kind == IdentifierType::Operator
    }
}

impl TryFrom<&str> for Identifier {
    type Error = ParseFault;

    fn try_from(s: &str) -> Result<Identifier, ParseFault> {
        let mut path = s.split(':').map(|s| s.to_owned()).collect::<Vec<String>>();
        let name = path.pop().unwrap();
        let kind = IdentifierType::try_from(name.as_str())?;
        Ok(Identifier { name, path, kind })
    }
}
