use crate::parser::r#type;
use crate::parser::Type;
use std::convert::TryFrom;
use std::fmt;

#[derive(PartialEq, Eq, Hash, Clone, Debug, Default)]
pub struct Identifier {
    pub path: Vec<String>,
    pub name: String,
    pub kind: IdentifierType,
    pub anot: Option<Vec<Type>>,
}
impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let anot = self
            .anot
            .clone()
            .unwrap_or_else(Vec::new)
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(
            f,
            "{}{}",
            self.name,
            if anot == "" {
                "".into()
            } else {
                format!("<{}>", anot)
            }
        )
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

const OP_CHARS: &str = "!@#$%-+*/&?{}=;<>|";
pub const NAME_CHARS: &str = "abcdefghijklmnopqrstuvwxyz123456789_-";

impl Identifier {
    pub fn with_annotation<I: Iterator<Item = char>>(mut self, mut iter: I) -> Result<Self, ()> {
        let anot = r#type::annotation(&mut iter).expect("ET");
        let anot = if anot.is_empty() { None } else { Some(anot) };
        self.anot = anot;
        Ok(self)
    }

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
            anot: None,
            kind,
        }
    }

    pub fn is_operator(&self) -> bool {
        self.kind == IdentifierType::Operator
    }
}

impl TryFrom<&str> for Identifier {
    type Error = ();

    fn try_from(s: &str) -> Result<Identifier, Self::Error> {
        let mut path = s
            .split(|c| c == ':')
            .map(|s| s.to_owned())
            .collect::<Vec<String>>();
        let name = path.pop().unwrap();
        let mut iter = name.chars();

        let mut name = String::new();
        let first = iter.next().unwrap();
        let kind = if OP_CHARS.contains(|c| c == first) {
            IdentifierType::Operator
        } else {
            IdentifierType::Normal
        };
        name.push(first);
        while let Some(c) = iter.next() {
            if c == '<' {
                return Identifier {
                    path,
                    kind,
                    name,
                    anot: None,
                }
                .with_annotation(iter);
            }
            match kind {
                IdentifierType::Normal => {
                    if !NAME_CHARS.contains(|a| a == c) {
                        panic!("ET: {} not allowed in identifier", c);
                    }
                }
                IdentifierType::Operator => {
                    if !OP_CHARS.contains(|a| a == c) {
                        panic!("ET: {} not allowed in operator name", c);
                    }
                }
            }
            name.push(c);
        }
        Ok(Identifier {
            path,
            kind,
            name,
            anot: None,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normal() {
        let s = "add";
        assert_eq!(
            Identifier::try_from(s),
            Ok(Identifier {
                path: vec![],
                name: "add".into(),
                kind: IdentifierType::Normal,
                anot: None,
            })
        );
    }
    #[test]
    fn normal_single_anot() {
        let s = "add<int>";
        assert_eq!(
            Identifier::try_from(s),
            Ok(Identifier {
                path: vec![],
                name: "add".into(),
                kind: IdentifierType::Normal,
                anot: Some(vec![Type::Int])
            })
        );
    }
    #[test]
    fn normal_multi_anot() {
        let s = "add<int, float, int>";
        assert_eq!(
            Identifier::try_from(s),
            Ok(Identifier {
                path: vec![],
                name: "add".into(),
                kind: IdentifierType::Normal,
                anot: Some(vec![Type::Int, Type::Float, Type::Int]),
            })
        );
    }
    #[test]
    fn normal_list_anot() {
        let s = "add<[int]>";
        assert_eq!(
            Identifier::try_from(s),
            Ok(Identifier {
                path: vec![],
                name: "add".into(),
                kind: IdentifierType::Normal,
                anot: Some(vec![Type::List(Box::new(Type::Int))]),
            })
        );
    }

    #[test]
    fn normal_pathed() {
        let s = "from:then:add";
        assert_eq!(
            Identifier::try_from(s),
            Ok(Identifier {
                path: vec!["from".into(), "then".into()],
                name: "add".into(),
                kind: IdentifierType::Normal,
                anot: None,
            })
        );
    }

    #[test]
    fn oper() {
        let s = "+";
        assert_eq!(
            Identifier::try_from(s),
            Ok(Identifier {
                path: vec![],
                name: "+".into(),
                kind: IdentifierType::Operator,
                anot: None,
            })
        );
    }
    #[test]
    fn oper_single_anot() {
        let s = "+<int>";
        assert_eq!(
            Identifier::try_from(s),
            Ok(Identifier {
                path: vec![],
                name: "+".into(),
                kind: IdentifierType::Operator,
                anot: Some(vec![Type::Int]),
            })
        );
    }
    #[test]
    fn oper_multi_anot() {
        let s = "+<int, float, int>";
        assert_eq!(
            Identifier::try_from(s),
            Ok(Identifier {
                path: vec![],
                name: "+".into(),
                kind: IdentifierType::Operator,
                anot: Some(vec![Type::Int, Type::Float, Type::Int]),
            })
        );
    }
}
