use super::{r#type, Attr, ParseFault, Type};
use std::convert::TryFrom;
use std::fmt;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Identifier<A> {
    pub path: Vec<String>,
    pub name: String,
    pub kind: IdentifierType,
    pub anot: Option<Vec<A>>,
}
impl<A> Default for Identifier<A> {
    fn default() -> Self {
        Self {
            path: vec![],
            name: String::new(),
            kind: IdentifierType::Normal,
            anot: None,
        }
    }
}
impl<A: fmt::Display + Clone> fmt::Display for Identifier<A> {
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

impl<A> Identifier<A> {
    pub fn raw(name: &str) -> Identifier<A> {
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

    pub fn swap_anot<B>(self, anot: Option<Vec<B>>) -> Identifier<B> {
        Identifier {
            anot,
            name: self.name,
            kind: self.kind,
            path: self.path,
        }
    }

    fn base_from<I: Iterator<Item = char>>(source: &mut I) -> Result<Identifier<A>, ()> {
        let mut s = source
            .take_while(|&c| c != '<' && c != ' ')
            .collect::<String>();
        // Edge-case for operators that start with `<`
        if s.is_empty() {
            s.push('<');
            s.push_str(&source.collect::<String>());
        }
        if s.is_empty() {
            unreachable!();
        }
        let mut path = s
            .split(|c| c == ':')
            .map(|s| s.to_owned())
            .collect::<Vec<String>>();
        let name = path.pop().unwrap();
        let mut iter = name.chars();

        let first = iter.next().unwrap();
        let kind = if OP_CHARS.contains(|c| c == first) {
            IdentifierType::Operator
        } else {
            IdentifierType::Normal
        };

        Ok(Identifier {
            path,
            name,
            kind,
            anot: None,
        })
    }
}

impl Identifier<Attr> {
    pub fn is_targeted_sys(&self) -> bool {
        let mut yes = true;
        match &self.anot {
            None => yes,
            Some(anots) => {
                for anot in anots.iter() {
                    if let Some(matches) = anot.is_targeted_sys() {
                        if matches {
                            return true;
                        } else {
                            yes = false;
                        }
                    }
                }
                yes
            }
        }
    }
}

pub trait Anotable<T> {
    fn annotate<I: Iterator<Item = char>>(iter: I) -> Result<Vec<T>, ParseFault>;
}

impl Anotable<Type> for Type {
    fn annotate<I: Iterator<Item = char>>(mut iter: I) -> Result<Vec<Type>, ParseFault> {
        let anot = r#type::annotation(&mut iter).expect("ET");
        assert_eq!(iter.next(), None);
        Ok(anot)
    }
}

impl Anotable<String> for String {
    fn annotate<I: Iterator<Item = char>>(mut iter: I) -> Result<Vec<String>, ParseFault> {
        let mut anots = Vec::new();
        loop {
            let (segment, drop) = gather_anot_seg(&mut iter);
            if !segment.is_empty() {
                anots.push(segment.trim().to_string());
            }
            match drop {
                ',' => {}
                '>' => return Ok(anots),
                '\u{0000}' => return Ok(anots),
                other => panic!("ET: Unexpected {}", other),
            }
        }
    }
}
fn gather_anot_seg<I: Iterator<Item = char>>(iter: &mut I) -> (String, char) {
    let mut buf = String::new();
    loop {
        let c = match iter.next() {
            None => return (buf, 0 as char),
            Some(c) => c,
        };
        match c {
            '<' => {
                buf.push(c);
                iter.take_while(|c| *c != '>').for_each(|c| buf.push(c));
                buf.push('>');
            }
            '>' | ',' => return (buf, c),
            _ => buf.push(c),
        }
    }
}

impl<A: Anotable<A>> TryFrom<&str> for Identifier<A> {
    type Error = ();

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let mut iter = s.chars();
        let mut base = Identifier::base_from(&mut iter)?;
        let anot = A::annotate(iter).unwrap();
        if !anot.is_empty() {
            base.anot = Some(anot);
        }
        Ok(base)
    }
}

impl TryFrom<Identifier<String>> for Identifier<Type> {
    type Error = ParseFault;

    fn try_from(ident: Identifier<String>) -> Result<Identifier<Type>, Self::Error> {
        match &ident.anot {
            Some(anots) => {
                let mut buf = Vec::new();
                for segm in anots.iter() {
                    buf.push(Type::try_from(segm.as_str())?)
                }
                Ok(ident.swap_anot(Some(buf)))
            }

            None => Ok(ident.swap_anot(None)),
        }
    }
}

impl TryFrom<Identifier<String>> for Identifier<Attr> {
    type Error = ParseFault;

    fn try_from(ident: Identifier<String>) -> Result<Identifier<Attr>, Self::Error> {
        match &ident.anot {
            Some(anots) => {
                let mut buf = Vec::new();
                for segm in anots.iter() {
                    buf.push(Attr::try_from(segm.as_str())?)
                }
                Ok(ident.swap_anot(Some(buf)))
            }

            None => Ok(ident.swap_anot(None)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normal() {
        let s = "add";
        let anot: Identifier<Type> = Identifier::try_from(s).unwrap();
        assert_eq!(
            anot,
            Identifier {
                path: vec![],
                name: "add".into(),
                kind: IdentifierType::Normal,
                anot: None,
            }
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
        let anot: Identifier<Type> = Identifier::try_from(s).unwrap();
        assert_eq!(
            anot,
            Identifier {
                path: vec!["from".into(), "then".into()],
                name: "add".into(),
                kind: IdentifierType::Normal,
                anot: None,
            }
        );
    }

    #[test]
    fn oper() {
        let s = "+";
        let anot: Identifier<Type> = Identifier::try_from(s).unwrap();
        assert_eq!(
            anot,
            Identifier {
                path: vec![],
                name: "+".into(),
                kind: IdentifierType::Operator,
                anot: None,
            }
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
