use super::Type;
use crate::ir::Capturable;
use crate::parser::{Identifier, MaybeType};
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Hash, Clone, Eq, PartialEq, Debug)]
pub enum Identifiable {
    Param(usize),
    Captured(usize),
    Where((usize, usize), usize),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct IdentMeta {
    pub r#type: MaybeType,
    pub use_counter: u16,
    pub ident: Identifiable,
}

#[derive(Default, Clone, Eq, PartialEq)]
pub struct Meta {
    pub fid: usize,
    pub ident: Identifier,
    pub return_type: Type,
    pub identifiers: HashMap<String, IdentMeta>,
}
impl Hash for Meta {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.fid.hash(state);
        self.ident.hash(state);
        self.return_type.hash(state);
        for (k, v) in self.identifiers.iter() {
            k.hash(state);
            v.r#type.hash(state);
            v.ident.hash(state);
            v.use_counter.hash(state);
        }
    }
}

impl Meta {
    // Modifies the `use_counter` of self by comparison, and dumps those of which were captured
    // Used for lambda's
    pub fn was_used(&mut self, other: &Meta) -> Vec<Capturable> {
        let mut captured = Vec::new();
        for (name, im) in self.identifiers.iter_mut() {
            if let Identifiable::Param(n) = im.ident {
                let newer_im = &other.identifiers[name];
                if let Identifiable::Captured(_) = newer_im.ident {
                    if newer_im.use_counter > 0 {
                        // The parameter was used in the lambda! We need to capture it then
                        im.use_counter += newer_im.use_counter;
                        captured.push(Capturable::ParentParam(n));
                    }
                }
            }
        }
        captured
    }
    // Turns parameters into captured values and appends new parameters. This is used when encountering lambdas.
    pub fn lambda_swap(&mut self, params: &[Identifier], known_types: &[MaybeType]) {
        let mut captured_n = 0;
        for im in self.identifiers.values_mut() {
            if let Identifiable::Param(_) = im.ident {
                im.ident = Identifiable::Captured(captured_n);
                captured_n += 1;
                im.use_counter = 0;
            }
        }
        for (i, ident) in params.iter().enumerate() {
            self.identifiers.insert(
                ident.name.clone(),
                IdentMeta {
                    use_counter: 0,
                    r#type: match ident.anot.as_ref().and_then(|a| a.get(0)) {
                        Some(t) => MaybeType::Known(t.clone()),
                        None => known_types[i].clone(),
                    },
                    ident: Identifiable::Param(i),
                },
            );
        }
    }

    pub fn try_use(&mut self, name: &str) -> Option<&IdentMeta> {
        let identmeta = self.identifiers.get_mut(name)?;
        identmeta.use_counter += 1;
        Some(identmeta)
    }
}

impl fmt::Debug for Meta {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "fn {}:{} {} ({} -> {})  |{}|",
            self.fid,
            self.ident.name,
            self.identifiers
                .iter()
                .filter_map(|(k, v)| if let Identifiable::Param(_) = v.ident {
                    Some(k.as_str())
                } else {
                    None
                })
                .collect::<Vec<_>>()
                .join(" "),
            self.identifiers
                .values()
                .filter_map(|v| if let Identifiable::Param(_) = v.ident {
                    Some(v.r#type.to_string())
                } else {
                    None
                })
                .collect::<Vec<_>>()
                .join(" "),
            self.return_type,
            self.identifiers
                .iter()
                .filter_map(|(k, v)| {
                    if let Identifiable::Captured(_) = v.ident {
                        Some(k.as_str())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
                .join(" ")
        )
    }
}
