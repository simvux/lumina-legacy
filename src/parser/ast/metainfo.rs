use super::Type;
use crate::ir::Capturable;
use crate::parser::{Attr, FunctionBuilder, Identifier, MaybeType, ParseFault};
use std::convert::TryFrom;
use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Hash, Clone, Eq, PartialEq, Debug)]
pub enum Identifiable {
    Param(usize),
    Captured(usize),
    Where((usize, usize), usize),
}

#[derive(Clone, Eq, Debug)]
pub struct IdentMeta {
    pub r#type: MaybeType,
    pub use_counter: u16,
    pub ident: Identifiable,
}
impl PartialEq for IdentMeta {
    fn eq(&self, other: &Self) -> bool {
        self.r#type == other.r#type && self.ident == other.ident
    }
}
impl Hash for IdentMeta {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.r#type.hash(state);
        self.ident.hash(state);
    }
}

#[derive(Default, Clone, Eq, PartialEq, Hash)]
pub struct Meta {
    pub fid: usize,
    pub ident: Identifier<Attr>,
    pub return_type: Type,
    pub identifiers: Vec<(String, IdentMeta)>,
}

impl Meta {
    pub fn identifier(&self, name: &str) -> Option<&IdentMeta> {
        self.identifiers
            .iter()
            .find_map(|(n, im)| if n == name { Some(im) } else { None })
    }
    pub fn identifier_mut(&mut self, name: &str) -> Option<&mut IdentMeta> {
        self.identifiers
            .iter_mut()
            .find_map(|(n, im)| if n == name { Some(im) } else { None })
    }
    // Modifies the `use_counter` of self by comparison, and dumps those of which were captured
    // Used for lambda's
    pub fn was_used(&mut self, other: &Meta) -> Vec<Capturable> {
        let mut captured = Vec::new();
        for (name, im) in self.identifiers.iter_mut() {
            if let Identifiable::Param(n) = im.ident {
                let newer_im = &other.identifier(name).unwrap();
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
    pub fn lambda_swap(&mut self, params: &[Identifier<Type>], known_types: &[MaybeType]) {
        let mut captured_n = 0;
        for (_, im) in self.identifiers.iter_mut() {
            if let Identifiable::Param(_) = im.ident {
                im.ident = Identifiable::Captured(captured_n);
                captured_n += 1;
                im.use_counter = 0;
            }
        }
        for (i, ident) in params.iter().enumerate() {
            self.identifiers.push((
                ident.name.clone(),
                IdentMeta {
                    use_counter: 0,
                    r#type: match ident.anot.as_ref().and_then(|a| a.get(0)) {
                        Some(t) => MaybeType::Known(t.clone()),
                        None => known_types[i].clone(),
                    },
                    ident: Identifiable::Param(i),
                },
            ));
        }
    }

    pub fn try_use(&mut self, name: &str) -> Option<&IdentMeta> {
        let identmeta = self.identifier_mut(name)?;
        identmeta.use_counter += 1;
        Some(identmeta)
    }

    pub fn identifiers_from(
        func: &FunctionBuilder,
        given: &[MaybeType],
    ) -> Vec<(String, IdentMeta)> {
        let params = func
            .parameter_names
            .iter()
            .cloned()
            .zip(func.parameter_types.iter().cloned())
            .enumerate()
            .map(|(i, (n, _t))| {
                // TODO: Might wanna add an assertion of `t` and given[i] here
                (
                    n,
                    IdentMeta {
                        use_counter: 0,
                        ident: Identifiable::Param(i),
                        r#type: given[i].clone(),
                    },
                )
            });
        /*
        let wheres = func.wheres.iter().enumerate().map(|(i, (n, e))| IdentMeta {
            use_counter: 0,
            ident: Identifiable::Where
        })
        return params.chain(wheres).collect();
        */
        params.collect()
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
                .iter()
                .filter_map(|(_, v)| if let Identifiable::Param(_) = v.ident {
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
