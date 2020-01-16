use super::{ast, Identifier, MaybeType, ParseFault, Parser, Tracked, Type, PRELUDE_FID};
use std::cmp::Ordering;
use std::collections::HashMap;

impl Parser {
    pub fn find_func(
        &self,
        self_fid: usize,
        ident: &Identifier<Type>,
        params: &[MaybeType],
    ) -> Result<(usize, usize), ParseFault> {
        let variants = self.functions_named(self_fid, ident)?;
        let mut matches = matches_of(variants, |got| {
            if got.len() == params.len() {
                are_compatible(params, got)
            } else {
                false
            }
        });

        match matches.len() {
            0 => Err(ParseFault::FunctionVariantNotFound(
                ident.clone(),
                params.to_vec(),
                self_fid,
            )),
            1 => Ok(matches.remove(0)),
            _ => {
                matches.sort_by(|left, right| self.least_generics(left, right));
                Ok(matches.remove(0))
            }
        }
    }

    pub fn find_func_meta<'a>(
        &'a self,
        self_fid: usize,
        ident: &Identifier<Type>,
        params: &[MaybeType],
    ) -> Result<(&'a Tracked<ast::Entity>, ast::Meta), ParseFault> {
        let (fid, funcid) = self.find_func(self_fid, ident, params)?;
        let func = &self.modules[fid].functions[funcid];

        infer_all(params, &func.parameter_types);

        let mut return_type = func.returns.clone();
        let generics = find_generics(&func.parameter_types, params).unwrap();
        swap_generic(&mut return_type, &generics);

        let meta = ast::Meta {
            fid,
            ident: func.name.clone(),
            return_type,
            identifiers: ast::Meta::identifiers_from(func, params),
        };
        Ok((&func.body, meta))
    }

    // This is when we only know the *amount* of parameters, and not the actual parameters
    // themselves. Useful for when we are more likely to need annotations.
    pub fn find_func_only_suitable(
        &self,
        self_fid: usize,
        ident: &Identifier<Type>,
        atleast_params: usize,
    ) -> Result<(usize, usize), ParseFault> {
        let variants = self.functions_named(self_fid, ident)?;

        let mut matches = matches_of(variants, |got| got.len() >= atleast_params);
        let use_match = matches.remove(0);

        if matches.len() > 1 {
            panic!("Need type annotation");
        } else if matches.is_empty() {
            panic!("Function variant not found");
        }

        Ok(use_match)
    }

    // All variants in all reachable modules matching the function name
    pub fn functions_named<'a>(
        &'a self,
        self_fid: usize,
        ident: &Identifier<Type>,
    ) -> Result<Variants<'a>, ParseFault> {
        let mut all_variants = Vec::new();

        let fid = match ident.path.len() {
            0 => {
                if self_fid != PRELUDE_FID {
                    if let Ok(prelude_variants) = self.functions_named(PRELUDE_FID, ident) {
                        all_variants = prelude_variants;
                    }
                }
                self_fid
            }
            1 => self.modules[self_fid].imports[ident.path.last().as_ref().unwrap().as_str()],
            _ => panic!("ET: Invalid path length"),
        };
        let module = &self.modules[fid];

        if let Some(variants) = module.function_ids.get(&ident.name) {
            all_variants.push((fid, variants))
        };
        if all_variants.is_empty() {
            return Err(ParseFault::FunctionNotFound(ident.clone(), self_fid));
        }
        Ok(all_variants)
    }

    fn least_generics(&self, left: &(usize, usize), right: &(usize, usize)) -> Ordering {
        let left_f = &self.modules[left.0].functions[left.1];
        let right_f = &self.modules[right.0].functions[right.1];
        right_f
            .number_of_generics()
            .cmp(&left_f.number_of_generics())
    }
}

type Variants<'a> = Vec<(usize, &'a HashMap<Vec<Type>, usize>)>;

fn matches_of(variants: Variants, check: impl Fn(&[Type]) -> bool) -> Vec<(usize, usize)> {
    let mut matches = Vec::new();
    for (fid, variants) in variants.iter() {
        variants
            .iter()
            .filter(|(params, _)| check(params))
            .for_each(|(_, funcid)| matches.push((*fid, *funcid)));
    }
    matches
}

fn are_compatible(want: &[MaybeType], got: &[Type]) -> bool {
    if want.len() != got.len() {
        return false;
    }
    let mut generics: HashMap<u8, Type> = HashMap::new();
    want.iter().zip(got.iter()).all(|(want, got)| match want {
        MaybeType::Infer(t) => {
            if let Some(existing) = t.borrow().as_ref() {
                return generic_cmp(&mut generics, existing, got);
            }
            let decoded = got.clone().decoded(&generics);
            if let Type::Generic(_) = &decoded {
                // We can't infer here.
                return false;
            }
            true
        }
        MaybeType::Known(existing) => generic_cmp(&mut generics, existing, got),
    })
}

fn generic_cmp(generics: &mut HashMap<u8, Type>, left: &Type, right: &Type) -> bool {
    match &right {
        Type::Generic(n) => {
            if let Some(existing) = generics.get(n) {
                existing == left
            } else {
                generics.insert(*n, left.clone());
                true
            }
        }
        Type::List(box Type::Generic(n)) => {
            if let Some(existing) = generics.get(n) {
                Type::List(Box::new(existing.clone())) == *left
            } else if let Type::List(box left_inner_t) = left {
                generics.insert(*n, left_inner_t.clone());
                true
            } else {
                false
            }
        }
        Type::Function(box (takes, gives)) => {
            if let Type::Function(box (left_takes, left_gives)) = left {
                if takes.len() != left_takes.len() {
                    return false;
                }
                let params_ok = takes
                    .iter()
                    .enumerate()
                    .all(|(i, inner_right)| generic_cmp(generics, &left_takes[i], inner_right));
                if !params_ok {
                    return false;
                }
                generic_cmp(generics, left_gives, gives)
            } else {
                false
            }
        }
        _ => *right == *left,
    }
}

// NOTE: Assumes they're both the same length
fn infer_all(params: &[MaybeType], known: &[Type]) {
    for (i, p) in params.iter().enumerate() {
        if let MaybeType::Infer(t) = p {
            if t.borrow().is_none() {
                *t.borrow_mut() = Some(known[i].clone());
            }
            continue;
        }
    }
}

fn swap_generic(t: &mut Type, generics: &HashMap<u8, Type>) {
    match t {
        Type::Generic(n) => *t = generics[n].clone(),
        Type::List(inner) => swap_generic(inner, generics),
        Type::Function(box (takes, gives)) => {
            swap_generic(gives, generics);
            for t in takes {
                swap_generic(t, generics);
            }
        }
        _ => {}
    }
}

fn find_generics<'a>(matching: &[Type], got: &[MaybeType]) -> Result<HashMap<u8, Type>, &'a str> {
    let mut generics = HashMap::new();

    for (i, m) in matching.iter().enumerate() {
        if let Some(gens) = find_generic(m, &got[i]) {
            for (gen, t) in gens {
                generics.insert(gen, t);
            }
        }
    }
    Ok(generics)
}

fn find_generic(t: &Type, got: &MaybeType) -> Option<Vec<(u8, Type)>> {
    match t {
        Type::Generic(n) => Some(vec![(*n, got.clone().unwrap())]),
        Type::List(inner) => {
            if let MaybeType::Known(Type::List(got_inner)) = &got {
                let got_inner: Type = (**got_inner).clone();
                find_generic(&inner, &MaybeType::Known(got_inner))
            } else {
                unreachable!();
            }
        }
        Type::Function(box (takes, gives)) => {
            if let MaybeType::Known(Type::Function(box (got_takes, got_gives))) = &got {
                let mut buf = Vec::new();
                for (i, gt) in got_takes.iter().enumerate() {
                    if let Some(mut generics) =
                        find_generic(&takes[i], &MaybeType::Known(gt.clone()))
                    {
                        buf.append(&mut generics);
                    }
                }
                let got_return: Type = got_gives.clone();
                if let Some(mut generics) = find_generic(gives, &MaybeType::Known(got_return)) {
                    buf.append(&mut generics);
                }
                Some(buf)
            } else {
                unreachable!();
            }
        }
        _ => None,
    }
}
