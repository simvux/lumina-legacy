use super::{ast, Anot, Identifier, MaybeType, ParseFault, Parser, Tracked, Type, PRELUDE_FID};
use std::cmp::Ordering;
use std::collections::HashMap;

impl Parser {
    pub fn find_func(
        &self,
        self_fid: usize,
        ident: &Anot<Identifier, Type>,
        params: &[MaybeType],
    ) -> Result<(usize, usize), ParseFault> {
        let variants = self.functions_named(self_fid, ident)?;
        let mut matches = variants.keeping(|_, got| {
            if got.len() == params.len() {
                self.are_compatible(params, got)
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
        ident: &Anot<Identifier, Type>,
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
            identifiers: ast::Meta::identifiers_from(fid, funcid, func, params),
        };
        Ok((&func.body, meta))
    }

    // This is when we only know the *amount* of parameters, and not the actual parameters
    // themselves. Useful for when we are more likely to need annotations.
    pub fn find_func_only_suitable(
        &self,
        self_fid: usize,
        ident: &Anot<Identifier, Type>,
        atleast_params: usize,
    ) -> Result<(usize, usize), ParseFault> {
        let variants = self.functions_named(self_fid, ident)?;

        // let mut matches = matches_of(variants, |got| got.len() >= atleast_params);
        let mut matches = variants.keeping(|_, got| got.len() >= atleast_params);
        assert!(!matches.is_empty());
        let use_match = matches.remove(0);

        if !matches.is_empty() {
            panic!("Need type annotation");
        }

        Ok(use_match)
    }

    pub fn functions_named<'a>(
        &'a self,
        self_fid: usize,
        ident: &Anot<Identifier, Type>,
    ) -> Result<Variants, ParseFault> {
        let mut all_variants = Variants::default();

        let fid = match ident.inner.path.len() {
            0 => {
                if self_fid != PRELUDE_FID {
                    if let Ok(prelude_variants) = self.functions_named(PRELUDE_FID, ident) {
                        all_variants = prelude_variants;
                    }
                }
                self_fid
            }
            1 => {
                match self.modules[self_fid]
                    .imports
                    .get(ident.inner.path.last().as_ref().unwrap().as_str())
                {
                    Some(fid) => *fid,
                    None => {
                        return Err(ParseFault::ModuleNotImported(
                            ident.inner.path.last().cloned().unwrap(),
                        ))
                    }
                }
            }
            _ => panic!("ET: Invalid path length"),
        };
        let module = &self.modules[fid];

        if let Some(variants) = module.function_ids.get(&ident.inner.name) {
            self.gather_to_and_deserialize(fid, &mut all_variants, variants);
        };
        if all_variants.matching.is_empty() {
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

    pub fn find_type(
        &self,
        self_fid: usize,
        ident: &Anot<Identifier, Type>,
    ) -> Result<(usize, usize), ParseFault> {
        let fid = self.fid_from_path(self_fid, &ident.inner.path);
        self.modules[fid]
            .type_ids
            .get(&ident.inner.name)
            .copied()
            .map(|tid| (fid, tid))
            .ok_or_else(|| ParseFault::TypeNotFound(self_fid, ident.clone()))
    }

    fn fid_from_path(&self, self_fid: usize, path: &[String]) -> usize {
        match path.len() {
            0 => self_fid,
            1 => self.modules[self_fid].imports[path.last().as_ref().unwrap().as_str()],
            _ => panic!("ET: Invalid path length"),
        }
    }

    fn are_compatible(&self, want: &[MaybeType], got: &[Type]) -> bool {
        if want.len() != got.len() {
            return false;
        }
        let mut generics: HashMap<u8, Type> = HashMap::new();
        want.iter().zip(got.iter()).all(|(want, got)| match want {
            MaybeType::Infer(t) => {
                if let Some(existing) = t.borrow().as_ref() {
                    return self.generic_cmp(&mut generics, existing, got);
                }
                let decoded = got.clone().decoded(&generics);
                if let Type::Generic(_) = &decoded {
                    // We can't infer here.
                    return false;
                }
                true
            }
            MaybeType::Known(existing) => self.generic_cmp(&mut generics, existing, got),
        })
    }

    fn generic_cmp(&self, generics: &mut HashMap<u8, Type>, left: &Type, right: &Type) -> bool {
        match &right {
            Type::Custom(ident) => {
                panic!("Un-Deserialized custom type in comparison: {}", ident);
            }
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
                    let params_ok = takes.iter().enumerate().all(|(i, inner_right)| {
                        self.generic_cmp(generics, &left_takes[i], inner_right)
                    });
                    if !params_ok {
                        return false;
                    }
                    self.generic_cmp(generics, left_gives, gives)
                } else {
                    false
                }
            }
            _ => *right == *left,
        }
    }

    // Take a Type collection, deserialize `Custom` types, add them to self.
    fn gather_to_and_deserialize(
        &self,
        fid: usize,
        variants: &mut Variants,
        from: &HashMap<Vec<Type>, usize>,
    ) {
        from.iter().for_each(|(params, funcid)| {
            let params = params
                .iter()
                .map(|t| self.destruct_custom_type(fid, t.clone()))
                .collect();
            match variants.matching.iter_mut().find(|(id, _)| fid == *id) {
                Some((_, variants_this_module)) => {
                    // Modify existing module
                    variants_this_module.push((params, *funcid));
                }
                None => {
                    // Add new module variants container
                    variants.matching.push((fid, vec![(params, *funcid)]));
                }
            };
        });
    }
}

#[derive(Default, Debug)]
pub struct Variants {
    pub matching: Vec<(usize, Vec<(Vec<Type>, usize)>)>,
}
impl Variants {
    fn for_each(&self, mut f: impl FnMut(usize, usize, &[Type])) {
        self.matching.iter().for_each(|(fid, variants)| {
            variants
                .iter()
                .for_each(|(takes, funcid)| f(*fid, *funcid, takes))
        });
    }

    fn keeping(&self, mut predicate: impl FnMut(usize, &[Type]) -> bool) -> Vec<(usize, usize)> {
        let mut matches = Vec::new();
        self.for_each(|fid, funcid, types| {
            if predicate(fid, types) {
                matches.push((fid, funcid))
            }
        });
        matches
    }

    fn join(&mut self, other: Self) {
        for m in other.matching {
            self.matching.push(m);
        }
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
