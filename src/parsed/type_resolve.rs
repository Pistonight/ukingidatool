use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::sync::{mpsc, Arc, RwLock};

use itertools::Itertools;
use threadpool::ThreadPool;

use super::{TypeInfo, TypeName, TypeComp};

use crate::util::ProgressPrinter;

/// A resolver expression
pub enum Expr {
    /// 2 types are equivalent
    Equiv(usize, usize),
    /// If 2 is equiv to 3 then 0 is equiv to 1
    IfEquivThenEquiv(usize, usize, usize, usize),
    /// If all (a, b) pairs are equivalent, then a and b are equivalent
    IfAllEquivThenEquiv(usize, usize, Vec<(usize, usize)>),
}
struct PrintType<'a>(usize, &'a TypeResolver);
impl<'a> std::fmt::Display for PrintType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bucket = self.1.offset_to_bucket.get(&self.0).unwrap();
        let ty = self.1.offset_to_type.get(&bucket).unwrap();
        write!(f, "{}", ty)
    }
}


pub struct TypeResolver {
    pub offset_to_type: BTreeMap<usize, TypeInfo>,
    pub offset_to_bucket: BTreeMap<usize, usize>,
    pub bucket_to_offsets: BTreeMap<usize, Vec<usize>>,
}

impl TypeResolver {
    pub fn new(offset_to_type: BTreeMap<usize, TypeInfo>) -> Self {
        let offset_to_bucket = offset_to_type.keys().map(|k| (*k, *k)).collect();
        let bucket_to_offsets = offset_to_type.keys().map(|k| (*k, vec![*k])).collect();
        // let bucket_to_name = offset_to_type
        //     .iter()
        //     .map(|(k, v)| {
        //         let v = match v {
        //             TypeInfo::Prim(t) => TypeName::Prim(*t),
        //             TypeInfo::Typedef(name, _) => TypeName::Name(name.clone()),
        //             TypeInfo::Struct(x) => match &x.name {
        //                 Some(name) => TypeName::Name(name.clone()),
        //                 None => TypeName::Anon(*k),
        //             },
        //             TypeInfo::Enum(x) => match &x.name {
        //                 Some(name) => TypeName::Name(name.clone()),
        //                 None => TypeName::Anon(*k),
        //             },
        //             TypeInfo::Union(x) => match &x.name {
        //                 Some(name) => TypeName::Name(name.clone()),
        //                 None => TypeName::Anon(*k),
        //             },
        //             TypeInfo::Comp(x) => TypeName::Compound(x.clone()),
        //         };
        //         (*k, v)
        //     })
        //     .collect::<HashMap<_,_>>();
        Self {
            offset_to_bucket,
            offset_to_type,
            bucket_to_offsets,
        }
    }

    pub fn get_names(&self) -> BTreeMap<usize, TypeName> {
        let mut names = BTreeMap::new();
        for (offset, ty) in &self.offset_to_type {
            let name = match ty {
                TypeInfo::Prim(t) => TypeName::Prim(*t),
                TypeInfo::Typedef(name, _) => TypeName::Name(name.clone()),
                TypeInfo::Struct(x) => match &x.name {
                    Some(name) => TypeName::Name(name.clone()),
                    None => TypeName::Anon(*offset),
                },
                TypeInfo::Enum(x) => match &x.name {
                    Some(name) => TypeName::Name(name.clone()),
                    None => TypeName::Anon(*offset),
                },
                TypeInfo::Union(x) => match &x.name {
                    Some(name) => TypeName::Name(name.clone()),
                    None => TypeName::Anon(*offset),
                },
                TypeInfo::Comp(TypeComp::Ptr(x)) => {
                    TypeName::Comp(TypeComp::Ptr(*self.offset_to_bucket.get(x).unwrap()))
                }
                TypeInfo::Comp(TypeComp::Array(x, y)) => {
                    TypeName::Comp(TypeComp::Array(*self.offset_to_bucket.get(x).unwrap(), *y))
                }
                TypeInfo::Comp(TypeComp::Subroutine(x, y)) => {
                    let x = *self.offset_to_bucket.get(x).unwrap();
                    let y = y.iter().map(|x| *self.offset_to_bucket.get(x).unwrap()).collect();
                    TypeName::Comp(TypeComp::Subroutine(x, y))
                }
                TypeInfo::Comp(TypeComp::Ptmf(x, y, z)) => {
                    let x = *self.offset_to_bucket.get(x).unwrap();
                    let y = *self.offset_to_bucket.get(y).unwrap();
                    let z = z.iter().map(|x| *self.offset_to_bucket.get(x).unwrap()).collect();
                    TypeName::Comp(TypeComp::Ptmf(x, y, z))
                }
            };
            names.insert(*offset, name);
        }
        names
    }

    // pub fn get_bucket(&self, offset: usize) -> usize {
    //     let bucket = self.offset_to_bucket.get(&offset).unwrap();
    //     match self.merging_old_to_new_buckets.get(bucket) {
    //         Some(new_bucket) => *new_bucket,
    //         None => *bucket,
    //     }
    // }
    //
    pub fn find_mergeable(&self, inputs: &Vec<usize>) -> Vec<Vec<usize>> {
        // let mut added = HashSet::new();
        let mut merge_sets = Vec::new();
        for (i, a) in inputs.iter().enumerate() {
            let a = *a;
            // if !added.insert(a) {
            //     continue;
            // }
            let mut s = vec![a];
            for b in &inputs[i + 1..] {
                // if added.contains(b) {
                //     continue;
                // }
                let b = *b;
                if a == b {
                    continue;
                }
                if self.needs_merging(a, b) {
                    s.push(b);
                    // added.insert(b);
                }
            }
            if s.len() > 1 {
                merge_sets.push(s);
            }
        }
        merge_sets
    }

    pub fn find_mergeable_progress(&self, inputs: &Vec<usize>, msg: &str) -> Vec<Vec<usize>> {
        // let mut added = HashSet::new();
        let mut merge_sets = Vec::new();
        let progress = ProgressPrinter::new(inputs.len(), msg);
        for (i, a) in inputs.iter().enumerate() {
            progress.print(i, "");
            let a = *a;
            // if !added.insert(a) {
            //     continue;
            // }
            let mut s = vec![a];
            for b in &inputs[i + 1..] {
                // if added.contains(b) {
                //     continue;
                // }
                let b = *b;
                if a == b {
                    continue;
                }
                if self.needs_merging(a, b) {
                    s.push(b);
                    // added.insert(b);
                }
            }
            if s.len() > 1 {
                merge_sets.push(s);
            }
        }
        progress.done();
        merge_sets
    }

    pub fn eval_all_exprs(s: &Arc<RwLock<Self>>, pool: &ThreadPool, exprs: &mut Vec<Expr>) {
        let mut progress = ProgressPrinter::new(0, "Deduplicating types");
        loop {
            let results = TypeResolver::eval_exprs(s, &pool, &mut progress, exprs);
            if results.is_empty() {
                break s;
            }
            {
                s.write().unwrap().merge_all(&results, &mut progress);
            }
        };
        progress.done();
    }

    pub fn eval_exprs(s: &Arc<RwLock<Self>>, pool: &ThreadPool, progress: &mut ProgressPrinter, exprs: &mut Vec<Expr>) -> Vec<(usize, usize)>{
        let s = s.read().unwrap();
        progress.set_total(exprs.len());
        progress.print(0, "");
        let mut results = Vec::new();
        let old_expr = std::mem::take(exprs);
        for (i, expr) in old_expr.into_iter().enumerate() {
            match s.eval_expr(expr) {
                Ok((a,b)) => {
                    progress.print(i, format!("{} == {}", PrintType(a, &s), PrintType(b, &s)));
                    results.push((a,b));
                }
                Err(expr) => {
                    exprs.push(expr);
                }
            }
        }
        // let (send, recv) = mpsc::channel();
        // for expr in &old_expr.into_iter().chunks(81920) {
        //     let exprs = expr.collect::<Vec<_>>();
        //     let send = send.clone();
        //     let s= Arc::clone(s);
        //     pool.execute(move || {
        //         let s = s.read().unwrap();
        //         for expr in exprs {
        //             send.send(s.eval_expr(expr)).unwrap();
        //         }
        //     });
        // }
        // drop(send);
        // {
        //     let s = s.read().unwrap();
        //     for (i, result) in recv.iter().enumerate() {
        //         match result {
        //             Ok((a,b)) => {
        //                 progress.print(i, format!("{} == {}", PrintType(a, &s), PrintType(b, &s)));
        //                 results.push((a,b));
        //             }
        //             Err(expr) => {
        //                 exprs.push(expr);
        //             }
        //         }
        //     }
        // }
        // pool.join();
        results
    }

    /// Evalulate expression, return a pair of equivalent types if possible. Otherwise return the expression
    /// (which might be partially evaluated)
    fn eval_expr(&self, expr: Expr) -> Result<(usize, usize), Expr> {
        match expr {
            Expr::Equiv(a, b) => {
                Ok((a, b))
            }
            Expr::IfEquivThenEquiv(a, b, c, d) => {
                // is c == d?
                let c_bucket = self.offset_to_bucket.get(&c).unwrap();
                let d_bucket = self.offset_to_bucket.get(&d).unwrap();
                if c_bucket == d_bucket {
                    Ok((a, b))
                } else {
                    Err(Expr::IfEquivThenEquiv(a, b, c, d))
                }
            }
            Expr::IfAllEquivThenEquiv(a, b, mut pairs) => {
                let mut i = 0;
                while i < pairs.len() {
                    let (c, d) = pairs[i];
                    let c_bucket = self.offset_to_bucket.get(&c).unwrap();
                    let d_bucket = self.offset_to_bucket.get(&d).unwrap();
                    if c_bucket == d_bucket {
                        pairs.swap_remove(i);
                    } else {
                        i += 1;
                    }
                }
                match pairs.len() {
                    0 => Ok((a, b)),
                    1 => Err(Expr::IfEquivThenEquiv(a, b, pairs[0].0, pairs[0].1)),
                    _ => Err(Expr::IfAllEquivThenEquiv(a, b, pairs)),
                }
            }
        }
    }

    pub fn merge_typedefs(&mut self) {
        let mut typedefs = Vec::new();
        for offset in self.offset_to_type.keys() {
            self.make_typedef(*offset, &mut typedefs);
        }
        let progress = ProgressPrinter::new(typedefs.len(), "Merging typedefs");
        for (i, (a,b)) in typedefs.into_iter().enumerate() {
            progress.print(i, format!("{} == {}", PrintType(a, self), PrintType(b, self)));
            self.merge(a, b);
        }
        progress.done();
    }

    pub fn merge_simple_types(s: &Arc<RwLock<Self>>, pool: &ThreadPool, all: bool) {
        let mut exprs = Vec::new();
        let prim_keys = {
            let s2 = s.read().unwrap();
            s2.offset_to_type.iter().filter_map(|(offset, ty)| {
                if let TypeInfo::Prim(_) = ty {
                    Some(*s2.offset_to_bucket.get(offset).unwrap())
                } else {
                    None
                }
            }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
        };
        exprs.extend(Self::make_exprs_by_keys(s, pool, prim_keys, "Analyzing primitives"));
        let struct_keys = {
            let s2 = s.read().unwrap();
            s2.offset_to_type.iter().filter_map(|(offset, ty)| {
                if let TypeInfo::Struct(_) = ty {
                    Some(*s2.offset_to_bucket.get(offset).unwrap())
                } else {
                    None
                }
            }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
        };
        exprs.extend(Self::make_exprs_by_keys(s, pool, struct_keys, "Analyzing structs"));
        let union_keys = {
            let s2 = s.read().unwrap();
            s2.offset_to_type.iter().filter_map(|(offset, ty)| {
                if let TypeInfo::Union(_) = ty {
                    Some(*s2.offset_to_bucket.get(offset).unwrap())
                } else {
                    None
                }
            }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
        };
        exprs.extend(Self::make_exprs_by_keys(s, pool, union_keys, "Analyzing unions"));
        let enum_keys = {
            let s2 = s.read().unwrap();
            s2.offset_to_type.iter().filter_map(|(offset, ty)| {
                if let TypeInfo::Enum(_) = ty {
                    Some(*s2.offset_to_bucket.get(offset).unwrap())
                } else {
                    None
                }
            }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
        };
        exprs.extend(Self::make_exprs_by_keys(s, pool, enum_keys, "Analyzing enums"));
        if all {
        // let ptr_keys = {
        //     let s2 = s.read().unwrap();
        //     s2.offset_to_type.iter().filter_map(|(offset, ty)| {
        //         if let TypeInfo::Comp(TypeComp::Ptr(_)) = ty {
        //             Some(*s2.offset_to_bucket.get(offset).unwrap())
        //         } else {
        //             None
        //         }
        //     }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
        // };
        // exprs.extend(Self::make_exprs_by_keys(s, pool, ptr_keys, "Analyzing pointer types"));
        let array_keys = {
            let s2 = s.read().unwrap();
            s2.offset_to_type.iter().filter_map(|(offset, ty)| {
                if let TypeInfo::Comp(TypeComp::Array(_, _)) = ty {
                    Some(*s2.offset_to_bucket.get(offset).unwrap())
                } else {
                    None
                }
            }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
        };
        exprs.extend(Self::make_exprs_by_keys(s, pool, array_keys, "Analyzing array types"));
        let subroutine_keys = {
            let s2 = s.read().unwrap();
            s2.offset_to_type.iter().filter_map(|(offset, ty)| {
                if let TypeInfo::Comp(TypeComp::Subroutine(_, _)) = ty {
                    Some(*s2.offset_to_bucket.get(offset).unwrap())
                } else {
                    None
                }
            }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
        };
        exprs.extend(Self::make_exprs_by_keys(s, pool, subroutine_keys, "Analyzing subroutine types"));
        let ptmf_keys = {
            let s2 = s.read().unwrap();
            s2.offset_to_type.iter().filter_map(|(offset, ty)| {
                if let TypeInfo::Comp(TypeComp::Ptmf(_, _, _)) = ty {
                    Some(*s2.offset_to_bucket.get(offset).unwrap())
                } else {
                    None
                }
            }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
        };
        exprs.extend(Self::make_exprs_by_keys(s, pool, ptmf_keys, "Analyzing ptr-to-member-function types"));
        }
        Self::eval_all_exprs(s, pool, &mut exprs);
    }

    pub fn merge_pointers(s: &Arc<RwLock<Self>>, pool: &ThreadPool) {
        let ptr_keys = {
            let s2 = s.read().unwrap();
            s2.offset_to_type.iter().filter_map(|(offset, ty)| {
                if let TypeInfo::Comp(TypeComp::Ptr(_)) = ty {
                    Some(*s2.offset_to_bucket.get(offset).unwrap())
                } else {
                    None
                }
            }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
        };
        let merge_sets = s.read().unwrap().find_mergeable_progress(&ptr_keys, "Deduplicating pointers");
        for merge_set in merge_sets {
            s.write().unwrap().merge_set(&merge_set);
        }
    }
    pub fn merge_all_types(s: &Arc<RwLock<Self>>, pool: &ThreadPool) {
        let keys = {
            let s2 = s.read().unwrap();
            s2.bucket_to_offsets.keys().copied().collect::<Vec<_>>()
        };
        let merge_sets = s.read().unwrap().find_mergeable_progress(&keys, "Deduplicating all types");
        for merge_set in merge_sets {
            s.write().unwrap().merge_set(&merge_set);
        }
    }

    pub fn make_exprs(s: &Arc<RwLock<Self>>, pool: &ThreadPool) -> Vec<Expr> {
        let mut exprs = Vec::new();
        let ptr_keys = {
            let s2 = s.read().unwrap();
            s2.offset_to_type.iter().filter_map(|(offset, ty)| {
                if let TypeInfo::Comp(TypeComp::Ptr(_)) = ty {
                    Some(*s2.offset_to_bucket.get(offset).unwrap())
                } else {
                    None
                }
            }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
        };
        exprs.extend(Self::make_exprs_by_keys(s, pool, ptr_keys, "Analyzing pointer types"));
        let array_keys = {
            let s2 = s.read().unwrap();
            s2.offset_to_type.iter().filter_map(|(offset, ty)| {
                if let TypeInfo::Comp(TypeComp::Array(_, _)) = ty {
                    Some(*s2.offset_to_bucket.get(offset).unwrap())
                } else {
                    None
                }
            }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
        };
        exprs.extend(Self::make_exprs_by_keys(s, pool, array_keys, "Analyzing array types"));
        let subroutine_keys = {
            let s2 = s.read().unwrap();
            s2.offset_to_type.iter().filter_map(|(offset, ty)| {
                if let TypeInfo::Comp(TypeComp::Subroutine(_, _)) = ty {
                    Some(*s2.offset_to_bucket.get(offset).unwrap())
                } else {
                    None
                }
            }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
        };
        exprs.extend(Self::make_exprs_by_keys(s, pool, subroutine_keys, "Analyzing subroutine types"));
        let ptmf_keys = {
            let s2 = s.read().unwrap();
            s2.offset_to_type.iter().filter_map(|(offset, ty)| {
                if let TypeInfo::Comp(TypeComp::Ptmf(_, _, _)) = ty {
                    Some(*s2.offset_to_bucket.get(offset).unwrap())
                } else {
                    None
                }
            }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
        };
        exprs.extend(Self::make_exprs_by_keys(s, pool, ptmf_keys, "Analyzing ptr-to-member-function types"));

        exprs
    }
    fn make_exprs_by_keys(s: &Arc<RwLock<Self>>, pool: &ThreadPool, keys: Vec<usize>, msg: &str) -> Vec<Expr> {
        let len = keys.len();
        let total = len * (len + 1) / 2;
        let progress = ProgressPrinter::new(total, msg);
        let (send, recv) = mpsc::sync_channel(pool.max_count() * 2);
        struct Iter {
            a_offset: usize,
            b_offset: usize,
            keys: Vec<usize>,
        }
        impl Iter {
            fn new(keys: Vec<usize>) -> Self {
                Self {
                    a_offset: 0,
                    b_offset: 1,
                    keys,
                }
            }
        }
        impl Iterator for Iter {
            type Item = (usize, usize);
            fn next(&mut self) -> Option<Self::Item> {
                self.b_offset += 1;
                if self.b_offset >= self.keys.len() {
                    self.a_offset += 1;
                    self.b_offset = self.a_offset + 1;
                }
                if self.a_offset >= self.keys.len() || self.b_offset >= self.keys.len(){
                    return None;
                }
                Some((self.keys[self.a_offset], self.keys[self.b_offset]))
            }
        }
        let mut i = 0;
        let recv_thread = std::thread::spawn(move || {
            let mut exprs = Vec::new();
            for (recv_exprs, l) in recv {
                exprs.extend(recv_exprs);
                i += l;
                progress.print(i, "");
            }
        progress.done();
            exprs
        });
        for jobs in &Iter::new(keys).chunks(81920) {
            let jobs = jobs.collect::<Vec<_>>();
            let s2 = Arc::clone(s);
            let send = send.clone();
            pool.execute(move || {
                let mut exprs = Vec::new();
                let len = jobs.len();
                let s2 = s2.read().unwrap();
                for (a_offset, b_offset) in jobs {
                    s2.make_expr(a_offset, b_offset, &mut exprs, true);
                }
                // if exprs.is_empty() {
                //     for (a_offset, b_offset) in jobs {
                //         s2.make_expr_recursive(a_offset, b_offset, &mut exprs, false);
                //     }
                // }
                send.send((exprs, len)).unwrap();
            });

        }
        drop(send);
        
        let exprs = recv_thread.join().unwrap();
        pool.join();
        exprs
    }

    fn make_typedef(&self, offset: usize, exprs: &mut Vec<(usize, usize)>) {
        let ty = self.offset_to_type.get(&offset).unwrap();
        if let TypeInfo::Typedef(_, ty_offset) = ty {
            exprs.push((*ty_offset, offset));
        }
    }

    /// Make the resolver expression from a_offset and b_offset
    fn make_expr(&self, a_offset: usize, b_offset: usize, exprs: &mut Vec<Expr>, no_member: bool) {
        if a_offset > b_offset {
            panic!("a_offset <= b_offset");
        }
        if a_offset == b_offset {
            return;
        }
        let a_ty = self.offset_to_type.get(&a_offset).unwrap();
        let b_ty = self.offset_to_type.get(&b_offset).unwrap();
        match (a_ty, b_ty) {
            // note: typedefs are added in make_expr_typedef
            (TypeInfo::Prim(a_prim), TypeInfo::Prim(b_prim)) => {
                // two primitive types are only equivalent if they are the same
                if a_prim == b_prim {
                    exprs.push(Expr::Equiv(a_offset, b_offset));
                }
            }
            (TypeInfo::Struct(a_struct), TypeInfo::Struct(b_struct)) => {
                if a_struct.size != b_struct.size {
                    return;
                }
                if a_struct.size == 0 {
                    exprs.push(Expr::Equiv(a_offset, b_offset));
                    return; // all ZSTs are equivalent
                }
                if a_struct.members.len() != b_struct.members.len() {
                    return;
                }
                let check_member_names = match (&a_struct.name, &b_struct.name) {
                    (Some(a_name), Some(b_name)) => {
                        // if both A and B are named, they must be the same name to be considered
                        // equivalent, otherwise non-related types with the same layout will be
                        // merged
                        if a_name != b_name {
                            return;
                        }
                        // when they do have the same name, member names matter less, so we don't check
                        false
                    },
                    // if one of A and B is unnamed, they can be considered equivalent if their
                    // layouts are the same
                    _ => true,
                };
                if a_struct.vtable != b_struct.vtable {
                    return;
                }
                if no_member {
                    return;
                }
                // let mut member_exprs = Vec::with_capacity(a_struct.members.len());
                for (a_member, b_member) in a_struct.members.iter().zip(b_struct.members.iter()) {
                    if check_member_names {
                        match (&a_member.name, &b_member.name) {
                            (Some(a_name), Some(b_name)) => {
                                if a_name != b_name {
                                    return;
                                }
                            }
                            _ => (),
                        }
                    }
                    if a_member.is_base != b_member.is_base {
                        return;
                    }
                    if a_member.offset != b_member.offset {
                        return;
                    }
                    // member_exprs.push((a_member.ty_offset, b_member.ty_offset));
                }
                // if all previous check pass, then the two structs are equivalent if their member
                // types are equivalent
                // recursively add the expressions we depend on
                // for (a, b) in &member_exprs {
                //     self.make_expr_recursive(*a, *b, exprs, seen);
                // }
                // exprs.push(Expr::IfAllEquivThenEquiv(a_offset, b_offset, member_exprs));
                    exprs.push(Expr::Equiv(a_offset, b_offset));
            }
            (TypeInfo::Union(a_union), TypeInfo::Union(b_union)) => {
                // both union name and union member names don't matter
                if a_union.members.len() != b_union.members.len() {
                    return;
                }
                if no_member {
                    return;
                }
                // only consider the members in order for now
                let mut member_exprs = Vec::with_capacity(a_union.members.len());
                for (a_member, b_member) in a_union.members.iter().zip(b_union.members.iter()) {
                    member_exprs.push((a_member.1, b_member.1));
                }
                // if all previous check pass, then the two unions are equivalent if their member
                // types are equivalent
                // recursively add the expressions we depend on
                // for (a, b) in &member_exprs {
                //     self.make_expr_recursive(*a, *b, exprs, seen);
                // }
                exprs.push(Expr::IfAllEquivThenEquiv(a_offset, b_offset, member_exprs));
            }
            (TypeInfo::Enum(a_enum), TypeInfo::Enum(b_enum)) => {
                // names don't really matter for enum, so we don't check

                // size is the byte size, so they must match
                if a_enum.size != b_enum.size {
                    return;
                }
                // number of enumerators must match
                if a_enum.enumerators.len() != b_enum.enumerators.len() {
                    return;
                }
                for (a_enumerator, b_enumerator) in a_enum.enumerators.iter().zip(b_enum.enumerators.iter())
                {
                    // names matter for the enumerators, otherwise unrelated enums with the
                    // same values will be merged
                    if a_enumerator.0 != b_enumerator.0 || a_enumerator.1 != b_enumerator.1 {
                        return;
                    }
                }
                // if all previous checks pass, then the two enums are equivalent
                exprs.push(Expr::Equiv(a_offset, b_offset));
            }
            (TypeInfo::Comp(TypeComp::Ptr(a_ptr)), TypeInfo::Comp(TypeComp::Ptr(b_ptr))) => {
                // 2 ptrs are equivalent if their target types are equivalent
                // self.make_expr_recursive(*a_ptr, *b_ptr, exprs, seen);
                exprs.push(Expr::IfEquivThenEquiv(a_offset, b_offset, *a_ptr, *b_ptr));
            }
            (TypeInfo::Comp(TypeComp::Array(a_t, a_count)), TypeInfo::Comp(TypeComp::Array(b_t, b_count))) => {
                // 2 arrays are equivalent if their element types are equivalent and their counts are
                // the same
                if a_count != b_count {
                    return;
                }
                // self.make_expr_recursive(*a_t, *b_t, exprs, seen);
                exprs.push(Expr::IfEquivThenEquiv(a_offset, b_offset, *a_t, *b_t));
            }
            (TypeInfo::Comp(TypeComp::Subroutine(a_ret, a_param)), TypeInfo::Comp(TypeComp::Subroutine(b_ret, b_param))) => {
                // 2 subroutines are equivalent if their return types and parameter types are equivalent
                if a_param.len() != b_param.len() {
                    return;
                }
                if no_member {
                    return;
                }
                let mut member_exprs = Vec::with_capacity(a_param.len() + 1);
                member_exprs.push((*a_ret, *b_ret));
                // self.make_expr_recursive(*a_ret, *b_ret, exprs, seen);
                for (a_p, b_p) in a_param.iter().zip(b_param.iter()) {
                    // self.make_expr_recursive(*a_p, *b_p, exprs, seen);
                    member_exprs.push((*a_p, *b_p));
                }
                exprs.push(Expr::IfAllEquivThenEquiv(a_offset, b_offset, member_exprs));
            }
            (TypeInfo::Comp(TypeComp::Ptmf(a_this, a_ret, a_param)), TypeInfo::Comp(TypeComp::Ptmf(b_this, b_ret, b_param))) => {
                // ptmf are similar to subroutine, with an additional this type
                if a_param.len() != b_param.len() {
                    return;
                }
                if no_member {
                    return;
                }
                let mut member_exprs = Vec::with_capacity(a_param.len() + 2);
                member_exprs.push((*a_this, *b_this));
                // self.make_expr_recursive(*a_this, *b_this, exprs, seen);
                member_exprs.push((*a_ret, *b_ret));
                // self.make_expr_recursive(*a_ret, *b_ret, exprs, seen);
                for (a_p, b_p) in a_param.iter().zip(b_param.iter()) {
                    // self.make_expr_recursive(*a_p, *b_p, exprs, seen);
                    member_exprs.push((*a_p, *b_p));
                }
                exprs.push(Expr::IfAllEquivThenEquiv(a_offset, b_offset, member_exprs));
            }
            _ => {
                // if the two types are not the same type, they are not equivalent
            }
        }
    }


    fn needs_merging(&self, a_offset: usize, b_offset: usize) -> bool {
        let a_bucket = self.offset_to_bucket.get(&a_offset).unwrap();
        let b_bucket = self.offset_to_bucket.get(&b_offset).unwrap();
        if a_bucket == b_bucket {
            return false;
        }
        let mut seen = HashSet::new();
        return self.are_equiv(a_offset, b_offset, &mut seen);
    }

    /// Perform high-level merge checks
    ///
    /// Struct and union member types are not checked
    fn are_equiv(&self, a_offset: usize, b_offset: usize, memo: &mut HashSet<(usize, usize)>) -> bool {
        if a_offset > b_offset {
            return self.are_equiv(b_offset, a_offset, memo);
        }
        if a_offset == b_offset {
            return false;
        }
        if !memo.insert((a_offset, b_offset)) {
            return false;
        }
        // if let Some(result) = memo.get(&(a_offset, b_offset)) {
        //     return *result;
        // }
        // memo.insert((a_offset, b_offset), false);
        let a_ty = self.offset_to_type.get(&a_offset).unwrap();
        let b_ty = self.offset_to_type.get(&b_offset).unwrap();
        let result = match (a_ty, b_ty) {
            // note: typedefs are added in make_expr_typedef
            (TypeInfo::Prim(a_prim), TypeInfo::Prim(b_prim)) => {
                a_prim == b_prim
            }
            (TypeInfo::Struct(a_struct), TypeInfo::Struct(b_struct)) => {
                if a_struct.size != b_struct.size {
                    false
                } else if a_struct.size == 0 {
                    true // all ZSTs are equivalent
                } else if a_struct.members.len() != b_struct.members.len() {
                    false
                } else if a_struct.vtable != b_struct.vtable {
                    return false;
                } else {
                    match (&a_struct.name, &b_struct.name) {
                        (Some(a_name), Some(b_name))if a_name != b_name => {
                            // if both A and B are named, they must be the same name to be considered
                            // equivalent, otherwise non-related types with the same layout will be
                            // merged
                            false
                        },
                        _ => {
                            let mut r = true;
                            for (a_member, b_member) in a_struct.members.iter().zip(b_struct.members.iter()) {
                                match (&a_member.name, &b_member.name) {
                                    (Some(a_name), Some(b_name)) => {
                                        if a_name != b_name {
                                            r = false;
                                            break;
                                        }
                                    }
                                    _ => (),
                                }
                                if a_member.is_base != b_member.is_base {
                                    r = false;
                                    break;
                                }
                                if a_member.offset != b_member.offset {
                                    r = false;
                                    break;
                                }
                            }
                            r
                        }
                    }
                }
            }
            (TypeInfo::Union(a_union), TypeInfo::Union(b_union)) => {
                // both union name and union member names don't matter
                if a_union.members.len() != b_union.members.len() {
                    false
                } else {
                    let mut r = true;
                    // only consider the members in order for now
                    for (a_member, b_member) in a_union.members.iter().zip(b_union.members.iter()) {
                        if a_member.0 != b_member.0 {
                            r = false;
                            break;
                        }
                        if !self.are_equiv(a_member.1, b_member.1, memo) {
                            r = false;
                            break;
                        }
                    }
                    r
                }
            }
            (TypeInfo::Enum(a_enum), TypeInfo::Enum(b_enum)) => {
                // names don't really matter for enum, so we don't check

                // size is the byte size, so they must match
                if a_enum.size != b_enum.size {
                    false
                } else if a_enum.enumerators.len() != b_enum.enumerators.len() {
                    // number of enumerators must match
                     false
                } else {
                    let mut r = true;
                    for (a_enumerator, b_enumerator) in a_enum.enumerators.iter().zip(b_enum.enumerators.iter())
                    {
                        // names matter for the enumerators, otherwise unrelated enums with the
                        // same values will be merged
                        if a_enumerator.0 != b_enumerator.0 || a_enumerator.1 != b_enumerator.1 {
                            r = false;
                            break;
                        }
                    }
                    r
                }
            }
            (TypeInfo::Comp(TypeComp::Ptr(a_ptr)), TypeInfo::Comp(TypeComp::Ptr(b_ptr))) => {
                let a_bucket = self.offset_to_bucket.get(a_ptr).unwrap();
                let b_bucket = self.offset_to_bucket.get(b_ptr).unwrap();
                self.are_equiv(*a_bucket, *b_bucket, memo)
            }
            (TypeInfo::Comp(TypeComp::Array(a_t, a_count)), TypeInfo::Comp(TypeComp::Array(b_t, b_count))) => {
                if a_count != b_count {
                    false
                } else {
                    let a_bucket = self.offset_to_bucket.get(a_t).unwrap();
                    let b_bucket = self.offset_to_bucket.get(b_t).unwrap();
                    self.are_equiv(*a_bucket, *b_bucket, memo)
                }
            }
            (TypeInfo::Comp(TypeComp::Subroutine(a_ret, a_param)), TypeInfo::Comp(TypeComp::Subroutine(b_ret, b_param))) => {
                let a_bucket = self.offset_to_bucket.get(a_ret).unwrap();
                let b_bucket = self.offset_to_bucket.get(b_ret).unwrap();
                if a_param.len() != b_param.len() {
                    false
                } else if !self.are_equiv(*a_bucket, *b_bucket, memo) {
                    false
                } else {
                    let mut r = true;
                    for (a_p, b_p) in a_param.iter().zip(b_param.iter()) {
                        let a_bucket = self.offset_to_bucket.get(a_p).unwrap();
                        let b_bucket = self.offset_to_bucket.get(b_p).unwrap();
                        if !self.are_equiv(*a_bucket, *b_bucket, memo) {
                            r = false;
                            break;
                        }
                    }
                    r
                }
            }
            (TypeInfo::Comp(TypeComp::Ptmf(a_this, a_ret, a_param)), TypeInfo::Comp(TypeComp::Ptmf(b_this, b_ret, b_param))) => {
                let a_bucket_this = self.offset_to_bucket.get(a_this).unwrap();
                let b_bucket_this = self.offset_to_bucket.get(b_this).unwrap();
                let a_bucket= self.offset_to_bucket.get(a_ret).unwrap();
                let b_bucket= self.offset_to_bucket.get(b_ret).unwrap();
                // ptmf are similar to subroutine, with an additional this type
                if a_param.len() != b_param.len() {
                    false
                } else if !self.are_equiv(*a_bucket_this, *b_bucket_this, memo) {
                    false
                } else if !self.are_equiv(*a_bucket, *b_bucket, memo) {
                    false
                } else {
                    let mut r = true;
                    for (a_p, b_p) in a_param.iter().zip(b_param.iter()) {
                        let a_bucket = self.offset_to_bucket.get(a_p).unwrap();
                        let b_bucket = self.offset_to_bucket.get(b_p).unwrap();
                        if !self.are_equiv(*a_bucket, *b_bucket, memo) {
                            r = false;
                            break;
                        }
                    }
                    r
                }
            }
            _ => {
                false
            }
        };

        // memo.insert((a_offset, b_offset), result);
        result
    }
    pub fn merge_all(&mut self, offsets: &[(usize, usize)], progress: &mut ProgressPrinter) {
        for (a, b) in offsets {
            self.merge(*a, *b);
        }
    }
    pub fn merge_set(&mut self, offsets: &[usize]) {
        if offsets.len() < 2 {
            return;
        }
        let a = offsets[0];
        let a_bucket = *self.offset_to_bucket.get(&a).unwrap();

        let mut to_change = Vec::new();
        for b in &offsets[1..] {
            let b_bucket = *self.offset_to_bucket.get(b).unwrap();
            if a_bucket != b_bucket {
                // it's possible it's already merged
                if let Some(b_offsets) = self.bucket_to_offsets.remove(&b_bucket) {
                    to_change.push(b_offsets);
                }
            }
        }
        for b in to_change.iter().flatten() {
            self.offset_to_bucket.insert(*b, a_bucket);
        }
        let a_offsets = self.bucket_to_offsets.get_mut(&a_bucket).unwrap();
        a_offsets.reserve(to_change.iter().map(|x| x.len()).sum());
        for b_offsets in to_change {
            a_offsets.extend(b_offsets);
        }
    }

    /// Merge the bucket containing b into the bucket containing a
    pub fn merge(&mut self, a_offset: usize, b_offset: usize) {
        let a_bucket = *self.offset_to_bucket.get(&a_offset).unwrap();
        let b_bucket = *self.offset_to_bucket.get(&b_offset).unwrap();
        if a_bucket == b_bucket {
            return;
        }
        // offsets in b_bucket need to change their offset_to_bucket
        let b_offsets = self.bucket_to_offsets.remove(&b_bucket).unwrap();
        for b_offset in &b_offsets {
            self.offset_to_bucket.insert(*b_offset, a_bucket);
        }
        self.bucket_to_offsets.get_mut(&a_bucket).unwrap().extend(b_offsets);
    }
}

