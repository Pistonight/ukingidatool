use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::sync::{mpsc, Arc, RwLock};

use itertools::Itertools;
use threadpool::ThreadPool;

use super::{TypeInfo, TypeName, TypeComp};

use crate::util::ProgressPrinter;
use crate::worker::Pool;

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

struct InputIter {
    keys: Vec<usize>,
    a: usize,
    b: usize,
    chunk_size: usize
}
impl InputIter {
    fn new(keys: Vec<usize>, chunk_size: usize) -> Self {
        Self {
            keys,
            a: 0,
            b: 1,
            chunk_size
        }
    }
    fn total(&self) -> usize {
        let l = match self.keys.len() {
            0 => 0,
            n => n * (n - 1) / 2
        };
        l
    }
    fn next_single(&mut self) -> Option<(usize, usize)> {
        let len = self.keys.len();
        self.b+= 1;
        if self.b>= len {
            self.a+= 1;
            self.b= self.a+ 1;
        }
        if self.a>= len || self.b>= len {
            return None;
        }
        Some((self.keys[self.a], self.keys[self.b]))
    }
    
}
impl Iterator for InputIter {
    type Item = Vec<(usize, usize)>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut v = Vec::with_capacity(self.chunk_size);
        for _ in 0..self.chunk_size {
            match self.next_single() {
                Some(x) => v.push(x),
                None => break,
            }
        }
        if v.is_empty() {
            None
        } else {
            Some(v)
        }
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

    pub fn merge_typedefs(&mut self) {
        let progress = ProgressPrinter::new(self.offset_to_type.len(), "Merge typedefs");
        let mut typedefs = Vec::new();
        for (i, (offset, ty)) in self.offset_to_type.iter().enumerate() {
            if let TypeInfo::Typedef(_, ty_offset) = ty {
                progress.print(i, format!("0x{:08x}: {}", offset, ty));
                typedefs.push((*ty_offset, *offset));
            }
        }
        progress.done();
        for (a,b) in typedefs {
            self.merge(a, b);
        }
    }

    pub fn merge_pass1(s: &Arc<RwLock<Self>>) {
        Self::merge_pass_by_filter(s, 8192, 0.1, |_| true, "Deduplicate types pass 1");
    }

    pub fn merge_primitives(s: &Arc<RwLock<Self>>) {
        Self::merge_by_filter(s, |x| {
            let s = s.read().unwrap();
            let ty = s.offset_to_type.get(&x).unwrap();
            matches!(ty, TypeInfo::Prim(_))
        }, "Deduplicate primitives");
    }

    pub fn merge_structs(s: &Arc<RwLock<Self>>) {
        Self::merge_by_filter(s, |x| {
            let s = s.read().unwrap();
            let ty = s.offset_to_type.get(&x).unwrap();
            matches!(ty, TypeInfo::Struct(_))
        }, "Deduplicate structs");
    }

    pub fn merge_enums(s: &Arc<RwLock<Self>>) {
        Self::merge_by_filter(s, |x| {
            let s = s.read().unwrap();
            let ty = s.offset_to_type.get(&x).unwrap();
            matches!(ty, TypeInfo::Enum(_))
        }, "Deduplicate enums");
    }

    pub fn merge_unions(s: &Arc<RwLock<Self>>) {
        Self::merge_by_filter(s, |x| {
            let s = s.read().unwrap();
            let ty = s.offset_to_type.get(&x).unwrap();
            matches!(ty, TypeInfo::Union(_))
        }, "Deduplicate unions");
    }

    pub fn merge_pointers(s: &Arc<RwLock<Self>>) {
        Self::merge_by_filter(s, |x| {
            let s = s.read().unwrap();
            let ty = s.offset_to_type.get(&x).unwrap();
            matches!(ty, TypeInfo::Comp(TypeComp::Ptr(_)))
        }, "Deduplicate pointer types");
    }

    pub fn merge_pass2(s: &Arc<RwLock<Self>>) {
        Self::merge_by_filter(s, |_| true, "Deduplicate types pass 2");
    }

    pub fn merge_pass_by_filter(s: &Arc<RwLock<Self>>,
        window: usize,
        threshold: f64,
        filter: impl Fn(usize) -> bool,
        msg: &str,
    ) {
        let mut progress = ProgressPrinter::new(0, msg);
        for iteration in 1.. {
            let (keys_chunked, total) = {
                let s = s.read().unwrap();
                let mut keys_chunked = Vec::new();
                let filtered = s.filtered_buckets(&filter);
                for chunk in &filtered.into_iter().chunks(window) {
                    keys_chunked.push(chunk.collect::<Vec<_>>());
                }
                (keys_chunked, s.bucket_to_offsets.len())
            };
            progress.set_total(total);
            progress.set_prefix(format!("{msg} (iteration {})", iteration));
            progress.print(0, "preparing");
            progress.reset_timer();
            let s2 = Arc::clone(s);
            let input = keys_chunked.into_iter().map(move |x| (Arc::clone(&s2), x));
            let pool = Pool::run(input, move |(s, keys)| {
                let s = s.read().unwrap();
                let len = keys.len();
                (s.find_mergeable(&keys), len)
            });
            let mut merge_sets = Vec::new();
            let mut count = 0;
            for (m, l) in pool {
                count += l;
                progress.print(count, "");
                merge_sets.extend(m);
            }
            progress.reset_timer();
            progress.set_total(merge_sets.len());
            progress.set_prefix(format!("{msg} (iteration {}), merging {total} types", iteration));
            {
                let mut s = s.write().unwrap();
                for (i, merge_set) in merge_sets.into_iter().enumerate() {
                    progress.print(i, "");
                    s.merge_set(&merge_set);
                }
                let after = s.bucket_to_offsets.len();
                if ((total - after) as f64 / total as f64) < threshold {
                    break;
                }
                if window * crate::worker::num_threads() > after {
                    // can no longer take 100% advantage of CPU
                    break;
                }
            }
        }
        progress.done();
    }


    pub fn merge_by_filter(s: &Arc<RwLock<Self>>,
        f: impl Fn(usize) -> bool,
        msg: &str) {
        let mut last_keys_len = 0;
        let mut progress = ProgressPrinter::new(0, msg);
        for iteration in 1.. {
            let keys = {
                let s = s.read().unwrap();
                s.filtered_buckets(&f).into_iter().collect::<Vec<_>>()
            };
            if keys.len() == last_keys_len {
                break;
            }
            last_keys_len = keys.len();
            let s2 = Arc::clone(s);
            let input = InputIter::new(keys, 81920);
            let total = input.total();
            let input = input.map(move |x| (Arc::clone(&s2), x));
            progress.set_prefix(format!("{msg} (iteration {})", iteration));
            progress.set_total(total);
            progress.reset_timer();
            progress.print(0, "preparing");
            let pool = Pool::run(input, move |(s, keys)| {
                let s = s.read().unwrap();
                let len = keys.len();
                (s.find_mergeable2(&keys), len)
            });
            let mut merge_sets = BTreeMap::new();
            let mut count = 0;
            for (m, l) in pool {
                count += l;
                progress.print(count, "");
                for (k, v) in m {
                    let set = merge_sets.entry(k).or_insert_with(BTreeSet::new);
                    set.extend(v);
                }
            }
            {
                let mut s = s.write().unwrap();
                for merge_set in merge_sets.into_values() {
                    let merge_set: Vec<usize> = merge_set.into_iter().collect();
                    s.merge_set(&merge_set);
                }
            }
        }
        progress.done();
    }

    pub fn resolve_names(&self) -> BTreeMap<usize, TypeName> {
        let base_names = self.get_base_names();
        let total_buckets = self.bucket_to_offsets.len();
        let mut bucket_to_name = BTreeMap::<usize, TypeName>::new();
        let mut bucket_to_name_str = BTreeMap::<usize, HashSet<String>>::new();
        let mut last_len = 0;
        let progress = ProgressPrinter::new(total_buckets, "Resolve names");
        loop {
            if bucket_to_name.len() == total_buckets {
                break;
            }
            if last_len != 0 && last_len == bucket_to_name.len() {
                break;
            }
            last_len = bucket_to_name.len();
            // eprintln!("resolved {}/{}", bucket_to_name.len(), total_buckets);
            'outer: for (bucket, offsets) in &self.bucket_to_offsets {
                if bucket_to_name.contains_key(bucket) {
                    // already resolved
                    continue;
                }
                let mut best_offset = None;
                let mut best_name = None;
                let mut all_names = HashSet::new();
                for offset in offsets {
                    let name = match base_names.get(offset) {
                        Some(name) => name.clone(),
                        None => {
                            let bucket = self.offset_to_bucket.get(offset).unwrap();
                            match bucket_to_name.get(bucket) {
                                Some(name) => name.clone(),
                                None => {
                                    let ty = self.offset_to_type.get(offset).unwrap();
                                    match ty {
                                        TypeInfo::Prim(t) => TypeName::Prim(*t),
                                        TypeInfo::Typedef(name, _) => TypeName::Name(name.clone()),
                                        TypeInfo::Struct(x) => match &x.name {
                                            Some(name) => TypeName::Name(name.clone()),
                                            None => TypeName::Anon(*offset, "struct"),
                                        },
                                        TypeInfo::Enum(x) => match &x.name {
                                            Some(name) => TypeName::Name(name.clone()),
                                            None => TypeName::Anon(*offset, "enum"),
                                        },
                                        TypeInfo::Union(x) => match &x.name {
                                            Some(name) => TypeName::Name(name.clone()),
                                            None => TypeName::Anon(*offset, "union"),
                                        },
                                        TypeInfo::Comp(TypeComp::Ptr(x)) => {
                                            let bucket = self.offset_to_bucket.get(x).unwrap();
                                            let name = match bucket_to_name.get(bucket) {
                                                Some(name) => name.clone(),
                                                None => {
                                                    // can't resolve yet
                                                    continue 'outer;
                                                }
                                            };
                                            TypeName::Comp(Box::new(TypeComp::Ptr(name)))
                                        }
                                        TypeInfo::Comp(TypeComp::Array(x, len)) => {
                                            let bucket = self.offset_to_bucket.get(x).unwrap();
                                            let name = match bucket_to_name.get(bucket) {
                                                Some(name) => name.clone(),
                                                None => {
                                                    // can't resolve yet
                                                    continue 'outer;
                                                }
                                            };
                                            TypeName::Comp(Box::new(TypeComp::Array(name, *len)))
                                        }
                                        TypeInfo::Comp(TypeComp::Subroutine(r, p)) => {
                                            let bucket = self.offset_to_bucket.get(r).unwrap();
                                            let r_name = match bucket_to_name.get(bucket) {
                                                Some(name) => name.clone(),
                                                None => {
                                                    // can't resolve yet
                                                    continue 'outer;
                                                }
                                            };
                                            let mut p_names = Vec::new();
                                            for p_off in p {
                                                let p_bucket = self.offset_to_bucket.get(p_off).unwrap();
                                                let p_name = match bucket_to_name.get(p_bucket) {
                                                    Some(name) => name.clone(),
                                                    None => {
                                                        // can't resolve yet
                                                        continue 'outer;
                                                    }
                                                };
                                                p_names.push(p_name);
                                            }
                                            
                                            TypeName::Comp(Box::new(TypeComp::Subroutine(r_name, p_names)))
                                        }
                                        TypeInfo::Comp(TypeComp::Ptmf(t, r, p)) => {
                                            let bucket = self.offset_to_bucket.get(t).unwrap();
                                            let t_name = match bucket_to_name.get(bucket) {
                                                Some(name) => name.clone(),
                                                None => {
                                                    // can't resolve yet
                                                    continue 'outer;
                                                }
                                            };
                                            let bucket = self.offset_to_bucket.get(r).unwrap();
                                            let r_name = match bucket_to_name.get(bucket) {
                                                Some(name) => name.clone(),
                                                None => {
                                                    // can't resolve yet
                                                    continue 'outer;
                                                }
                                            };
                                            let mut p_names = Vec::new();
                                            for p_off in p {
                                                let p_bucket = self.offset_to_bucket.get(p_off).unwrap();
                                                let p_name = match bucket_to_name.get(p_bucket) {
                                                    Some(name) => name.clone(),
                                                    None => {
                                                        // can't resolve yet
                                                        continue 'outer;
                                                    }
                                                };
                                                p_names.push(p_name);
                                            }
                                            
                                            TypeName::Comp(Box::new(TypeComp::Ptmf(t_name, r_name, p_names)))
                                        }
                                    }
                                }
                            }
                        }
                    };
                    let name_str = name.to_string();
                    if !name_str.starts_with("anon_0x") {
                        all_names.insert(name_str);
                    }
                    match (&mut best_offset, &mut best_name) {
                        (Some(o), Some(n)) => {
                            if name.is_preferred_over(&n) == std::cmp::Ordering::Greater {
                                best_offset = Some(offset);
                                best_name = Some(name.clone());
                            }
                        }
                        (None, None) => {
                            best_offset = Some(offset);
                            best_name = Some(name.clone());
                        }
                        _ => {}
                    }
                }
                let name_str = best_name.as_ref().unwrap().to_string();
                bucket_to_name.insert(*bucket, best_name.unwrap());
                progress.print(bucket_to_name.len(), &name_str);
                if let Some(names) = bucket_to_name_str.get_mut(bucket) {
                    names.extend(all_names);
                } else {
                    bucket_to_name_str.insert(*bucket, all_names);
                }
            }
        }
        progress.done();
        for (bucket, name) in &bucket_to_name{
            let n = name.to_string();
            println!("0x{:08x}: {}", bucket, n);
            if !name.is_primitive() {
                for name in bucket_to_name_str.get(bucket).unwrap() {
                    if name != &n {
                        println!("  {}", name);
                    }
                }
            }
        }
        for bucket in self.bucket_to_offsets.keys() {
            if !bucket_to_name.contains_key(bucket) {
                panic!("0x{:08x}: <unresolved>", bucket);
            }
        }
        bucket_to_name
    }

    pub fn get_base_names(&self) -> BTreeMap<usize, TypeName> {
        let mut names = BTreeMap::new();
        for (offset, ty) in &self.offset_to_type {
            let name = match ty {
                TypeInfo::Prim(t) => TypeName::Prim(*t),
                TypeInfo::Typedef(name, _) => TypeName::Name(name.clone()),
                TypeInfo::Struct(x) => match &x.name {
                    Some(name) => TypeName::Name(name.clone()),
                    None => TypeName::Anon(*offset, "struct"),
                },
                TypeInfo::Enum(x) => match &x.name {
                    Some(name) => TypeName::Name(name.clone()),
                    None => TypeName::Anon(*offset, "enum"),
                },
                TypeInfo::Union(x) => match &x.name {
                    Some(name) => TypeName::Name(name.clone()),
                    None => TypeName::Anon(*offset, "union"),
                },
                _ => continue,
                // TypeInfo::Comp(c) => {
                //     if !include_comp {
                //         continue;
                //     }
                //     match c {
                //         TypeInfo::Comp(TypeComp::Ptr(x)) => {
                //             TypeName::Comp(TypeComp::Ptr(*self.offset_to_bucket.get(x).unwrap()))
                //         }
                //         TypeInfo::Comp(TypeComp::Array(x, y)) => {
                //             TypeName::Comp(TypeComp::Array(*self.offset_to_bucket.get(x).unwrap(), *y))
                //         }
                //         TypeInfo::Comp(TypeComp::Subroutine(x, y)) => {
                //             let x = *self.offset_to_bucket.get(x).unwrap();
                //             let y = y.iter().map(|x| *self.offset_to_bucket.get(x).unwrap()).collect();
                //             TypeName::Comp(TypeComp::Subroutine(x, y))
                //         }
                //         TypeInfo::Comp(TypeComp::Ptmf(x, y, z)) => {
                //             let x = *self.offset_to_bucket.get(x).unwrap();
                //             let y = *self.offset_to_bucket.get(y).unwrap();
                //             let z = z.iter().map(|x| *self.offset_to_bucket.get(x).unwrap()).collect();
                //             TypeName::Comp(TypeComp::Ptmf(x, y, z))
                //         }
                //     }
                // }
            };
            names.insert(*offset, name);
        }
        names
    }

    pub fn filtered_buckets(&self, f: impl Fn(usize) -> bool) -> BTreeSet<usize> {
        self.bucket_to_offsets.keys().filter(|x| f(**x)).copied().collect()
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
        let mut merge_sets = Vec::new();
        for (i, a) in inputs.iter().enumerate() {
            let a = *a;
            let mut s = vec![a];
            for b in &inputs[i + 1..] {
                let b = *b;
                if a == b {
                    continue;
                }
                if self.needs_merging(a, b) {
                    s.push(b);
                }
            }
            if s.len() > 1 {
                merge_sets.push(s);
            }
        }
        merge_sets
    }
    pub fn find_mergeable2(&self, inputs: &Vec<(usize, usize)>) -> BTreeMap<usize, Vec<usize>> {
        let mut merge_sets = BTreeMap::new();
        for (a, b) in inputs {
            let a = *a;
            let b = *b;
            if a == b {
                continue;
            }
            let set = merge_sets.entry(a).or_insert_with(|| vec![a]);
            if self.needs_merging(a, b) {
                set.push(b);
            }
        }
        merge_sets
    }

    pub fn complete_vtables(&mut self) {
        let progress = ProgressPrinter::new(self.bucket_to_offsets.len(), "Merge vtables");

        for (i, offsets) in self.bucket_to_offsets.values().enumerate() {
            progress.print(i, "");
            let mut vtable = Vec::new();
            for offset in offsets {
                let ty = self.offset_to_type.get(offset).unwrap();
                if let TypeInfo::Struct(s) = ty {
                    if s.vtable.len() > vtable.len() {
                        for i in vtable.len()..s.vtable.len() {
                            vtable.push(s.vtable[i].clone());
                        }
                    }
                }
            }
            for offset in offsets {
                let ty = self.offset_to_type.get_mut(offset).unwrap();
                if let TypeInfo::Struct(s) = ty {
                    s.vtable = vtable.clone();
                }
            }
        }
        progress.done();
    }


    // pub fn eval_all_exprs(s: &Arc<RwLock<Self>>, pool: &ThreadPool, exprs: &mut Vec<Expr>) {
    //     let mut progress = ProgressPrinter::new(0, "Deduplicating types");
    //     loop {
    //         let results = TypeResolver::eval_exprs(s, &pool, &mut progress, exprs);
    //         if results.is_empty() {
    //             break s;
    //         }
    //         {
    //             s.write().unwrap().merge_all(&results, &mut progress);
    //         }
    //     };
    //     progress.done();
    // }

    // pub fn merge_simple_types(s: &Arc<RwLock<Self>>, pool: &ThreadPool, all: bool) {
    //     let mut exprs = Vec::new();
    //     let prim_keys = {
    //         let s2 = s.read().unwrap();
    //         s2.offset_to_type.iter().filter_map(|(offset, ty)| {
    //             if let TypeInfo::Prim(_) = ty {
    //                 Some(*s2.offset_to_bucket.get(offset).unwrap())
    //             } else {
    //                 None
    //             }
    //         }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
    //     };
    //     exprs.extend(Self::make_exprs_by_keys(s, pool, prim_keys, "Analyzing primitives"));
    //     let struct_keys = {
    //         let s2 = s.read().unwrap();
    //         s2.offset_to_type.iter().filter_map(|(offset, ty)| {
    //             if let TypeInfo::Struct(_) = ty {
    //                 Some(*s2.offset_to_bucket.get(offset).unwrap())
    //             } else {
    //                 None
    //             }
    //         }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
    //     };
    //     exprs.extend(Self::make_exprs_by_keys(s, pool, struct_keys, "Analyzing structs"));
    //     let union_keys = {
    //         let s2 = s.read().unwrap();
    //         s2.offset_to_type.iter().filter_map(|(offset, ty)| {
    //             if let TypeInfo::Union(_) = ty {
    //                 Some(*s2.offset_to_bucket.get(offset).unwrap())
    //             } else {
    //                 None
    //             }
    //         }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
    //     };
    //     exprs.extend(Self::make_exprs_by_keys(s, pool, union_keys, "Analyzing unions"));
    //     let enum_keys = {
    //         let s2 = s.read().unwrap();
    //         s2.offset_to_type.iter().filter_map(|(offset, ty)| {
    //             if let TypeInfo::Enum(_) = ty {
    //                 Some(*s2.offset_to_bucket.get(offset).unwrap())
    //             } else {
    //                 None
    //             }
    //         }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
    //     };
    //     exprs.extend(Self::make_exprs_by_keys(s, pool, enum_keys, "Analyzing enums"));
    //     if all {
    //     // let ptr_keys = {
    //     //     let s2 = s.read().unwrap();
    //     //     s2.offset_to_type.iter().filter_map(|(offset, ty)| {
    //     //         if let TypeInfo::Comp(TypeComp::Ptr(_)) = ty {
    //     //             Some(*s2.offset_to_bucket.get(offset).unwrap())
    //     //         } else {
    //     //             None
    //     //         }
    //     //     }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
    //     // };
    //     // exprs.extend(Self::make_exprs_by_keys(s, pool, ptr_keys, "Analyzing pointer types"));
    //     let array_keys = {
    //         let s2 = s.read().unwrap();
    //         s2.offset_to_type.iter().filter_map(|(offset, ty)| {
    //             if let TypeInfo::Comp(TypeComp::Array(_, _)) = ty {
    //                 Some(*s2.offset_to_bucket.get(offset).unwrap())
    //             } else {
    //                 None
    //             }
    //         }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
    //     };
    //     exprs.extend(Self::make_exprs_by_keys(s, pool, array_keys, "Analyzing array types"));
    //     let subroutine_keys = {
    //         let s2 = s.read().unwrap();
    //         s2.offset_to_type.iter().filter_map(|(offset, ty)| {
    //             if let TypeInfo::Comp(TypeComp::Subroutine(_, _)) = ty {
    //                 Some(*s2.offset_to_bucket.get(offset).unwrap())
    //             } else {
    //                 None
    //             }
    //         }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
    //     };
    //     exprs.extend(Self::make_exprs_by_keys(s, pool, subroutine_keys, "Analyzing subroutine types"));
    //     let ptmf_keys = {
    //         let s2 = s.read().unwrap();
    //         s2.offset_to_type.iter().filter_map(|(offset, ty)| {
    //             if let TypeInfo::Comp(TypeComp::Ptmf(_, _, _)) = ty {
    //                 Some(*s2.offset_to_bucket.get(offset).unwrap())
    //             } else {
    //                 None
    //             }
    //         }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
    //     };
    //     exprs.extend(Self::make_exprs_by_keys(s, pool, ptmf_keys, "Analyzing ptr-to-member-function types"));
    //     }
    //     Self::eval_all_exprs(s, pool, &mut exprs);
    // }

    // pub fn merge_pointers(s: &Arc<RwLock<Self>>, pool: &ThreadPool) {
    //     let ptr_keys = {
    //         let s2 = s.read().unwrap();
    //         s2.offset_to_type.iter().filter_map(|(offset, ty)| {
    //             if let TypeInfo::Comp(TypeComp::Ptr(_)) = ty {
    //                 Some(*s2.offset_to_bucket.get(offset).unwrap())
    //             } else {
    //                 None
    //             }
    //         }).collect::<BTreeSet<_>>().into_iter().collect::<Vec<_>>()
    //     };
    //     let merge_sets = s.read().unwrap().find_mergeable_progress(&ptr_keys, "Deduplicating pointers");
    //     for merge_set in merge_sets {
    //         s.write().unwrap().merge_set(&merge_set);
    //     }
    // }
    // pub fn merge_all_types(s: &Arc<RwLock<Self>>, pool: &ThreadPool) {
    //     let keys = {
    //         let s2 = s.read().unwrap();
    //         s2.bucket_to_offsets.keys().copied().collect::<Vec<_>>()
    //     };
    //     let merge_sets = s.read().unwrap().find_mergeable_progress(&keys, "Deduplicating all types");
    //     for merge_set in merge_sets {
    //         s.write().unwrap().merge_set(&merge_set);
    //     }
    // }


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
            return true;
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
                if a_struct.is_decl || b_struct.is_decl {
                    return a_struct.name == b_struct.name;
                }
                if a_struct.size != b_struct.size {
                    false
                } else if a_struct.size == 0 {
                    true // all ZSTs are equivalent
                } else if a_struct.members.len() != b_struct.members.len() {
                    false
                } else {
                    match (&a_struct.name, &b_struct.name) {
                        (Some(a_name), Some(b_name))if a_name != b_name => {
                            // if both A and B are named, they must be the same name to be considered
                            // equivalent, otherwise non-related types with the same layout will be
                            // merged
                            false
                        },
                        _ => {
                    // since vtable could be incomplete, we need to check the names
                    // instead of relying on the length
                    for (a_name, b_name) in a_struct.vtable.iter().zip(b_struct.vtable.iter()) {
                        if a_name.starts_with('~') && b_name.starts_with('~') {
                            // dtor names might be different, it's ok
                            continue;
                        }
                        if a_name != b_name {
                            return false;
                        }
                    }
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
                if a_union.is_decl || b_union.is_decl {
                    return a_union.name == b_union.name;
                }
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
                        let a_bucket = *self.offset_to_bucket.get(&a_member.1).unwrap();
                        let b_bucket = *self.offset_to_bucket.get(&b_member.1).unwrap();
                        if !self.are_equiv(a_bucket, b_bucket, memo) {
                            r = false;
                            break;
                        }
                    }
                    r
                }
            }
            (TypeInfo::Enum(a_enum), TypeInfo::Enum(b_enum)) => {
                if a_enum.is_decl || b_enum.is_decl {
                    return a_enum.name == b_enum.name;
                }
                // names matters if they don't have any enumerators
                if a_enum.enumerators.len() == 0 || b_enum.enumerators.len() == 0 {
                    match (&a_enum.name, &b_enum.name) {
                        (Some(a_name), Some(b_name)) => {
                            if a_name != b_name {
                                return false;
                            }
                        }
                        _ => (),
                    }
                }

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

