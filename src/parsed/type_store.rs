use std::{
    collections::{BTreeMap , HashSet},
    sync::{mpsc, Arc, RwLock},
};

use itertools::Itertools;

use crate::util::ProgressPrinter;

use super::{EnumDef, MemberDef, StructDef, TypeComp, TypeDef, TypeInfo, TypePrim, TypeResolver, UnionDef};

pub struct TypeStore {
    offset_to_def: BTreeMap<usize, Arc<TypeDef>>,
}

impl TypeStore {
    pub fn resolve(mut offset_to_type: BTreeMap<usize, TypeInfo>) -> Self {
        // use -1 for void
        offset_to_type.insert(usize::MAX, TypeInfo::Prim(TypePrim::Void));

        let pool = threadpool::Builder::new().build();

        let mut resolver = TypeResolver::new(offset_to_type.into_iter().collect());
        resolver.merge_typedefs();
        let resolver = Arc::new(RwLock::new(resolver));

        // Use a small window of 8192 to get 95% of the types deduped quickly
        let window = 8192;
        let threshold = 0.05;
        loop {
            let keys = {
                let resolver = resolver.read().unwrap();
                resolver.bucket_to_offsets.keys().copied().collect::<Vec<_>>()
            };
            let progress = ProgressPrinter::new(keys.len(), "Deduplicating types");
            progress.print(0, "");
            let (send, recv) = mpsc::sync_channel(pool.max_count() * 2);
            for jobs in &keys.iter().chunks(window) {
                let jobs = jobs.copied().collect::<Vec<_>>();
                let resolver = Arc::clone(&resolver);
                let send = send.clone();
                pool.execute(move || {
                    let resolver = resolver.read().unwrap();
                    let len = jobs.len();
                    let merge_sets = resolver.find_mergeable(&jobs);
                    send.send((merge_sets,len) ).unwrap()
                });
            }
            drop(send);
            let mut merge_sets = Vec::new();
            let mut total = 0;
            for (m,l) in recv {
                total += l;
                progress.print(total, "");
                merge_sets.extend(m);
            }
            progress.done();
            {
                let mut resolver = resolver.write().unwrap();
                let before = resolver.bucket_to_offsets.len();
                for m in merge_sets {
                    resolver.merge_set(&m);
                }
                let after = resolver.bucket_to_offsets.len();
                if ((before - after) as f64 / before as f64) < threshold {
                    break;
                }
            }
        }

        TypeResolver::merge_simple_types(&resolver, &pool, false);

        // now do the same, to composite types
        let window = 16384;
        let threshold = 0.01;
        loop {
            let keys = {
                let resolver = resolver.read().unwrap();
                resolver.bucket_to_offsets.keys().filter_map(|k| {
                    match resolver.offset_to_type.get(k) {
                        Some(TypeInfo::Comp(TypeComp::Ptr(_))) => Some(*k),
                        _ => None
                    }
                }).collect::<Vec<_>>()
            };
            let progress = ProgressPrinter::new(keys.len(), "Deduplicating types");
            progress.print(0, "");
            let (send, recv) = mpsc::sync_channel(pool.max_count() * 2);
            for jobs in &keys.iter().chunks(window) {
                let jobs = jobs.copied().collect::<Vec<_>>();
                let resolver = Arc::clone(&resolver);
                let send = send.clone();
                pool.execute(move || {
                    let resolver = resolver.read().unwrap();
                    let len = jobs.len();
                    let merge_sets = resolver.find_mergeable(&jobs);
                    send.send((merge_sets,len) ).unwrap()
                });
            }
            drop(send);
            let mut merge_sets = Vec::new();
            let mut total = 0;
            for (m,l) in recv {
                total += l;
                progress.print(total, "");
                merge_sets.extend(m);
            }
            progress.done();
            let before = {
                resolver.read().unwrap().bucket_to_offsets.len()
            };
            {
                let mut resolver = resolver.write().unwrap();
                for m in merge_sets {
                    resolver.merge_set(&m);
                }
            }
            TypeResolver::merge_simple_types(&resolver, &pool, false);
            {
                let resolver = resolver.read().unwrap();
                let after = resolver.bucket_to_offsets.len();
                if ((before - after) as f64 / before as f64) < threshold {
                    break;
                }
            }
        }
        loop {
            let before = {
                resolver.read().unwrap().bucket_to_offsets.len()
            };
            TypeResolver::merge_pointers(&resolver, &pool);
            let after = {
                resolver.read().unwrap().bucket_to_offsets.len()
            };
            if before == after {
                break;
            }
        }
        loop {
            let before = {
                resolver.read().unwrap().bucket_to_offsets.len()
            };
            TypeResolver::merge_all_types(&resolver, &pool);
            let after = {
                resolver.read().unwrap().bucket_to_offsets.len()
            };
            if before == after {
                break;
            }
        }

        // TypeResolver::merge_pointers(&resolver, &pool);
        // TypeResolver::merge_simple_types(&resolver, &pool);

        // let mut exprs = TypeResolver::make_exprs(&resolver, &pool);
        // TypeResolver::eval_all_exprs(&resolver, &pool, &mut exprs);
        //
        //
        //
        // let resolver_rc = Arc::new(resolver);
        // let mut exprs = TypeResolver::make_exprs(&resolver_rc, &pool);
        // resolver = Arc::into_inner(resolver_rc).unwrap();
        //
        //
        // loop {
        //     let resolver_rc = Arc::new(resolver);
        //     let results = TypeResolver::eval_exprs(&resolver_rc, &pool, &mut progress, &mut exprs);
        //     pool.join();
        //     resolver = Arc::into_inner(resolver_rc).unwrap();
        //     if results.is_empty() {
        //         break;
        //     }
        //     resolver.merge_all(&results, &mut progress);
        // }
        // progress.done();
        // drop(pool);
        {
            let resolver = resolver.read().unwrap();
            let names = resolver.get_names();
            for (bucket, offsets) in &resolver.bucket_to_offsets {
                let mut best_offset = None;
                let mut best_name = None;
                for offset in offsets {
                    let name = names.get(offset).unwrap();
                    match (&mut best_offset, &mut best_name) {
                        (Some(o), Some(n)) => {
                            if name.is_preferred_over(&n, &names) == std::cmp::Ordering::Greater {
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
                println!("0x{1:08x}: {0}", best_name.unwrap().display(&names)  , best_offset.unwrap());
            }
        }

        println!("{} types", resolver.read().unwrap().bucket_to_offsets.len());
        todo!();









        // let mut offset_type = resolver.offset_to_type.iter().map(|(k, v)| (*k, v.clone())).collect::<Vec<_>>();
        // let mut offset_to_def = BTreeMap::new();
        // let mut next_offset_type = Vec::with_capacity(offset_type.len());
        // while !offset_type.is_empty() {
        //     while let Some((offset, ty)) = offset_type.pop() {
        //         let bucket = *resolver.offset_to_bucket.get(&offset).unwrap();
        //         if let Some(def) = offset_to_def.get(&bucket) {
        //             offset_to_def.insert(offset, Arc::clone(def));
        //             continue;
        //         }
        //         if bucket != offset {
        //             // not ready yet
        //             next_offset_type.push((bucket, ty));
        //             continue;
        //         }
        //         match info_to_def(offset, bucket, ty, 
        //             &resolver.offset_to_type,
        //             &offset_to_def, &resolver.bucket_to_name) {
        //             Ok(def) => {
        //                 offset_to_def.insert(offset, def);
        //             }
        //             Err(ty) => {
        //                 next_offset_type.push((offset, ty));
        //             }
        //         }
        //     }
        //     std::mem::swap(&mut offset_type, &mut next_offset_type);
        // }

        // for (offset, name) in offset_to_name {
        //     let def = match offset_to_def.get(&offset) {
        //         Some(def) => Arc::clone(def),
        //         None => {
        //             panic!("Failed to resolve type at offset 0x{:08x}", offset);
        //         }
        //     };
        //     println!("0x{:08x}: {} -> {}", offset, name, def);
        // }

        // Self { offset_to_def }
    }
}

// fn info_to_def(
//     offset: usize,
//     bucket: usize,
//     ty: TypeInfo,
//     offset_to_info: &HashMap<usize, TypeInfo>,
//     offset_to_def: &HashMap<usize, Arc<TypeDef>>,
//     bucket_to_name: &HashMap<usize, TypeName>,
// ) -> Result<Arc<TypeDef>, TypeInfo> {
//     match ty {
//         TypeInfo::Typedef(_, t) => {
//             match offset_to_def.get(&t) {
//                 Some(def) => Ok(Arc::clone(def)),
//                 None => {
//                     let base_ty = offset_to_info.get(&t).unwrap();
//                     return info_to_def(offset, bucket, base_ty.clone(), offset_to_info, offset_to_def, bucket_to_name);
//                 }
//             }
//         }
//         TypeInfo::Prim(x) => Ok(Arc::new(TypeDef::Prim(x))),
//         TypeInfo::Struct(x) => {
//             let mut member_tys = Vec::with_capacity(x.members.len());
//             for m in &x.members {
//                 let m_ty = match offset_to_def.get(&m.ty_offset) {
//                     Some(def) => Arc::clone(def),
//                     None => {
//                         return Err(TypeInfo::Struct(x));
//                     }
//                 };
//                 member_tys.push(m_ty);
//             }
//             let members = member_tys
//                 .into_iter()
//                 .zip(x.members.into_iter())
//                 .flat_map(|(ty, m)| {
//                     if ty.is_zst() {
//                         None // remove zero-sized members
//                     } else {
//                         Some(
//                     MemberDef {
//                         offset: m.offset,
//                         name: m.name,
//                         is_base: m.is_base,
//                         ty,
//                     })}
//                 }) .collect();
//             let s = StructDef {
//                 name: bucket_to_name.get(&bucket).unwrap().get_named().map(|x| x.to_string()),
//                 vtable: x.vtable,
//                 size: x.size,
//                 members,
//             };
//             Ok(Arc::new(TypeDef::Struct(s)))
//         }
//         TypeInfo::Enum(x) => {
//             let e = EnumDef {
//                 name: bucket_to_name.get(&bucket).unwrap().get_named().map(|x| x.to_string()),
//                 size: x.size,
//                 enumerators: x.enumerators,
//             };
//             Ok(Arc::new(TypeDef::Enum(e)))
//         }
//         TypeInfo::Union(x) => {
//             let mut member_tys = Vec::with_capacity(x.members.len());
//             for m in &x.members {
//                 let m_ty = match offset_to_def.get(&m.1) {
//                     Some(def) => Arc::clone(def),
//                     None => {
//                         return Err(TypeInfo::Union(x));
//                     }
//                 };
//                 member_tys.push(m_ty);
//             }
//             let members = member_tys
//                 .into_iter()
//                 .zip(x.members.into_iter())
//                 .map(|(ty, m)| (m.0, ty))
//                 .collect();
//             let u = UnionDef { 
//                 name: bucket_to_name.get(&bucket).unwrap().get_named().map(|x| x.to_string()),
//                 size: x.size,
//                 members };
//             Ok(Arc::new(TypeDef::Union(u)))
//         }
//         TypeInfo::Comp(TypeComp::Ptr(x)) => {
//             let base = match offset_to_def.get(&x) {
//                 Some(def) => Arc::clone(def),
//                 None => {
//                     return Err(ty);
//                 }
//             };
//             Ok(Arc::new(TypeDef::Comp(TypeComp::Ptr(base))))
//         }
//         TypeInfo::Comp(TypeComp::Array(x, count)) => {
//             let base = match offset_to_def.get(&x) {
//                 Some(def) => Arc::clone(def),
//                 None => {
//                     return Err(ty);
//                 }
//             };
//             Ok(Arc::new(TypeDef::Comp(TypeComp::Array(
//                 base, count,
//             ))))
//         }
//         TypeInfo::Comp(TypeComp::Subroutine(ret_ty, param_ty)) => {
//             let ret_ty2 = match offset_to_def.get(&ret_ty) {
//                 Some(def) => Arc::clone(def),
//                 None => {
//                     return Err(TypeInfo::Comp(TypeComp::Subroutine(
//                         ret_ty, param_ty,
//                     )));
//                 }
//             };
//             let mut param_tys = Vec::with_capacity(param_ty.len());
//             for p in &param_ty {
//                 let p_ty = match offset_to_def.get(p) {
//                     Some(def) => Arc::clone(def),
//                     None => {
//                         return Err(TypeInfo::Comp(TypeComp::Subroutine(
//                             ret_ty, param_ty,
//                         )));
//                     }
//                 };
//                 param_tys.push(p_ty);
//             }
//             Ok(Arc::new(TypeDef::Comp(TypeComp::Subroutine(
//                 ret_ty2, param_tys,
//             ))))
//         }
//         TypeInfo::Comp(TypeComp::Ptmf(this_ty, ret_ty, param_ty)) => {
//             let this_ty2 = match offset_to_def.get(&this_ty) {
//                 Some(def) => Arc::clone(def),
//                 None => {
//                     return Err(TypeInfo::Comp(TypeComp::Ptmf(
//                         this_ty, ret_ty, param_ty,
//                     )));
//                 }
//             };
//             let ret_ty2 = match offset_to_def.get(&ret_ty) {
//                 Some(def) => Arc::clone(def),
//                 None => {
//                     return Err(TypeInfo::Comp(TypeComp::Ptmf(
//                         this_ty, ret_ty, param_ty,
//                     )));
//                 }
//             };
//             let mut param_tys = Vec::with_capacity(param_ty.len());
//             for p in &param_ty {
//                 let p_ty = match offset_to_def.get(p) {
//                     Some(def) => Arc::clone(def),
//                     None => {
//                         return Err(TypeInfo::Comp(TypeComp::Ptmf(
//                             this_ty, ret_ty, param_ty,
//                         )));
//                     }
//                 };
//                 param_tys.push(p_ty);
//             }
//             Ok(Arc::new(TypeDef::Comp(TypeComp::Ptmf(
//                 this_ty2, ret_ty2, param_tys,
//             ))))
//         }
//     }
// }

