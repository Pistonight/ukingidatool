use std::{
    collections::{BTreeMap , HashSet},
    sync::{mpsc, Arc, RwLock},
};

use itertools::Itertools;

use crate::{parsed::TypeYaml, util::ProgressPrinter};

use super::{Namespace,EnumDef, MemberDef, StructDef, TypeComp, TypeDef, TypeInfo, TypeName, TypePrim, TypeResolver, UnionDef};

pub struct TypeStore {
    resolver: TypeResolver,
    pub names: BTreeMap<usize, TypeName>,
    pub referenced: HashSet<usize>,
}

impl TypeStore {
    pub fn resolve(mut offset_to_type: BTreeMap<usize, TypeInfo>, offset_to_ns: &BTreeMap<usize, Namespace>) -> Self {
        // use -1 for void
        offset_to_type.insert(usize::MAX, TypeInfo::Prim(TypePrim::Void));

        let mut resolver = TypeResolver::new(offset_to_type.into_iter().collect());
        resolver.merge_typedefs();
        let resolver = Arc::new(RwLock::new(resolver));
        TypeResolver::merge_pass1(&resolver);
        TypeResolver::merge_primitives(&resolver);
        TypeResolver::merge_enums(&resolver);
        TypeResolver::merge_structs(&resolver);
        TypeResolver::merge_unions(&resolver);
        TypeResolver::merge_pointers(&resolver);
        TypeResolver::merge_pass2(&resolver);

        let names = {
            resolver.read().unwrap().resolve_names()
                .into_iter()
                .map(|(bucket, name)| (bucket, name.convert_anonymous(offset_to_ns)))
                .collect()
        };

        let resolver = Arc::into_inner(resolver).unwrap();
        let resolver = resolver.into_inner().unwrap();

        Self { resolver, names, referenced: HashSet::new()}
    }
}

// fn info_to_def(
//     bucket: usize,
//     ty: &TypeInfo,
//     offset_to_bucket: &BTreeMap<usize, usize>,
//     bucket_to_def: &BTreeMap<usize, Arc<TypeDef>>,
//     bucket_to_name: &BTreeMap<usize, TypeName>,
// ) -> Arc<TypeDef> {
//     if let Some(def) = bucket_to_def.get(&bucket) {
//         return Arc::clone(def);
//     }
//     let def = match ty {
//         TypeInfo::Typedef(_, t) => {
//             // let bucket = *offset_to_bucket.get(&t).unwrap();
//             // match bucket_to_def.get(&bucket) {
//             //     Some(def) => Ok(Arc::clone(def)),
//             //     None => {
//             //         let base_ty = offset_to_info.get(&t).unwrap();
//             //         return info_to_def(offset, bucket, base_ty.clone(), offset_to_info, offset_to_def, bucket_to_name);
//             //     }
//             // }
//             panic!("Typedefs should have been resolved");
//         }
//         TypeInfo::Prim(x) => {
//             Arc::new(TypeDef::Prim(x.clone()))
//         },
//         TypeInfo::Struct(x) => {
//             let mut member_tys = Vec::with_capacity(x.members.len());
//             for m in &x.members {
//                 let m_bucket = *offset_to_bucket.get(&m.ty_offset).unwrap();
//                 let m_
//                 let m_ty = match bucket_to_def.get(&m_bucket) {
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
//                 let m_bucket = *offset_to_bucket.get(&m.1).unwrap();
//                 let m_ty = match bucket_to_def.get(&m_bucket) {
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
//             let bucket = *offset_to_bucket.get(&x).unwrap();
//             let base = match bucket_to_def.get(&bucket) {
//                 Some(def) => Arc::clone(def),
//                 None => {
//                     return Err(ty);
//                 }
//             };
//             Ok(Arc::new(TypeDef::Comp(TypeComp::Ptr(base))))
//         }
//         TypeInfo::Comp(TypeComp::Array(x, count)) => {
//             let bucket = *offset_to_bucket.get(&x).unwrap();
//             let base = match bucket_to_def.get(&bucket) {
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
//             let bucket = *offset_to_bucket.get(&ret_ty).unwrap();
//             let ret_ty2 = match bucket_to_def.get(&bucket) {
//                 Some(def) => Arc::clone(def),
//                 None => {
//                     return Err(TypeInfo::Comp(TypeComp::Subroutine(
//                         ret_ty, param_ty,
//                     )));
//                 }
//             };
//             let mut param_tys = Vec::with_capacity(param_ty.len());
//             for p in &param_ty {
//                 let p_bucket = *offset_to_bucket.get(p).unwrap();
//                 let p_ty = match bucket_to_def.get(&p_bucket) {
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
//             let bucket = *offset_to_bucket.get(&this_ty).unwrap();
//             let this_ty2 = match bucket_to_def.get(&bucket) {
//                 Some(def) => Arc::clone(def),
//                 None => {
//                     return Err(TypeInfo::Comp(TypeComp::Ptmf(
//                         this_ty, ret_ty, param_ty,
//                     )));
//                 }
//             };
//             let bucket = *offset_to_bucket.get(&ret_ty).unwrap();
//             let ret_ty2 = match bucket_to_def.get(&bucket) {
//                 Some(def) => Arc::clone(def),
//                 None => {
//                     return Err(TypeInfo::Comp(TypeComp::Ptmf(
//                         this_ty, ret_ty, param_ty,
//                     )));
//                 }
//             };
//             let mut param_tys = Vec::with_capacity(param_ty.len());
//             for p in &param_ty {
//                 let p_bucket = *offset_to_bucket.get(p).unwrap();
//                 let p_ty = match bucket_to_def.get(&p_bucket) {
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

impl TypeStore {
    pub fn are_equal(&self, a: usize, b: usize) -> bool {
        let a = self.resolver.offset_to_bucket.get(&a).unwrap();
        let b = self.resolver.offset_to_bucket.get(&b).unwrap();
        *a == *b
    }

    pub fn get_bucket(&self, offset: usize) -> usize {
        *self.resolver.offset_to_bucket.get(&offset).unwrap()
    }

    pub fn get_name(&self, offset: usize) -> TypeName {
        let bucket = self.resolver.offset_to_bucket.get(&offset).unwrap();
        self.names.get(&bucket).unwrap().clone()
    }

    pub fn mark_referenced(&mut self, offset: usize) {
        let bucket = *self.resolver.offset_to_bucket.get(&offset).unwrap();
        if !self.referenced.insert(bucket) {
            // already marked
            return;
        }
        // recursively mark
        let ty = self.resolver.offset_to_type.get(&offset).unwrap();
        match ty {
            TypeInfo::Prim(_) => {}
            TypeInfo::Enum(_) => {}
            TypeInfo::Typedef(_, t) => {
                self.mark_referenced(*t);
            }
            TypeInfo::Struct(s) => {
                let tys = s.members.iter().map(|m| m.ty_offset).collect::<Vec<_>>();
                for t in tys {
                    self.mark_referenced(t);
                }
            }
            TypeInfo::Union(u) => {
                let tys = u.members.iter().map(|m| m.1).collect::<Vec<_>>();
                for t in tys {
                    self.mark_referenced(t);
                }
            }
            TypeInfo::Comp(TypeComp::Ptr(t)) => {
                self.mark_referenced(*t);
            }
            TypeInfo::Comp(TypeComp::Array(t, _)) => {
                self.mark_referenced(*t);
            }
            TypeInfo::Comp(TypeComp::Subroutine(ret_ty, param_ty)) => {
                let tys = std::iter::once(*ret_ty).chain(param_ty.iter().copied()).collect::<Vec<_>>();
                for t in tys {
                    self.mark_referenced(t);
                }
            }
            TypeInfo::Comp(TypeComp::Ptmf(this_ty, ret_ty, param_ty)) => {
                let tys = std::iter::once(*this_ty).chain(std::iter::once(*ret_ty)).chain(param_ty.iter().copied()).collect::<Vec<_>>();
                for t in tys {
                    self.mark_referenced(t);
                }
            }
        }
    }

    pub fn create_defs(&self, offset_to_ns: &BTreeMap<usize, Namespace>) -> BTreeMap<String, TypeDef> {
        let progress = ProgressPrinter::new(self.referenced.len(), "Create type definitions");
        let mut name_to_def = BTreeMap::new();
        for (i, bucket) in self.referenced.iter().enumerate() {
            progress.print(i, "");
            if let Some((name, info)) = self.get_info_for_def(*bucket) {
                let name = match name {
                    Some(n) => n,
                    None => {
                        let suffix = match &info {
                            TypeInfo::Enum(_) => "anonmymous_enum",
                            TypeInfo::Struct(_) => "anonymous_struct",
                            TypeInfo::Union(_) => "anonymous_union",
                            TypeInfo::Comp(c) => {
                                if let TypeComp::Ptmf(_, _, _) = c {
                                    "anonymous_ptmf"
                                } else {
                                    unreachable!()
                                }
                            }
                            _ => unreachable!(),
                        };
                        offset_to_ns.get(&bucket).unwrap().get_with(suffix)
                    }
                };
                let key = name.clone();
                let def = match info {
                    TypeInfo::Enum(x) => {
                        TypeDef::Enum(EnumDef {
                            name,
                            size: x.size,
                            enumerators: x.enumerators.clone(),
                        })
                    }
                    TypeInfo::Struct(x) => {
                        let members = x.members.into_iter().map(|m| {
                            let name = m.name.unwrap_or_else(|| format!("field_{:x}", m.offset));
                            let ty_yaml = self.get_name(m.ty_offset).yaml_string();
                            MemberDef {
                                offset: m.offset,
                                name,
                                is_base: m.is_base,
                                ty_yaml,
                            }
                        });
                        TypeDef::Struct(StructDef {
                            name,
                            vtable: x.vtable.clone(),
                            size: x.size,
                            members: members.collect(),
                        })
                    }
                    TypeInfo::Union(x) => {
                        let members = x.members.into_iter()
                            .enumerate()
                            .map(|(i, (name, offset))| {
                                let name = name.unwrap_or_else(|| format!("_{}", i));
                            let ty_yaml = self.get_name(offset).yaml_string();
                            (name, ty_yaml)
                        });
                        TypeDef::Union(UnionDef {
                            name,
                            size: x.size,
                            members: members.collect(),
                        })
                    }
                    TypeInfo::Comp(c) => {
                        if let TypeComp::Ptmf(_, _, _) = c {
                            TypeDef::ptmf(name)
                        } else {
                            unreachable!()
                        }
                    }
                    _ => unreachable!(),
                };
                name_to_def.insert(key, def);
            }
        }
        progress.done();
        name_to_def
    }

    fn get_info_for_def(&self, bucket: usize) -> Option<(Option<String>, TypeInfo)> {
        let name = self.names.get(&bucket).unwrap();
        let name = match name {
            TypeName::Prim(_) => return None,
            TypeName::Comp(c) => {
                if let TypeComp::Ptmf(this,_,_) = c.as_ref() {
                    if let TypeName::Name(n) = this {
                        Some(format!("{}_ptmf", n))
                    } else {
                        None
                    }
                } else {
                    return None;
                }
            },
            TypeName::Anon(_, _) => None,
            TypeName::Name(n) => Some(n.clone()),
        };
        let mut is_struct = None;
        let mut is_union = None;
        for offset in self.resolver.bucket_to_offsets.get(&bucket).unwrap() {
            let ty = self.resolver.offset_to_type.get(offset).unwrap();
            match ty {
                TypeInfo::Comp(TypeComp::Ptmf(_, _, _)) => return Some((name, ty.clone())),
                TypeInfo::Enum(_) => return Some((name ,ty.clone())),
                TypeInfo::Struct(_) => if is_struct.is_none() { is_struct = Some(ty.clone()) },
                TypeInfo::Union(_) => if is_union.is_none() { is_union = Some(ty.clone()) },
                _ => return None,
            }
        }

        if let Some(ty) = is_struct {
            Some((name, ty))
        } else if let Some(ty) = is_union {
            Some((name, ty))
        } else {
            None
        }
    }

}
