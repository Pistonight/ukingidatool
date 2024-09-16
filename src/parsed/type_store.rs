use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::sync::{Arc, RwLock};

use error_stack::{report, Result, ResultExt};

use super::{
    EnumDef, Error, MemberDef, NamespaceMap, StructDef, TypeComp, TypeDef, TypeInfo, TypeName,
    TypePrim, TypeResolver, TypeYaml, UnionDef,
};
use crate::util::ProgressPrinter;

pub struct TypeStore {
    resolver: TypeResolver,
    names: BTreeMap<usize, TypeName>,
    referenced: HashSet<usize>,
}

impl TypeStore {
    pub fn resolve(
        mut offset_to_type: BTreeMap<usize, TypeInfo>,
        namespaces: &NamespaceMap,
    ) -> Result<Self, Error> {
        // use -1 for void
        offset_to_type.insert(usize::MAX, TypeInfo::Prim(TypePrim::Void));

        let mut resolver = TypeResolver::new(offset_to_type);
        resolver.merge_typedefs();
        let resolver = Arc::new(RwLock::new(resolver));
        TypeResolver::merge_pass1(&resolver);
        TypeResolver::merge_primitives(&resolver);
        TypeResolver::merge_enums(&resolver);
        TypeResolver::merge_structs(&resolver);
        TypeResolver::merge_unions(&resolver);
        TypeResolver::merge_pointers(&resolver);
        TypeResolver::merge_pass2(&resolver);

        let resolver = Arc::into_inner(resolver).unwrap();
        let mut resolver = resolver.into_inner().unwrap();
        resolver
            .complete_vtables()
            .attach_printable_lazy(|| "While completing vtables")?;
        let names = resolver.resolve_names(namespaces)?;

        Ok(Self {
            resolver,
            names,
            referenced: HashSet::new(),
        })
    }
}

impl TypeStore {
    pub fn are_equal(&self, a: usize, b: usize) -> Result<bool, Error> {
        let a = self.resolver.get_bucket(a).attach_printable_lazy(|| {
            format!("While comparing offset: 0x{:08x} and 0x{:08x}", a, b)
        })?;
        let b = self.resolver.get_bucket(b).attach_printable_lazy(|| {
            format!(
                "While comparing offset: bucket 0x{:08x} and offset 0x{:08x}",
                a, b
            )
        })?;
        Ok(a == b)
    }

    pub fn get_bucket(&self, offset: usize) -> Result<usize, Error> {
        self.resolver.get_bucket(offset)
    }

    pub fn get_name(&self, offset: usize) -> Result<TypeName, Error> {
        let bucket = self
            .get_bucket(offset)
            .attach_printable_lazy(|| format!("While getting name for offset: 0x{:08x}", offset))?;
        self.names
            .get(&bucket)
            .ok_or(Error::UnresolvedName)
            .attach_printable_lazy(|| {
                format!(
                    "While getting name for offset: 0x{:08x} in bucket 0x{:08x}",
                    offset, bucket
                )
            })
            .cloned()
    }

    pub fn mark_referenced(&mut self, offset: usize) -> Result<(), Error> {
        self.mark_referenced_internal(offset, 0)
    }

    fn mark_referenced_internal(&mut self, offset: usize, depth: usize) -> Result<(), Error> {
        let bucket = self
            .resolver
            .get_bucket(offset)
            .attach_printable_lazy(|| format!("While marking offset: 0x{:08x}", offset))?;
        if !self.referenced.insert(bucket) {
            // already marked
            return Ok(());
        }
        // recursively mark
        for offset in self
            .resolver
            .get_bucket_offsets(bucket)?
            .iter()
            .copied()
            .collect::<Vec<_>>()
        {
            self.do_mark_offset(bucket, offset, depth)
                .attach_printable_lazy(|| format!("While marking offset: 0x{:08x}", offset))?;
        }
        Ok(())
    }

    fn do_mark_offset(&mut self, _bucket: usize, offset: usize, depth: usize) -> Result<(), Error> {
        #[cfg(feature = "debug-type-gc")]
        {
            let name = self.get_name(offset)?;
            println!(
                "{}(bucket_0x{:08x}) marking 0x{:08x} {name}",
                "  ".repeat(depth),
                _bucket,
                offset
            );
        }
        let _old_depth = depth;
        let depth = depth + 1;
        let ty = self
            .resolver
            .get_info(offset)
            .attach_printable_lazy(|| format!("While marking offset: 0x{:08x}", offset))?;
        match ty {
            TypeInfo::Prim(_) => {}
            TypeInfo::Enum(_) => {}
            TypeInfo::Typedef(_, t) => {
                #[cfg(feature = "debug-type-gc")]
                {
                    let name = self.get_name(*t)?;
                    println!(
                        "{}(offset_0x{:08x}) typedef -> 0x{:08x} {name}",
                        "  ".repeat(depth),
                        offset,
                        *t
                    );
                }
                self.mark_referenced_internal(*t, depth)
                    .attach_printable_lazy(|| format!("While marking offset: 0x{:08x}", offset))?;
            }
            TypeInfo::Struct(s) => {
                for t in s.members.iter().map(|m| m.ty_offset).collect::<Vec<_>>() {
                    #[cfg(feature = "debug-type-gc")]
                    {
                        let name = self.get_name(t)?;
                        println!(
                            "{}(offset_0x{:08x}) struct member 0x{:08x} {name}",
                            "  ".repeat(depth),
                            offset,
                            t
                        );
                    }
                    self.mark_referenced_internal(t, depth)
                        .attach_printable_lazy(|| {
                            format!("While marking offset: 0x{:08x}", offset)
                        })?;
                }
            }
            TypeInfo::Union(u) => {
                for t in u.members.iter().map(|m| m.1).collect::<Vec<_>>() {
                    #[cfg(feature = "debug-type-gc")]
                    {
                        let name = self.get_name(t)?;
                        println!(
                            "{}(offset_0x{:08x}) union member 0x{:08x} {name}",
                            "  ".repeat(depth),
                            offset,
                            t
                        );
                    }
                    self.mark_referenced_internal(t, depth)
                        .attach_printable_lazy(|| {
                            format!("While marking offset: 0x{:08x}", offset)
                        })?;
                }
            }
            TypeInfo::Comp(TypeComp::Ptr(t)) => {
                #[cfg(feature = "debug-type-gc")]
                {
                    let name = self.get_name(*t)?;
                    println!(
                        "{}(offset_0x{:08x}) pointer -> 0x{:08x} {name}",
                        "  ".repeat(depth),
                        offset,
                        t
                    );
                }
                self.mark_referenced_internal(*t, depth)
                    .attach_printable_lazy(|| format!("While marking offset: 0x{:08x}", offset))?;
            }
            TypeInfo::Comp(TypeComp::Array(t, _)) => {
                #[cfg(feature = "debug-type-gc")]
                {
                    let name = self.get_name(*t)?;
                    println!(
                        "{}(offset_0x{:08x}) array -> 0x{:08x} {name}",
                        "  ".repeat(depth),
                        offset,
                        t
                    );
                }
                self.mark_referenced_internal(*t, depth)
                    .attach_printable_lazy(|| format!("While marking offset: 0x{:08x}", offset))?;
            }
            TypeInfo::Comp(TypeComp::Subroutine(ret_ty, param_ty)) => {
                for t in std::iter::once(*ret_ty)
                    .chain(param_ty.iter().copied())
                    .collect::<Vec<_>>()
                {
                    #[cfg(feature = "debug-type-gc")]
                    {
                        let name = self.get_name(t)?;
                        println!(
                            "{}(offset_0x{:08x}) subroutine -> 0x{:08x} {name}",
                            "  ".repeat(depth),
                            offset,
                            t
                        );
                    }
                    self.mark_referenced_internal(t, depth)
                        .attach_printable_lazy(|| {
                            format!("While marking offset: 0x{:08x}", offset)
                        })?;
                }
            }
            TypeInfo::Comp(TypeComp::Ptmf(this_ty, ret_ty, param_ty)) => {
                for t in std::iter::once(*this_ty)
                    .chain(std::iter::once(*ret_ty))
                    .chain(param_ty.iter().copied())
                    .collect::<Vec<_>>()
                {
                    #[cfg(feature = "debug-type-gc")]
                    {
                        let name = self.get_name(t)?;
                        println!(
                            "{}(offset_0x{:08x}) ptmf -> 0x{:08x} {name}",
                            "  ".repeat(depth),
                            offset,
                            t
                        );
                    }
                    self.mark_referenced_internal(t, depth)
                        .attach_printable_lazy(|| {
                            format!("While marking offset: 0x{:08x}", offset)
                        })?;
                }
            }
        }
        #[cfg(feature = "debug-type-gc")]
        {
            let name = self.get_name(offset)?;
            println!(
                "{}finish marking 0x{:08x} {name}",
                "  ".repeat(_old_depth),
                offset
            );
        }
        Ok(())
    }

    pub fn create_defs(&self) -> Result<BTreeMap<String, TypeDef>, Error> {
        let progress = ProgressPrinter::new(self.referenced.len(), "Create type definitions");
        let mut referenced_names = BTreeMap::<String, BTreeSet<String>>::new();
        let mut name_to_def = BTreeMap::new();
        for (i, bucket) in self.referenced.iter().enumerate() {
            progress.print(i, "");
            let info_def = self.get_info_for_def(*bucket).attach_printable_lazy(|| {
                format!("While creating def for bucket 0x{:08x}", bucket)
            })?;
            if let Some((name, info)) = info_def {
                let key = name.clone();
                let def = match info {
                    TypeInfo::Enum(x) => TypeDef::Enum(EnumDef {
                        name,
                        size: x.size,
                        enumerators: x.enumerators.clone(),
                    }),
                    TypeInfo::Struct(x) => {
                        let members = x
                            .members
                            .into_iter()
                            .map(|m| {
                                let name =
                                    m.name.unwrap_or_else(|| format!("field_{:x}", m.offset));
                                let ty_name = self.get_name(m.ty_offset)?;
                                if let TypeName::Name(x) = &ty_name {
                                    referenced_names
                                        .entry(x.clone())
                                        .or_default()
                                        .insert(key.clone());
                                }
                                let ty_yaml = ty_name.yaml_string();
                                Ok(MemberDef {
                                    offset: m.offset,
                                    name,
                                    is_base: m.is_base,
                                    ty_yaml,
                                })
                            })
                            .collect::<Result<Vec<_>, Error>>()?;
                        TypeDef::Struct(StructDef {
                            name,
                            vtable: x.vtable.clone(),
                            size: x.size,
                            members,
                        })
                    }
                    TypeInfo::Union(x) => {
                        let members = x
                            .members
                            .into_iter()
                            .enumerate()
                            .map(|(i, (name, offset))| {
                                let name = name.unwrap_or_else(|| format!("_{}", i));
                                let ty_name = self.get_name(offset)?;
                                if let TypeName::Name(x) = &ty_name {
                                    referenced_names
                                        .entry(x.clone())
                                        .or_default()
                                        .insert(key.clone());
                                }
                                let ty_yaml = ty_name.yaml_string();
                                Ok((name, ty_yaml))
                            })
                            .collect::<Result<Vec<_>, Error>>()?;
                        TypeDef::Union(UnionDef {
                            name,
                            size: x.size,
                            members,
                        })
                    }
                    TypeInfo::Comp(c) => {
                        if let TypeComp::Ptmf(_, _, _) = c {
                            // PTMF actually doesn't need the original type
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
        for (x, referers) in referenced_names {
            if !name_to_def.contains_key(&x) {
                return Err(report!(Error::BrokenTypeRef(x.clone())))
                    .attach_printable(format!("Referenced by {:?}", referers));
            }
        }
        Ok(name_to_def)
    }

    /// Get the type info for creating def for the bucket
    ///
    /// Return value:
    /// - None - don't create a def for this bucket
    /// - Some(name, info) - create a def with the given name
    fn get_info_for_def(&self, bucket: usize) -> Result<Option<(String, TypeInfo)>, Error> {
        let name = self.get_name(bucket)?;
        let name = match name {
            // don't create definitions for primitive types
            TypeName::Prim(_) => return Ok(None),
            TypeName::Name(n) => n.clone(),
            TypeName::Comp(c) => {
                // need to create definition to PTMFs as structs
                if let TypeComp::Ptmf(this, _, _) = c.as_ref() {
                    if let TypeName::Name(n) = this {
                        format!("{}_ptmf", n)
                    } else {
                        return Err(report!(Error::UnexpectedUnnamedPtmfThis))
                            .attach_printable(format!("PTMF has base type: {}", this))?;
                    }
                } else {
                    // don't create definitions for pointers, arrays, subroutines
                    return Ok(None);
                }
            }
        };
        let mut is_struct = None;
        let mut is_union = None;
        for offset in self.resolver.get_bucket_offsets(bucket)? {
            let ty = self.resolver.get_info(*offset)?.clone();
            match &ty {
                TypeInfo::Comp(TypeComp::Ptmf(_, _, _)) => return Ok(Some((name, ty))),
                TypeInfo::Enum(_) => return Ok(Some((name, ty))),
                TypeInfo::Struct(_) => {
                    if is_struct.is_none() {
                        is_struct = Some(ty)
                    }
                }
                TypeInfo::Union(_) => {
                    if is_union.is_none() {
                        is_union = Some(ty)
                    }
                }
                _ => continue,
            }
        }

        Ok(if let Some(ty) = is_struct {
            Some((name, ty))
        } else if let Some(ty) = is_union {
            Some((name, ty))
        } else {
            None
        })
    }
}
