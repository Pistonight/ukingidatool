use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::sync::{Arc, RwLock};

use error_stack::{report, Result, ResultExt};

use super::{
    EnumDef, Error, MemberDef, NamespaceMap, StructDef, StructInfo, TypeComp, TypeDef, TypeInfo, TypeName, TypePrim, TypeResolver, TypeYaml, UnionDef
};
use crate::util::ProgressPrinter;

pub struct TypeStore {
    resolver: TypeResolver,
    names: BTreeMap<usize, TypeName>,
    sizes: BTreeMap<usize, usize>,
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
        let sizes = resolver.resolve_sizes()?;

        Ok(Self {
            resolver,
            names,
            sizes,
            referenced: HashSet::new(),
        })
    }
}

impl TypeStore {
    pub fn are_equal(&self, a: usize, b: usize) -> Result<bool, Error> {
        let ty_a = self.resolver.get_info(a)?;
        let ty_b = self.resolver.get_info(b)?;
        match (ty_a.is_decl(), ty_b.is_decl()) {
            (true, true) => {
                // both are declarations, compare the declaration name
                return Ok(ty_a.declaration_name() == ty_b.declaration_name())
            }
            (true, false) => {
                // does b bucket have any name equals to A
                let offsets = self.resolver.get_bucket_offsets(b)?;
                for b in offsets {
                    let ty_b = self.resolver.get_info(*b)?;
                    if ty_b.declaration_name() == ty_a.declaration_name() {
                        return Ok(true);
                    }
                }
                return Ok(false)
            }
            (false, true) => {
                // does a bucket have any name equals to B
                let offsets = self.resolver.get_bucket_offsets(a)?;
                for a in offsets {
                    let ty_a = self.resolver.get_info(*a)?;
                    if ty_a.declaration_name() == ty_b.declaration_name() {
                        return Ok(true);
                    }
                }
                return Ok(false)
            }
            (false, false) => { }
        }
        // both are definitions, compare the bucket
        let a = self.resolver.get_bucket(a)?;
        let b = self.resolver.get_bucket(b)?;
        if a ==b {
            return Ok(true);
        }
        // composition
        match (ty_a, ty_b) {
            (TypeInfo::Comp(TypeComp::Ptr(a)), TypeInfo::Comp(TypeComp::Ptr(b))) => {
                // both are pointers, compare the target type
                Ok(self.are_equal(*a, *b)?)
            }
            (TypeInfo::Comp(TypeComp::Array(a, l1)), TypeInfo::Comp(TypeComp::Array(b, l2))) => {
                // both are arrays, compare the element type
                Ok(l1 == l2 && self.are_equal(*a, *b)?)
            }
            (TypeInfo::Comp(TypeComp::Subroutine(a_ret, a_param)), TypeInfo::Comp(TypeComp::Subroutine(b_ret, b_param))) => {
                // both are subroutines, compare the return type and parameters
                if !self.are_equal(*a_ret, *b_ret)? {
                    return Ok(false);
                }
                if a_param.len() != b_param.len() {
                    return Ok(false);
                }
                for (a, b) in a_param.iter().zip(b_param.iter()) {
                    if !self.are_equal(*a, *b)? {
                        return Ok(false);
                    }
                }
                return Ok(true);
            }
            (TypeInfo::Comp(TypeComp::Ptmf(a_this, a_ret, a_param)), TypeInfo::Comp(TypeComp::Ptmf(b_this, b_ret, b_param))) => {
                if !self.are_equal(*a_this, *b_this)? {
                    return Ok(false);
                }
                if !self.are_equal(*a_ret, *b_ret)? {
                    return Ok(false);
                }
                if a_param.len() != b_param.len() {
                    return Ok(false);
                }
                for (a, b) in a_param.iter().zip(b_param.iter()) {
                    if !self.are_equal(*a, *b)? {
                        return Ok(false);
                    }
                }
                return Ok(true);
            }
            _ => Ok(false),
        }
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

    /// Get the size of the type at offset. Return an error if the type doesn't have a size (like
    /// void of a function type
    pub fn get_size(&self, offset: &usize) -> Result<usize, Error> {
        let bucket = self
            .get_bucket(*offset)
            .attach_printable_lazy(|| format!("While getting size for offset: 0x{:08x}", offset))?;

        let size = self
            .sizes
            .get(&bucket)
            .ok_or(Error::UnresolvedSize)
            .attach_printable_lazy(|| {
                format!(
                    "While getting size for offset: 0x{:08x} in bucket 0x{:08x}",
                    offset, bucket
                )
            })?;
        Ok(*size)
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
        // // recursively mark
        // for offset in self
        //     .resolver
        //     .get_bucket_offsets(bucket)?
        //     .iter()
        //     .copied()
        //     .collect::<Vec<_>>()
        // {
            // self.do_mark_offset(bucket, offset, depth)
            //     .attach_printable_lazy(|| format!("While marking offset: 0x{:08x}", offset))?;
        // }
            self.do_mark_offset(bucket, bucket, depth)
                .attach_printable_lazy(|| format!("While marking offset: 0x{:08x}", offset))?;
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
                let refed = 
                s.members.iter().map(|m| m.ty_offset).chain(
                s.vtable
                    .iter().flat_map(|x| std::iter::once(x.retty_offset).chain(x.argty_offsets.iter().copied()))
                ).collect::<Vec<_>>();
                for t in refed {
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
        let mut referenced_names = NameRefMap::default();
        let mut name_to_def = BTreeMap::new();
        for (i, bucket) in self.referenced.iter().enumerate() {
            progress.print(i, "");
            self.create_def_for_bucket(*bucket, &mut name_to_def, &mut referenced_names)
                .attach_printable(format!("While creating def for bucket 0x{:08x}", bucket))?;
        }
        progress.done();
        for (x, referers) in referenced_names.0 {
            if !name_to_def.contains_key(&x) {
                return Err(report!(Error::BrokenTypeRef(x.clone())))
                    .attach_printable(format!("Referenced by {:?}", referers));
            }
        }
        Ok(name_to_def)
    }

    fn create_def_for_bucket<'a>(&self, bucket: usize, name_to_def: &'a mut BTreeMap<String, TypeDef>, referenced_names: &mut NameRefMap) -> Result<Option<&'a TypeDef>, Error> {
        let info_def = self.get_info_for_def(bucket).attach_printable_lazy(|| {
            format!("While creating def for bucket 0x{:08x}", bucket)
        })?;
        let (name, info) = match info_def {
            Some(x) => x,
            None => return Ok(None),
        };
        let key = name.clone();
        let def = match info {
            TypeInfo::Enum(x) => {
                if x.is_decl {
                    return Ok(None);
                }
                TypeDef::Enum(EnumDef {
                    name,
                    size: x.size,
                    enumerators: x.enumerators.clone(),
                })
            }
            TypeInfo::Struct(x) => {
                if x.is_decl {
                    return Ok(None);
                }
                TypeDef::Struct(self.create_struct_def(x, &name, name_to_def, referenced_names)?)
            }
            TypeInfo::Union(x) => {
                if x.is_decl {
                    return Ok(None);
                }
                let mut members: Vec<(String, String)> = Vec::with_capacity(x.members.len());
                for (i, (name, offset)) in x.members.into_iter().enumerate() {
                    let mut name = name.unwrap_or_else(|| format!("_{}", i));
                    while members.iter().any(|x| x.0 == name) {
                        name.push('_');
                    }
                    let ty_name = self.get_name(offset)?;
                    referenced_names.add(&key, &ty_name);
                    let ty_yaml = ty_name.yaml_string();
                    members.push((name, ty_yaml))
                }
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
        if let Some(old) = name_to_def.get(&key) {
            if old != &def {
                return Err(report!(Error::ConflictingType))
                    .attach_printable(format!("Old: {:?}", old))
                    .attach_printable(format!("New: {:?}", def));
            }
        } else {
            name_to_def.insert(key.clone(), def);
        }
        return Ok(name_to_def.get(&key));
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

    /// Create a struct layout after tail padding and empty base optimization,
    /// and ensure member name uniqueness
    fn create_struct_def(&self, info: StructInfo, name: &str, name_to_def: &mut BTreeMap<String, TypeDef>, referenced_names: &mut NameRefMap) -> Result<StructDef, Error> {
        let mut member_sizes: Vec<usize> = Vec::with_capacity(info.members.len());
        for i in 0..info.members.len()  {
            let offset = info.members[i].offset;
            let next_offset = if i < info.members.len() - 1 {
                info.members[i + 1].offset
            } else {
                info.size
            };
            member_sizes.push(next_offset - offset);
        }

        let mut members: Vec<MemberDef> = Vec::new();
        for (m, m_size) in info.members.into_iter().zip(member_sizes.into_iter()) {
            let mut m_name = m.name.unwrap_or_else(|| format!("field_{:x}", m.offset));
            while members.iter().any(|x| x.name == m_name) {
                m_name.push('_');
            }

            if m.is_base {
                if m_size == 0 {
                    #[cfg(feature = "debug-layout")]
                    {
                        println!("struct {}, base member `{}`, empty base", name, m_name);
                    }
                    // empty base optimization - base is removed
                    continue;
                }
                let base_size = self.get_size(&m.ty_offset)?;
                if base_size > m_size {
                    #[cfg(feature = "debug-layout")]
                    {
                        println!("struct {}, base member `{}`, tail-padding", name, m_name);
                    }
                    // tail padding optimization
                    // expand the base members into the struct
                    let bucket = self.get_bucket(m.ty_offset)?;
                    let base_def = self.create_def_for_bucket(
                        bucket,
                        name_to_def, 
                        referenced_names)?
                        .ok_or(report!(Error::InvalidLayout))
                        .attach_printable(format!("While resolving tail padding optimization for base member, cannot find base definition for member `{m_name}"))?;
                    let base_def = match base_def {
                        TypeDef::Struct(x) => x,
                        _ => {
                            let r = report!(Error::InvalidLayout)
                            .attach_printable(format!("Base Member `{m_name}` is not a struct. Currently unions are not supported as base type"));
                            return Err(r);
                        }
                    };
                    for base_member in &base_def.members {
                        let mut base_m_name = format!("{}_{}", m_name, base_member.name);
                        while members.iter().any(|x| x.name == base_m_name) {
                            base_m_name.push('_');
                        }
                        let m = MemberDef {
                            offset: m.offset + base_member.offset,
                            name: base_m_name,
                            is_base: true,
                            ty_yaml: base_member.ty_yaml.clone(),
                        };
                        members.push(m);
                    }
                    continue;
                }
            } else {
                if m_size == 0 {
                    let r = report!(Error::InvalidLayout)
                    .attach_printable(format!("Non-base Member `{}` has zero size in the struct layout", m_name));
                    return Err(r);
                }
            }



            let ty_name = self.get_name(m.ty_offset)?;
            referenced_names.add(&name, &ty_name);
            let ty_yaml = ty_name.yaml_string();
            let m = MemberDef {
                offset: m.offset,
                name: m_name,
                is_base: m.is_base,
                ty_yaml,
            };
            members.push(m);
        }
        let mut vtable: Vec<(String, String)> = Vec::with_capacity(info.vtable.len());
        for vfptr in info.vtable.into_iter() {
            let mut v_name = vfptr.name;
            while vtable.iter().any(|x| x.0 == v_name) {
                v_name.push('_');
            }
            let retty_name = self.get_name(vfptr.retty_offset)?;
            let argty_names = vfptr.argty_offsets
                .iter()
                .map(|&offset| {
                    self.get_name(offset)
                })
                .collect::<Result<Vec<_>, Error>>()?;
            let ty_name = TypeName::Comp(Box::new(TypeComp::Subroutine(
                retty_name,
                argty_names,
            )));
            let ty_name = TypeName::pointer(ty_name);
            referenced_names.add(&name, &ty_name);

            vtable.push((v_name, ty_name.yaml_string()))
        }

        // let mut name = name.to_string();
        // if name.includes("anonymous_struct") {
        //    
        // }

        let def = StructDef {
            name: name.to_string(),
            vtable,
            size: info.size,
            members,
        };

        Ok(def)
    }
}

#[derive(Default)]
struct NameRefMap(BTreeMap<String, BTreeSet<String>>);
impl NameRefMap {
    fn add(&mut self, name: &str, refed: &TypeName) {
        let e = self.0.entry(name.to_string()).or_default();
        for r in refed.referenced_names() {
            e.insert(r);
        }
    }
}
