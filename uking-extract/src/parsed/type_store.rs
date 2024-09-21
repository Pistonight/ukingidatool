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

}
