use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::hash::{DefaultHasher, Hash, Hasher};
use std::sync::{Arc, RwLock};

use itertools::Itertools;

use error_stack::{report, Result, ResultExt};

use super::{Error, NamespaceMap, TypeComp, TypeInfo, TypeName, VtableInfo};

use crate::util::ProgressPrinter;
use crate::worker::Pool;

struct InputIter {
    keys: Vec<usize>,
    a: usize,
    b: usize,
    chunk_size: usize,
}
impl InputIter {
    fn new(keys: Vec<usize>, chunk_size: usize) -> Self {
        Self {
            keys,
            a: 0,
            b: 1,
            chunk_size,
        }
    }
    /// Factor when multiplied by to get the total number of combinations
    fn factor(&self) -> usize {
        match self.keys.len() {
            0 => 1,
            n => (n - 1) / 2,
        }
    }
    fn next_single(&mut self) -> Option<(usize, usize)> {
        let len = self.keys.len();
        self.b += 1;
        if self.b >= len {
            self.a += 1;
            self.b = self.a + 1;
        }
        if self.a >= len || self.b >= len {
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
    offset_to_type: BTreeMap<usize, TypeInfo>,
    offset_to_bucket: BTreeMap<usize, usize>,
    bucket_to_offsets: BTreeMap<usize, Vec<usize>>,
}

impl TypeResolver {
    pub fn new(offset_to_type: BTreeMap<usize, TypeInfo>) -> Self {
        let offset_to_bucket = offset_to_type.keys().map(|k| (*k, *k)).collect();
        let bucket_to_offsets = offset_to_type.keys().map(|k| (*k, vec![*k])).collect();
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
        for (a, b) in typedefs {
            self.merge(a, b);
        }
    }

    pub fn merge_pass1(s: &Arc<RwLock<Self>>) {
        Self::merge_pass_by_filter(s, 8192, 0.1, |_| true, "Deduplicate types pass 1");
    }

    pub fn merge_primitives(s: &Arc<RwLock<Self>>) {
        Self::merge_by_filter(
            s,
            |x| {
                let s = s.read().unwrap();
                let ty = s.offset_to_type.get(&x).unwrap();
                matches!(ty, TypeInfo::Prim(_))
            },
            "Deduplicate primitives",
        );
    }

    pub fn merge_structs(s: &Arc<RwLock<Self>>) {
        Self::merge_by_filter(
            s,
            |x| {
                let s = s.read().unwrap();
                let ty = s.offset_to_type.get(&x).unwrap();
                matches!(ty, TypeInfo::Struct(x))
            },
            "Deduplicate structs",
        );
    }

    pub fn merge_enums(s: &Arc<RwLock<Self>>) {
        Self::merge_by_filter(
            s,
            |x| {
                let s = s.read().unwrap();
                let ty = s.offset_to_type.get(&x).unwrap();
                matches!(ty, TypeInfo::Enum(x))
            },
            "Deduplicate enums",
        );
    }

    pub fn merge_unions(s: &Arc<RwLock<Self>>) {
        Self::merge_by_filter(
            s,
            |x| {
                let s = s.read().unwrap();
                let ty = s.offset_to_type.get(&x).unwrap();
                matches!(ty, TypeInfo::Union(x))
            },
            "Deduplicate unions",
        );
    }

    pub fn merge_pointers(s: &Arc<RwLock<Self>>) {
        Self::merge_by_filter(
            s,
            |x| {
                let s = s.read().unwrap();
                let ty = s.offset_to_type.get(&x).unwrap();
                matches!(ty, TypeInfo::Comp(TypeComp::Ptr(_)))
            },
            "Deduplicate pointer types",
        );
    }

    pub fn merge_pass2(s: &Arc<RwLock<Self>>) {
        Self::merge_by_filter(s, |_| true, "Deduplicate types pass 2");
    }

    fn merge_pass_by_filter(
        s: &Arc<RwLock<Self>>,
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
                let len = filtered.len();
                let window = window.min(len / crate::worker::num_threads());
                for chunk in &filtered.into_iter().chunks(window) {
                    keys_chunked.push(chunk.collect::<Vec<_>>());
                }
                (keys_chunked, len)
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
            progress.set_prefix(format!(
                "{msg} (iteration {}), merging {total} types",
                iteration
            ));
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
                // if window * crate::worker::num_threads() > after {
                //     // can no longer take 100% advantage of CPU
                //     break;
                // }
            }
        }
        progress.done();
    }

    fn merge_by_filter(s: &Arc<RwLock<Self>>, f: impl Fn(usize) -> bool, msg: &str) {
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
            progress.set_total(keys.len());
            let s2 = Arc::clone(s);
            let input = InputIter::new(keys, 81920);
            let factor = input.factor();
            let input = input.map(move |x| (Arc::clone(&s2), x));
            progress.set_prefix(format!("{msg} (iteration {})", iteration));
            progress.reset_timer();
            progress.print(0, "");
            let pool = Pool::run(input, move |(s, keys)| {
                let s = s.read().unwrap();
                let len = keys.len();
                (s.find_mergeable2(&keys), len)
            });
            let mut merge_sets = BTreeMap::new();
            let mut count = 0;
            for (m, l) in pool {
                count += l;
                progress.print(count / factor, "");
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

    fn filtered_buckets(&self, f: impl Fn(usize) -> bool) -> BTreeSet<usize> {
        self.bucket_to_offsets
            .keys()
            .filter(|x| f(**x))
            .copied()
            .collect()
    }

    pub fn resolve_sizes(&self) -> Result<BTreeMap<usize, usize>, Error> {
        let mut bucket_to_sizes = BTreeMap::new();
        loop {
            let last_len = bucket_to_sizes.len();
            'outer: for (bucket, offsets) in &self.bucket_to_offsets {
                // find the first type that has a size
                for offset in offsets {
                    let ty = self.get_info(*offset)?;
                    let size = match ty {
                        TypeInfo::Prim(p) => {
                            if let Some(size) = p.size() {
                                size
                            } else {
                                continue;
                            }
                        }
                        TypeInfo::Typedef(_,_) => continue,
                        TypeInfo::Enum(e) => {
                            if e.is_decl {
                                continue;
                            }
                            e.size
                        }
                        TypeInfo::Struct(s) => {
                            if s.is_decl {
                                continue;
                            }
                            s.size
                        }
                        TypeInfo::Union(u) => {
                            if u.is_decl {
                                continue;
                            }
                            u.size
                        }
                        TypeInfo::Comp(TypeComp::Ptr(_)) => 8,
                        TypeInfo::Comp(TypeComp::Ptmf(_,_,_)) => 16,
                        TypeInfo::Comp(TypeComp::Array(t, len)) => {
                            let bucket = self.get_bucket(*t)?;
                            if let Some(size) = bucket_to_sizes.get(&bucket) {
                                size * len
                            } else {
                                // can't calculate array size yet
                                continue 'outer;
                            }
                        }
                        TypeInfo::Comp(TypeComp::Subroutine(_, _)) => {
                            // subroutine types don't have size,
                            // only pointers do
                            continue;
                        }
                    };
                    bucket_to_sizes.insert(*bucket, size);
                    continue 'outer;
                }
            }
            if last_len == bucket_to_sizes.len() {
                break;
            }
        }
        Ok(bucket_to_sizes)
    }

    pub fn get_bucket(&self, offset: usize) -> Result<usize, Error> {
        self.offset_to_bucket
            .get(&offset)
            .copied()
            .ok_or(Error::UnlinkedType(offset))
            .attach_printable_lazy(|| format!("While getting bucket for offset: 0x{:08x}", offset))
    }

    pub fn get_info(&self, offset: usize) -> Result<&TypeInfo, Error> {
        self.offset_to_type
            .get(&offset)
            .ok_or(Error::UnlinkedType(offset))
            .attach_printable_lazy(|| {
                format!("While getting type info for offset: 0x{:08x}", offset)
            })
    }

    pub fn get_bucket_offsets(&self, bucket: usize) -> Result<&Vec<usize>, Error> {
        self.bucket_to_offsets
            .get(&bucket)
            .ok_or(Error::UnlinkedType(bucket))
            .attach_printable_lazy(|| format!("While getting offsets for bucket: 0x{:08x}", bucket))
    }

    fn find_mergeable(&self, inputs: &Vec<usize>) -> Vec<Vec<usize>> {
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

    fn find_mergeable2(&self, inputs: &Vec<(usize, usize)>) -> BTreeMap<usize, Vec<usize>> {
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

    pub fn complete_vtables(&mut self) -> Result<(), Error> {
        let progress = ProgressPrinter::new(self.bucket_to_offsets.len(), "Merge vtables");

        for (i, offsets) in self.bucket_to_offsets.values().enumerate() {
            progress.print(i, "");
            let mut vtable = VtableInfo::default();
            for offset in offsets {
                let ty = self.get_info(*offset)?;
                if let TypeInfo::Struct(s) = ty {
                    vtable.merge(&s.vtable);
                }
            }
            for offset in offsets {
                // unwrap: should return err above if offset is not in the map
                let ty = self.offset_to_type.get_mut(offset).unwrap();
                if let TypeInfo::Struct(s) = ty {
                    s.vtable = vtable.clone();
                }
            }
        }
        progress.done();
        Ok(())
    }

    fn needs_merging(&self, a_offset: usize, b_offset: usize) -> bool {
        let a_bucket = self.offset_to_bucket.get(&a_offset).unwrap();
        let b_bucket = self.offset_to_bucket.get(&b_offset).unwrap();
        if a_bucket == b_bucket {
            // already same bucket, no need to merge
            return false;
        }
        let mut seen = HashSet::new();
        return self.are_equiv_bucket(*a_bucket, *b_bucket, &mut seen, false);
    }

    fn are_equiv(
        &self,
        a_offset: usize,
        b_offset: usize,
        seen: &mut HashSet<(usize, usize)>,
        allow_decl: bool,
    ) -> bool {
        let a_bucket = self.offset_to_bucket.get(&a_offset).unwrap();
        let b_bucket = self.offset_to_bucket.get(&b_offset).unwrap();
        return self.are_equiv_bucket(*a_bucket, *b_bucket, seen, allow_decl);
    }

    /// Perform high-level merge checks
    ///
    /// Struct and union member types are not checked
    fn are_equiv_bucket(
        &self,
        a_bucket: usize,
        b_bucket: usize,
        seen: &mut HashSet<(usize, usize)>,
        allow_decl: bool
    ) -> bool {
        if a_bucket == b_bucket {
            return true;
        }
        if a_bucket > b_bucket {
            return self.are_equiv_bucket(b_bucket, a_bucket, seen, allow_decl);
        }
        if !seen.insert((a_bucket, b_bucket)) {
            return false;
        }
        let a_ty = self.offset_to_type.get(&a_bucket).unwrap();
        let b_ty = self.offset_to_type.get(&b_bucket).unwrap();
        match (a_ty, b_ty) {
            // note: typedefs are added in make_expr_typedef
            (TypeInfo::Prim(a_prim), TypeInfo::Prim(b_prim)) => {
                return a_prim == b_prim;
            }
            (TypeInfo::Struct(a_struct), TypeInfo::Struct(b_struct)) => {
                if a_struct.is_decl || b_struct.is_decl {
                    // if allow_decl || (a_struct.is_decl && b_struct.is_decl) {
                        return a_struct.name == b_struct.name;
                    // }
                    // return false;
                }
                if a_struct.size != b_struct.size {
                    return false;
                }
                if a_struct.members.len() != b_struct.members.len() {
                    return false;
                }
                match (&a_struct.name, &b_struct.name) {
                    (Some(a_name), Some(b_name)) => {
                        // if both A and B are named, they must be the same name to be considered
                        // equivalent, otherwise non-related types with the same layout will be
                        // merged
                        if a_name != b_name {
                            return false;
                        }
                    }
                    (None, None) => {
                        // if both doesn't have name, they are equivalent
                    }
                    (Some(_), None) | (None, Some(_))
                     => {
                        // if one of them is named, they can only be merged
                        // if there are members
                        // Otherwise, consider 2 empty structs A and B.
                        // They are both equivalent to an anonymous struct with no members.
                        // However, A and B could be totally unrelated and not decompiled yet
                        if a_struct.members.is_empty() || a_struct.size == 0 {
                            return false;
                        }
                    }
                }
                if !a_struct.vtable.is_equiv_to(&b_struct.vtable) {
                    return false;
                }
                for (a_member, b_member) in a_struct.members.iter().zip(b_struct.members.iter()) {
                    match (&a_member.name, &b_member.name) {
                        (Some(a_name), Some(b_name)) => {
                            if a_name != b_name {
                                return false;
                            }
                        }
                        _ => (),
                    }
                    if a_member.is_base != b_member.is_base {
                        return false;
                    }
                    if a_member.offset != b_member.offset {
                        return false;
                    }
                    // types must be checked as well
                    if !self.are_equiv(a_member.ty_offset, b_member.ty_offset, seen, false) {
                        return false;
                    }
                }
                return true;
            }
            (TypeInfo::Union(a_union), TypeInfo::Union(b_union)) => {
                if a_union.is_decl || b_union.is_decl {
                    // if allow_decl || (a_union.is_decl && b_union.is_decl) {
                        return a_union.name == b_union.name;
                    // }
                    // return false;
                }
                // both union name and union member names don't matter
                if a_union.members.len() != b_union.members.len() {
                    return false;
                }
                // see above for the explanation
                match (&a_union.name, &b_union.name) {
                    (Some(a_name), Some(b_name)) => {
                        if a_name != b_name {
                            return false;
                        }
                    }
                    (None, None) => { }
                    (Some(_), None) | (None, Some(_)) => {
                        if a_union.members.is_empty() || a_union.size == 0 {
                            return false;
                        }
                    }
                }
                // only consider the members in order for now
                for (a_member, b_member) in a_union.members.iter().zip(b_union.members.iter()) {
                    if a_member.0 != b_member.0 {
                        return false;
                    }
                    if !self.are_equiv(a_member.1, b_member.1, seen, false) {
                        return false;
                    }
                }
                return true;
            }
            (TypeInfo::Enum(a_enum), TypeInfo::Enum(b_enum)) => {
                if a_enum.is_decl || b_enum.is_decl {
                    // if allow_decl || (a_enum.is_decl && b_enum.is_decl){
                        return a_enum.name == b_enum.name;
                    // }
                    // return false;
                }
                // names matters if they don't have any enumerators
                if a_enum.enumerators.len() == 0 || b_enum.enumerators.len() == 0 {
                    match (&a_enum.name, &b_enum.name) {
                        (Some(a_name), Some(b_name)) => {
                            if a_name != b_name {
                                return false;
                            }
                        }
                        (None, None) => { }
                        _ => return false
                    }
                }

                // size is the byte size, so they must match
                if a_enum.size != b_enum.size {
                    return false;
                }
                if a_enum.enumerators.len() != b_enum.enumerators.len() {
                    // number of enumerators must match
                    return false;
                }
                for (a_enumerator, b_enumerator) in
                    a_enum.enumerators.iter().zip(b_enum.enumerators.iter())
                {
                    // names matter for the enumerators, otherwise unrelated enums with the
                    // same values will be merged
                    if a_enumerator != b_enumerator {
                        return false;
                    }
                }
                return true;
            }
            (TypeInfo::Comp(TypeComp::Ptr(a_ptr)), TypeInfo::Comp(TypeComp::Ptr(b_ptr))) => {
                return self.are_equiv(*a_ptr, *b_ptr, seen, true)
            }
            (
                TypeInfo::Comp(TypeComp::Array(a_t, a_count)),
                TypeInfo::Comp(TypeComp::Array(b_t, b_count)),
            ) => {
                if a_count != b_count {
                    return false;
                }
                return self.are_equiv(*a_t, *b_t, seen, false);
            }
            (
                TypeInfo::Comp(TypeComp::Subroutine(a_ret, a_param)),
                TypeInfo::Comp(TypeComp::Subroutine(b_ret, b_param)),
            ) => {
                if a_param.len() != b_param.len() {
                    return false;
                }
                if !self.are_equiv(*a_ret, *b_ret, seen, false) {
                    return false;
                }
                for (a_p, b_p) in a_param.iter().zip(b_param.iter()) {
                    if !self.are_equiv(*a_p, *b_p, seen, false) {
                        return false;
                    }
                }
                return true;
            }
            (
                TypeInfo::Comp(TypeComp::Ptmf(a_this, a_ret, a_param)),
                TypeInfo::Comp(TypeComp::Ptmf(b_this, b_ret, b_param)),
            ) => {
                if a_param.len() != b_param.len() {
                    return false;
                }
                // ptmf are similar to subroutine, with an additional this type
                if !self.are_equiv(*a_this, *b_this, seen, false) {
                    return false;
                }
                if !self.are_equiv(*a_ret, *b_ret, seen, false) {
                    return false;
                }

                for (a_p, b_p) in a_param.iter().zip(b_param.iter()) {
                    if !self.are_equiv_bucket(*a_p, *b_p, seen, false) {
                        return false;
                    }
                }
                return true;
            }
            _ => return false,
        };
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
        self.bucket_to_offsets
            .get_mut(&a_bucket)
            .unwrap()
            .extend(b_offsets);
    }
}


/// hash members for anonymous types so they don't conflict
fn hash_members<'a>(members: impl Iterator<Item = &'a str>) -> u64{
    let mut string_for_hash = String::new();
    for m in members {
        string_for_hash.push_str(&format!("{}{}", m.len(), m));
    }
    let mut hasher = DefaultHasher::new();
    string_for_hash.hash(&mut hasher);
    hasher.finish()
}