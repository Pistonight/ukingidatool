use std::{collections::{BTreeMap, BTreeSet, HashSet}, hash::{DefaultHasher, Hash, Hasher}, sync::{Arc, RwLock}};

use error_stack::{report, Result, ResultExt};

use crate::{util::ProgressPrinter, worker::Pool};

use super::{Error, NamespaceMap, Offset, TypeComp, TypeInfo, TypeName};
struct InputIter {
    keys: Vec<Offset>,
    a: usize,
    b: usize,
    chunk_size: usize,
}
impl InputIter {
    fn new(keys: Vec<Offset>, chunk_size: usize) -> Self {
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
    fn next_single(&mut self) -> Option<(Offset, Offset)> {
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
    type Item = Vec<(Offset, Offset)>;
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

#[derive(Debug, thiserror::Error)]
pub enum NameError {
    #[error("No base name available")]
    NoBaseName,
    #[error("There are unresolved names")]
    UnresolvedNames,
    #[error("There are duplicated names")]
    DuplicatedNames,
}

pub struct TypeResolver2 {
    off2info: BTreeMap<Offset, TypeInfo>,
    off2bkt: BTreeMap<Offset, Offset>,
    buckets: BTreeMap<Offset, Bucket>,
}

impl TypeResolver2 {
    pub fn new(off2info: BTreeMap<Offset, TypeInfo>,
        off2bkt: BTreeMap<Offset, Offset>,
        buckets: BTreeMap<Offset, Vec<Offset>>) -> Self {
        let buckets = buckets.into_iter()
            .map(|(k, v)| (k, Bucket::new(v)))
            .collect();
        Self {
            off2info,
            off2bkt,
            buckets,
        }
    }

    pub fn merge_buckets(s: &Arc<RwLock<Self>>, namespaces: &NamespaceMap) {
        Self::merge_buckets_by_filter("Merge primitive types", s, namespaces, |_, v| {
            v.type_ == BucketType::Prim
        });
        Self::merge_buckets_by_filter("Merge enums", s, namespaces, |_, v| {
            v.type_ == BucketType::Enum
        });
        Self::merge_buckets_by_filter("Merge structs", s, namespaces,|_, v| {
            v.type_ == BucketType::Struct
        });
        Self::merge_buckets_by_filter("Merge unions", s, namespaces, |_, v| {
            v.type_ == BucketType::Union
        });
        Self::merge_buckets_by_filter("Merge compound types", s, namespaces,|_, v| {
            v.type_ == BucketType::Comp
        });
        Self::merge_buckets_by_filter("Merge all types", s, namespaces,|_, _| true);
        {
            let s = s.read().unwrap();
            let count = s.buckets.len();
            println!("Total buckets: {}", count);
        }
    }

    fn merge_buckets_by_filter(tag: &str, s: &Arc<RwLock<Self>>, 
        namespaces: &NamespaceMap,
        f: impl Fn(&Offset, &Bucket) -> bool) {
        let mut last_key_len = 0;
        let mut progress = ProgressPrinter::new(0, tag);
        for i in 1.. {
            {
                let mut s = s.write().unwrap();
                s.reduce_and_check_buckets(namespaces);
            }
            let keys = {
                let s = s.read().unwrap();
                s.buckets.iter().filter_map(|(k, v)| {
                    if f(k, v) {
                        Some(*k)
                    } else {
                        None
                    }
                }).collect::<Vec<_>>()
            };
            if keys.len() == last_key_len {
                break;
            }
            last_key_len = keys.len();
            progress.set_total(keys.len());
            let s2 = Arc::clone(s);
            let input = InputIter::new(keys, 81920);
            let factor = input.factor();
            let input = input.map(move |x| (Arc::clone(&s2), x));
            progress.set_prefix(format!("{tag} (iteration {})", i));
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
                progress.set_prefix(format!("{tag} (iteration {}): merging", i));
                progress.set_total(merge_sets.len());
                progress.reset_timer();
                progress.print(0, "");
                let mut s = s.write().unwrap();
                for (i, (k, v)) in merge_sets.into_iter().enumerate() {
                    progress.print(i, "");
                    let v = v.into_iter().collect::<Vec<_>>();
                    s.merge_all(&k, &v);
                }
            }
        }
        progress.set_total(last_key_len);
        progress.set_prefix(tag);
        progress.done();
    }

    fn find_mergeable2(&self, inputs: &Vec<(Offset, Offset)>) -> BTreeMap<Offset, Vec<Offset>> {
        let mut merge_sets = BTreeMap::<Offset, Vec<Offset>>::new();
        for (a, b) in inputs {
            let set = merge_sets.entry(*a).or_default();
            if self.can_merge_bucket(a, b) {
                set.push(*b);
            }
        }
        merge_sets
    }

    fn reduce_and_check_buckets(&mut self, namespaces: &NamespaceMap) {
        for (bucket_key, bucket) in &mut self.buckets {
            bucket.reduce(&self.off2info, namespaces);
            bucket.check(bucket_key, &self.off2info);
        }
    }

    fn can_merge_bucket(&self, bkt_a: &Offset, bkt_b: &Offset) -> bool {
        self.can_merge_bucket_internal(bkt_a, bkt_b, &mut Vec::new())
    }
    fn can_merge_bucket_internal(&self, bkt_a: &Offset, bkt_b: &Offset, seen: &mut Vec<(Offset, Offset)>) -> bool {
        let bucket_a = self.buckets.get(bkt_a).unwrap();
        let bucket_b = self.buckets.get(bkt_b).unwrap();
        match (&bucket_a.type_, &bucket_b.type_) {
            (BucketType::Prim, BucketType::Prim) => {
                let a = bucket_a.candidates[0];
                let a = self.off2info.get(&a).unwrap();
                let b = bucket_b.candidates[0];
                let b = self.off2info.get(&b).unwrap();
                match (a, b) {
                    (TypeInfo::Prim(a), TypeInfo::Prim(b)) => a == b,
                    _ => false
                }
            }
            (BucketType::Enum, BucketType::Enum) => {
                for a in &bucket_a.candidates {
                    let info_a = self.off2info.get(a).unwrap();
                    for b in &bucket_b.candidates {
                        let info_b = self.off2info.get(b).unwrap();
                        match (info_a, info_b) {
                            (TypeInfo::Enum(a), TypeInfo::Enum(b)) => {
                                if a.is_decl || b.is_decl {
                                    match (&a.name, &b.name) {
                                        (Some(a), Some(b)) => {
                                            if a == b {
                                                return true;
                                            }
                                        }
                                        _ => continue,
                                    }
                                }
                                if a.name != b.name {
                                    continue;
                                }
                                if a.size != b.size {
                                    continue;
                                }
                                if a.enumerators.len() != b.enumerators.len() {
                                    continue;
                                }
                                if a.enumerators.len() == 0 {
                                    if a.name != b.name {
                                        continue;
                                    }
                                }
                                return true;
                            }
                            _ => continue,
                        }
                    }
                }
                return false;
            }
            (BucketType::Struct, BucketType::Struct) => {
                for a in &bucket_a.candidates {
                    let info_a = self.off2info.get(a).unwrap();
                    for b in &bucket_b.candidates {
                        let info_b = self.off2info.get(b).unwrap();
                        match (info_a, info_b) {
                            (TypeInfo::Struct(a), TypeInfo::Struct(b)) => {
                                if a.is_decl || b.is_decl {
                                    match (&a.name, &b.name) {
                                        (Some(a), Some(b)) => {
                                            if a == b {
                                                return true;
                                            }
                                        }
                                        _ => continue,
                                    }
                                }
                                if a.name!=b.name {
                                    continue;
                                }
                                if a.size!=b.size {
                                    continue;
                                }
                                if a.members.len()!=b.members.len() {
                                    continue;
                                }
                                if a.members.is_empty() {
                                    if a.name != b.name {
                                        continue;
                                    }
                                }
                                if !a.vtable.is_equiv_to(&b.vtable) {
                                    continue;
                                }
                for (m_a, m_b) in a.members.iter().zip(b.members.iter()) {
                    if m_a.name != m_b.name {
                                        continue;
                    }
                    if m_a.is_base != m_b.is_base {
                                        continue;
                    }
                    if m_a.offset != m_b.offset {
                                        continue;
                    }
                }
                                let already_recursing = seen
                                    .iter()
                                    .any(|(a, b)| (*a == *bkt_a && *b == *bkt_b) || (*a == *bkt_b && *b == *bkt_a));
                                if !already_recursing {
                                    seen.push((*bkt_a, *bkt_b));
                                    let mut r = true;
                                    for (m_a, m_b) in a.members.iter().zip(b.members.iter()) {
                                        let bkt_a = self.off2bkt.get(&m_a.ty_offset).unwrap();
                                        let bkt_b = self.off2bkt.get(&m_b.ty_offset).unwrap();
                                        if !self.can_merge_bucket_internal(bkt_a, bkt_b, seen) {
                                            r = false;
                                            break;
                                        }
                                    }
                                    seen.pop();
                                    if !r {
                                        continue;
                                    }
                                }
                                return true;
                            }
                            _ => continue,
                        }
                    }
                }
                return false;
            }
            (BucketType::Union, BucketType::Union) => {
                // let mut is_a_all_decl = true;
                // let mut has_name_match = false;
                for a in &bucket_a.candidates {
                    let info_a = self.off2info.get(a).unwrap();
                    for b in &bucket_b.candidates {
                        let info_b = self.off2info.get(b).unwrap();
                        match (info_a, info_b) {
                            (TypeInfo::Union(a), TypeInfo::Union(b)) => {
                                if a.is_decl || b.is_decl {
                                    match (&a.name, &b.name) {
                                        (Some(a), Some(b)) => {
                                            if a == b {
                                                return true;
                                            }
                                        }
                                        _ => continue,
                                    }
                                }
                                if a.name!=b.name {
                                    continue;
                                }
                                if a.size!=b.size {
                                    continue;
                                }
                                if a.members.len()!=b.members.len() {
                                    continue;
                                }
                                if a.members.is_empty() {
                                    if a.name != b.name {
                                        continue;
                                    }
                                }
                                for (m_a, m_b) in a.members.iter().zip(b.members.iter()) {
                                    if m_a.0 != m_b.0 {
                                        continue;
                                    }
                                }
                                let already_recursing = seen
                                    .iter()
                                    .any(|(a, b)| (*a == *bkt_a && *b == *bkt_b) || (*a == *bkt_b && *b == *bkt_a));
                                if !already_recursing {
                                    seen.push((*bkt_a, *bkt_b));
                                    let mut r = true;
                                    for (m_a, m_b) in a.members.iter().zip(b.members.iter()) {
                                        let bkt_a = self.off2bkt.get(&m_a.1).unwrap();
                                        let bkt_b = self.off2bkt.get(&m_b.1).unwrap();
                                        if !self.can_merge_bucket_internal(bkt_a, bkt_b, seen) {
                                            r = false;
                                            break;
                                        }
                                    }
                                    seen.pop();
                                    if !r {
                                        continue;
                                    }
                                }
                                return true;
                            }
                            _ => continue,
                        }
                    }
                }
                return false;
            }
            (BucketType::Comp, BucketType::Comp) => {
                for a in &bucket_a.candidates {
                    let info_a = self.off2info.get(a).unwrap();
                    for b in &bucket_b.candidates {
                        let info_b = self.off2info.get(b).unwrap();
                        match (info_a, info_b) {
                            (TypeInfo::Comp(TypeComp::Ptr(a)), TypeInfo::Comp(TypeComp::Ptr(b))) => {
                                seen.push((*bkt_a, *bkt_b));
                                let bkt_a = self.off2bkt.get(a).unwrap();
                                let bkt_b = self.off2bkt.get(b).unwrap();
                                let r = self.can_merge_bucket_internal(bkt_a, bkt_b, seen);
                                seen.pop();
                                if r {
                                    return true;
                                }
                            }
                            (TypeInfo::Comp(TypeComp::Array(a, len_a)), TypeInfo::Comp(TypeComp::Array(b, len_b))) => {
                                if len_a != len_b {
                                    continue;
                                }
                                seen.push((*bkt_a, *bkt_b));
                                let bkt_a = self.off2bkt.get(a).unwrap();
                                let bkt_b = self.off2bkt.get(b).unwrap();
                                let r = self.can_merge_bucket_internal(bkt_a, bkt_b, seen);
                                seen.pop();
                                if r {
                                    return true;
                                }
                            }
                            (TypeInfo::Comp(TypeComp::Subroutine(a)), TypeInfo::Comp(TypeComp::Subroutine(b))) => {
                                let a = a.iter().copied().collect::<Vec<_>>();
                                let b = b.iter().copied().collect::<Vec<_>>();
                                if a.len() != b.len() {
                                    continue;
                                }
                                seen.push((*bkt_a, *bkt_b));
                                let mut r = true;
                                for (a, b) in a.iter().zip(b.iter()) {
                                    let bkt_a = self.off2bkt.get(a).unwrap();
                                    let bkt_b = self.off2bkt.get(b).unwrap();
                                    if !self.can_merge_bucket_internal(bkt_a, bkt_b, seen) {
                                        r = false;
                                        break;
                                    }
                                }
                                seen.pop();
                                if r {
                                    return true;
                                }
                            }
                            (TypeInfo::Comp(TypeComp::Ptmf(this_a, a)), TypeInfo::Comp(TypeComp::Ptmf(this_b, b))) => {
                                seen.push((*bkt_a, *bkt_b));
                                let bkt_a = self.off2bkt.get(this_a).unwrap();
                                let bkt_b = self.off2bkt.get(this_b).unwrap();
                                let r = self.can_merge_bucket_internal(bkt_a, bkt_b, seen);
                                if !r {
                                    seen.pop();
                                    continue;
                                }
                                let a = a.iter().copied().collect::<Vec<_>>();
                                let b = b.iter().copied().collect::<Vec<_>>();
                                if a.len() != b.len() {
                                    seen.pop();
                                    continue;
                                }
                                let mut r = true;
                                for (a, b) in a.iter().zip(b.iter()) {
                                    let bkt_a = self.off2bkt.get(a).unwrap();
                                    let bkt_b = self.off2bkt.get(b).unwrap();
                                    if !self.can_merge_bucket_internal(bkt_a, bkt_b, seen) {
                                        r = false;
                                        break;
                                    }
                                }
                                seen.pop();
                                if r {
                                    return true;
                                }
                            }
                            _ => continue,
                        }
                    }
                }
                return false;
            }
            _ => return false
        }
    }

    /// Merge the all buckets containing b into the bucket containing a
    fn merge_all(&mut self, off_a: &Offset, off_b: &[Offset]) {
        let bkt_a = *self.off2bkt.get(off_a).unwrap();
        let mut to_change = Vec::new();
        for b in off_b {
            let bkt_b = self.off2bkt.get(b).unwrap();
            if *bkt_b == bkt_a {
                // shouldn't happen, just safe guard
                continue;
            }
            if let Some(bucket_b) = self.buckets.remove(&bkt_b) {
                to_change.extend(bucket_b.original_candidates);
            }
        }
        for off_b in &to_change {
            self.off2bkt.insert(*off_b, bkt_a);
        }
        let bkt_a = self.buckets
            .get_mut(&bkt_a)
            .unwrap();
        bkt_a
            .candidates.extend(to_change.clone());
        bkt_a.original_candidates.extend(to_change);
    }

    pub fn resolve_names( &mut self,) -> Result<BTreeMap<Offset, TypeName>, NameError> {
        let mut base_names = self
            .buckets
            .iter()
            .filter_map(|(bkt, bucket)| {
                let mut names = match bucket.type_ {
                    BucketType::Comp => return None,
                    _ => bucket.names.iter().cloned().collect::<Vec<_>>(),
                };
                names.sort();
                Some((*bkt, names))
            })
            .collect::<BTreeMap<_, _>>();
        #[cfg(feature = "debug-resolve-name")]
        {
            println!("Initial names:");
            for (bkt, names) in &base_names {
                println!("{}:", bkt);
                for name in names {
                    println!("  {}", name);
                }
            }
        }
        loop {
            let mut seen_names = HashSet::new();
            let mut duplicate_names = HashSet::new();
            for (bkt, names) in &base_names {
                let best_name = match names.last() {
                    Some(name) => name.clone(),
                    None => {
                        return Err(report!(NameError::NoBaseName))
                            .attach_printable_lazy(|| format!("For bucket {}", bkt));
                    }
                };
                if !seen_names.insert(best_name.clone()) {
                    duplicate_names.insert(best_name);
                }
            }
            if duplicate_names.is_empty() {
                break;
            }
            #[cfg(feature = "debug-resolve-name")]
            {
                println!(">>>>>>>>>>>>>>>");
                println!("Removing Duplicate names:");
                for name in &duplicate_names {
                    println!("  {}", name);
                }
            }
            for (bkt, names) in &mut base_names {
                let best_name = match names.last() {
                    Some(name) => name,
                    None => {
                        return Err(report!(NameError::NoBaseName))
                            .attach_printable_lazy(|| format!("For bucket {}", bkt));
                    }
                };
                if duplicate_names.contains(best_name) {
                    names.pop();
                }
            }
        }
        let mut bkt2name = base_names
            .into_iter()
            .map(|(bkt, mut names)| {
                let name = names.pop().unwrap();
                (bkt, name)
            })
            .collect::<BTreeMap<_, _>>();
        #[cfg(feature = "debug-resolve-name")]
        {
            println!("Resolved base names:");
            for (bkt, name) in &bkt2name {
                println!("{}: {}", bkt, name);
            }
        }
        loop {
            let mut to_insert = Vec::new();
            for (bkt, bucket) in self.buckets.iter_mut().filter(|(k, _)| !bkt2name.contains_key(k)) {
                bucket.reduce_comp_name(&self.off2info, &self.off2bkt, &bkt2name);
                if !bucket.names.is_empty() {
                    let mut names = bucket.names.iter().cloned().collect::<Vec<_>>();
                    names.sort();
                    let best_name = names.pop().unwrap();
                    to_insert.push((*bkt, best_name));
                }
            }
            if to_insert.is_empty() {
                break;
            }
            for (bkt, name) in to_insert {
                bkt2name.insert(bkt, name);
            }
        }
        if bkt2name.len() != self.buckets.len() {
            #[cfg(feature = "debug-resolve-name")]
            {
                println!("Unresolved names:");
                for (bkt, bucket) in &self.buckets {
                    if !bkt2name.contains_key(bkt) {
                        println!("{}: {:?}", bkt, bucket.names);
                    }
                }
            }
            return Err(report!(NameError::UnresolvedNames));
        }
        let seen_names = bkt2name.values().cloned().collect::<HashSet<_>>();
        if seen_names.len() != bkt2name.len() {
            #[cfg(feature = "debug-resolve-name")]
            {
                let mut seen_names = HashSet::new();
                println!("Duplicated names:");
                for (bkt, name) in &bkt2name {
                    if !seen_names.insert(name) {
                        println!("{}: {}", bkt, name);
                    }
                }
            }
            return Err(report!(NameError::DuplicatedNames));
        }
        #[cfg(feature = "debug-resolve-name")]
        {
            println!("All Resolved names:");
            for (bkt, name) in &bkt2name {
                println!("{}: {}", bkt, name);
            }
        }
        Ok(bkt2name)
    }
}

struct Bucket {
    type_: BucketType,
    names: HashSet<TypeName>,
    candidates: Vec<Offset>,
    original_candidates: Vec<Offset>,
}

impl Bucket {
    fn new(off: Vec<Offset>) -> Self {
        Self {
            type_: BucketType::Unknown,
            names: Default::default(),
            candidates: off.clone(),
            original_candidates: off,
        }
    }
    fn check(&self, id: &Offset, off2info: &BTreeMap<Offset, TypeInfo>) {
        if self.candidates.is_empty() {
            panic!("Empty bucket {}: {:?}", id, self.names);
        }
        if self.type_ == BucketType::Unknown {
            panic!("Unknown type in bucket {}", id);
        }
        for c in &self.candidates {
            let info = off2info.get(c).unwrap();
            if !matches!(info, TypeInfo::Typedef(_,_)) {
                return;
            }
        }
        panic!("All candidates are typedefs in bucket {}", id);
    }

    fn reduce(&mut self, off2info: &BTreeMap<Offset, TypeInfo>, namespaces: &NamespaceMap) {
        if self.type_ == BucketType::Prim {
            // if self is already reduced to primitive, no need to do anything
            return;
        }
        for candidate in std::mem::take(&mut self.candidates) {
            let info = off2info.get(&candidate).unwrap();
            match info {
                TypeInfo::Prim(p) => {
                    // if bucket contains primitive, reduce the whole bucket to that primitive
                    self.type_ = BucketType::Prim;
                    self.names.clear();
                    self.names.insert(TypeName::Prim(p.clone()));
                    self.candidates.clear();
                    self.candidates.push(candidate);
                    return;
                }
                TypeInfo::Typedef(name, _) => {
                    // for typedef, absorb it into the names
                    self.names.insert(TypeName::Name(name.clone()));
                }
                TypeInfo::Enum(x) => {
                    // take the name
                    if let Some(name) = &x.name {
                        self.names.insert(TypeName::Name(name.clone()));
                    } else {
                        let hash = hash_members(x.enumerators.iter().map(|x|x.0.as_str()));
                        let name = TypeName::anonymous_enum(candidate.into(), namespaces, hash).unwrap();
                        self.names.insert(name);
                    }
                    if BucketType::Enum.is_preferred_over(&self.type_) {
                        self.type_ = BucketType::Enum;
                        self.candidates.clear();
                    }
                    self.candidates.push(candidate);
                }
                TypeInfo::Comp(_) => {
                    if BucketType::Comp.is_preferred_over(&self.type_) {
                        // change self type to comp
                        self.type_ = BucketType::Comp;
                        self.candidates.clear();
                    }
                    // self is already comp, add the candidate
                    self.candidates.push(candidate);
                }
                TypeInfo::Struct(x) => {
                    // take the name
                    if let Some(name) = &x.name {
                        self.names.insert(TypeName::Name(name.clone()));
                    } else {
                        let hash = hash_members(x.members.iter().map(|x|x.name.as_deref().unwrap_or("anonymous")));
                        let name = TypeName::anonymous_struct(candidate.into(), namespaces, hash).unwrap();
                        self.names.insert(name);
                    }
                    if BucketType::Struct.is_preferred_over(&self.type_) {
                        self.type_ = BucketType::Struct;
                        self.candidates.clear();
                    }
                    self.candidates.push(candidate);
                }
                TypeInfo::Union(x) => {
                    // take the name
                    if let Some(name) = &x.name {
                        self.names.insert(TypeName::Name(name.clone()));
                    } else {
                        let hash = hash_members(x.members.iter().map(|x|x.0.as_deref().unwrap_or("anonymous")));
                        let name = TypeName::anonymous_union(candidate.into(), namespaces, hash).unwrap();
                        self.names.insert(name);
                    }
                    if BucketType::Union.is_preferred_over(&self.type_) {
                        self.type_ = BucketType::Union;
                        self.candidates.clear();
                    }
                    self.candidates.push(candidate);
                }
            }
        }
        // remove declarations if there are any non-declarations
        if self.candidates.len() > 1 {
            let mut has_decl = false;
            let mut has_non_decl = false;
            for candidates in &self.candidates {
                let info = off2info.get(&candidates).unwrap();
                if info.is_decl() {
                    has_decl = true;
                } else {
                    has_non_decl = true;
                }
            }
            if has_decl && has_non_decl {
                self.candidates
                    .retain(|candidate| !off2info.get(&candidate).unwrap().is_decl())
            }
        }

        // clear names for composite type, because we will use a composite name
        if self.type_ == BucketType::Comp {
            self.names.clear();
        }
    }

    fn reduce_comp_name(&mut self, 
        off2info: &BTreeMap<Offset, TypeInfo>, 
        off2bkt: &BTreeMap<Offset, Offset>,
        bkt2name: &BTreeMap<Offset, TypeName>) {
        'out: for candidate in std::mem::take(&mut self.candidates) {
            let info = off2info.get(&candidate).unwrap();
            let c = match info {
                TypeInfo::Comp(c) => c,
                _ => panic!("reduce_comp_name called on non-comp"),
            };
            match c {
                TypeComp::Ptr(t) => {
                    let bkt_t = off2bkt.get(t).unwrap();
                    if let Some(name) = bkt2name.get(bkt_t) {
                        self.names.insert(TypeName::pointer(name.clone()));
                    } else {
                        self.candidates.push(candidate);
                    }
                }
                TypeComp::Array(t, l) => {
                    let bkt_t = off2bkt.get(t).unwrap();
                    if let Some(name) = bkt2name.get(bkt_t) {
                        self.names.insert(TypeName::array(name.clone(), *l));
                    } else {
                        self.candidates.push(candidate);
                    }
                }
                TypeComp::Subroutine(sub) => {
                    let bkt_ret = off2bkt.get(&sub.retty).unwrap();
                    let name_ret = match bkt2name.get(bkt_ret) {
                        Some(name) => name.clone(),
                        None => {
                            self.candidates.push(candidate);
                            continue;
                        }
                    };
                    let mut params = Vec::new();
                    for t in &sub.params {
                        let bkt_t = off2bkt.get(t).unwrap();
                        if let Some(name) = bkt2name.get(bkt_t) {
                            params.push(name.clone());
                        } else {
                            self.candidates.push(candidate);
                            continue 'out;
                        }
                    }
                    let sub = TypeComp::subroutine(name_ret, params);
                    self.names.insert(TypeName::Comp(Box::new(sub)));
                }
                TypeComp::Ptmf(this, sub) => {
                    let bkt_this = off2bkt.get(this).unwrap();
                    let name_this = match bkt2name.get(bkt_this) {
                        Some(name) => name.clone(),
                        None => {
                            self.candidates.push(candidate);
                            continue;
                        }
                    };
                    let bkt_ret = off2bkt.get(&sub.retty).unwrap();
                    let name_ret = match bkt2name.get(bkt_ret) {
                        Some(name) => name.clone(),
                        None => {
                            self.candidates.push(candidate);
                            continue;
                        }
                    };
                    let mut params = Vec::new();
                    for t in &sub.params {
                        let bkt_t = off2bkt.get(t).unwrap();
                        if let Some(name) = bkt2name.get(bkt_t) {
                            params.push(name.clone());
                        } else {
                            self.candidates.push(candidate);
                            continue 'out;
                        }
                    }
                    let sub = TypeComp::ptmf(name_this, name_ret, params);
                    self.names.insert(TypeName::Comp(Box::new(sub)));
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum BucketType {
    Prim,
    Enum,
    Comp,
    Struct,
    Union,
    Unknown,
}

impl BucketType {
    fn is_preferred_over(&self, other: &BucketType) -> bool {
        match (self, other) {
            (Self::Prim, _) => true,

            (Self::Enum, Self::Prim) => false,
            (Self::Enum, _) => true,

            (Self::Comp, Self::Prim | Self::Enum) => false,
            (Self::Comp, _) => true,

            (Self::Struct, Self::Prim | Self::Enum | Self::Comp) => false,
            (Self::Struct, _) => true,

            (Self::Union, Self::Prim | Self::Enum | Self::Comp | Self::Struct) => false,
            (Self::Union, _) => true,

            (Self::Unknown, _) => false,
        }
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
