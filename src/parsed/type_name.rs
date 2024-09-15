use std::collections::BTreeMap;

use super::{TypeComp, TypePrim, TypeResolver};


#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeName {
    /// No name at offset
    Anon(usize),
    /// Primitive
    Prim(TypePrim),
    /// Named type
    Name(String),
    /// Compound, indexing into offset_to_bucket
    Comp(TypeComp<usize>),
}

pub struct PrintTypeName<'a, 'b> {
    pub name: &'a TypeName,
    pub map: &'b BTreeMap<usize, TypeName>,
}

impl<'a, 'b> std::fmt::Display for PrintTypeName<'a, 'b> where 'b : 'a{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.name {
            TypeName::Anon(offset) => write!(f, "anon_0x{:08x}", offset),
            TypeName::Prim(prim) => write!(f, "{}", prim),
            TypeName::Name(name) => write!(f, "\"{}\"", name),
            TypeName::Comp(TypeComp::Ptr(t)) => {
                let t_name = self.map.get(t).unwrap();
                write!(f, "*{}", Self { name: t_name, map: self.map })
            }
            TypeName::Comp(TypeComp::Array(t, len)) => {
                let t_name = self.map.get(t).unwrap();
                write!(f, "{}[{}]", Self { name: t_name, map: self.map }, len)
            }
            TypeName::Comp(c) => write!(f, "{}", c),
        }
    }
}

impl TypeName {
    pub fn display<'a, 'b>(&'a self, map: &'b BTreeMap<usize, TypeName>) -> PrintTypeName where 'b : 'a{
        PrintTypeName { name: self, map }
    }
    pub fn get_named(&self) -> Option<&str> {
        match self {
            Self::Name(x) => Some(x),
            _ => None,
        }
    }
    /// Return if this type name is primitive, or composed of primitive types
    pub fn is_primitive(&self, name: &BTreeMap<usize, TypeName>) -> bool {
        match self {
            Self::Prim(_) => true,
            Self::Comp(c) => match c {
                TypeComp::Ptr(t) => name
                    .get(t)
                    .unwrap_or_else(|| {
                        panic!(
                            "Failed to get type name from offset 0x{:08x}",
                            t
                        )
                    })
                    .is_primitive(name),
                TypeComp::Array(t, _) => name
                    .get(t)
                    .unwrap()
                    .is_primitive(name),
                TypeComp::Subroutine(ret_ty, param_ty) => {
                    name
                        .get(ret_ty)
                        .unwrap()
                        .is_primitive(name)
                        && param_ty.iter().all(|t| {
                            name
                                .get(t)
                                .unwrap()
                                .is_primitive(name)
                        })
                }
                TypeComp::Ptmf(this_ty, ret_ty, param_ty) => {
                    name
                        .get(this_ty)
                        .unwrap()
                        .is_primitive(name)
                        && name
                            .get(ret_ty)
                            .unwrap()
                            .is_primitive(name)
                        && param_ty.iter().all(|t| {
                            name
                                .get(t)
                                .unwrap()
                                .is_primitive(name)
                        })
                }
            },
            _ => false,
        }
    }
    
    /// Count the number of anonymous types in this type name
    pub fn count_anonymous(&self, name: &BTreeMap<usize, TypeName>) -> usize {
        match self {
            Self::Anon(_) => 1,
            Self::Comp(c) => match c {
                TypeComp::Ptr(t) => name
                    .get(t)
                    .unwrap()
                    .count_anonymous(name),
                TypeComp::Array(t, _) => name
                    .get(t)
                    .unwrap()
                    .count_anonymous(name),
                TypeComp::Subroutine(ret_ty, param_ty) => {
                    name
                        .get(ret_ty)
                        .unwrap()
                        .count_anonymous(name)
                        + param_ty
                            .iter()
                            .map(|t| {
                                name
                                    .get(t)
                                    .unwrap()
                                    .count_anonymous(name)
                            })
                            .sum::<usize>()
                }
                TypeComp::Ptmf(this_ty, ret_ty, param_ty) => {
                    name
                        .get(this_ty)
                        .unwrap()
                        .count_anonymous(name)
                        + name
                            .get(ret_ty)
                            .unwrap()
                            .count_anonymous(name)
                        + param_ty
                            .iter()
                            .map(|t| {
                                name
                                    .get(t)
                                    .unwrap()
                                    .count_anonymous(name)
                            })
                            .sum::<usize>()
                }
            },
            _ => 0,
        }
    }
    
    /// Return if this name is preferred over other name, based on some heuristics
    pub fn is_preferred_over(&self, other: &Self, name: &BTreeMap<usize, Self>) -> std::cmp::Ordering {
        // prefer primitive types over non-primitive types
        if self.is_primitive(name) && !other.is_primitive(name) {
            return std::cmp::Ordering::Greater;
        }
        // prefer less anonymous types
        if self.count_anonymous(name) < other.count_anonymous(name) {
            return std::cmp::Ordering::Greater;
        }
        let self_name = match self {
            Self::Anon(_) => {
                // self is not less anonymous than other, so prefer other
                return std::cmp::Ordering::Less;
            }
            Self::Prim(_) => {
                // self is primitive and other is primitive too, they are probably the same type
                return std::cmp::Ordering::Equal;
            }
            Self::Comp(c) => {
                let other_c = match other {
                    Self::Comp(x) => x,
                    // prefer compound types
                    _ => return std::cmp::Ordering::Greater,
                };
                match c {
                    TypeComp::Ptr(t) | TypeComp::Array(t, _) => {
                        match other_c {
                            TypeComp::Ptr(t_c) | TypeComp::Array(t_c, _) => {
                                let t_ty = name.get(t).unwrap();
                                let t_c_ty = name.get(t_c).unwrap();
                                return t_ty.is_preferred_over(t_c_ty, name);
                            }
                            // prefer more compounded types
                            _ => return std::cmp::Ordering::Less,
                        }
                    }
                    TypeComp::Subroutine(ret_ty, param_ty) => {
                        match other_c {
                            TypeComp::Ptr(_) | TypeComp::Array(_, _) => {
                                // prefer more compounded types
                                return std::cmp::Ordering::Greater;
                            }
                            TypeComp::Subroutine(ret_ty_c, param_ty_c) => {
                                let mut less_count = 0;
                                let mut more_count = 0;
    
                                let ty_iter = std::iter::once(ret_ty)
                                    .chain(param_ty.iter())
                                    .zip(std::iter::once(ret_ty_c).chain(param_ty_c.iter()));
                                for (t, t_c) in ty_iter {
                                    let t_ty = name.get(t).unwrap();
                                    let t_c_ty = name.get(t_c).unwrap();
                                    match t_ty.is_preferred_over(t_c_ty, name) {
                                        std::cmp::Ordering::Less => {
                                            if more_count > 0 {
                                                return std::cmp::Ordering::Equal;
                                            }
                                            less_count += 1;
                                        }
                                        std::cmp::Ordering::Greater => {
                                            if less_count > 0 {
                                                return std::cmp::Ordering::Equal;
                                            }
                                            more_count += 1;
                                        }
                                        _ => (),
                                    }
                                }
    
                                if more_count > 0 {
                                    return std::cmp::Ordering::Greater;
                                }
                                if less_count > 0 {
                                    return std::cmp::Ordering::Less;
                                }
                                return std::cmp::Ordering::Equal;
                            }
                            // prefer more compounded types
                            _ => return std::cmp::Ordering::Less,
                        }
                    }
                    TypeComp::Ptmf(this_ty, ret_ty, param_ty) => {
                        match other_c {
                            TypeComp::Ptmf(this_ty_c, ret_ty_c, param_ty_c) => {
                                let mut less_count = 0;
                                let mut more_count = 0;
    
                                let ty_iter = std::iter::once(this_ty)
                                    .chain(std::iter::once(ret_ty))
                                    .chain(param_ty.iter())
                                    .zip(
                                        std::iter::once(this_ty_c)
                                            .chain(std::iter::once(ret_ty_c))
                                            .chain(param_ty_c.iter()),
                                    );
                                for (t, t_c) in ty_iter {
                                    let t_ty = name.get(t).unwrap();
                                    let t_c_ty = name.get(t_c).unwrap();
                                    match t_ty.is_preferred_over(t_c_ty, name) {
                                        std::cmp::Ordering::Less => {
                                            if more_count > 0 {
                                                return std::cmp::Ordering::Equal;
                                            }
                                            less_count += 1;
                                        }
                                        std::cmp::Ordering::Greater => {
                                            if less_count > 0 {
                                                return std::cmp::Ordering::Equal;
                                            }
                                            more_count += 1;
                                        }
                                        _ => (),
                                    }
                                }
    
                                if more_count > 0 {
                                    return std::cmp::Ordering::Greater;
                                }
                                if less_count > 0 {
                                    return std::cmp::Ordering::Less;
                                }
                                return std::cmp::Ordering::Equal;
                            }
                            // prefer more compounded types
                            _ => return std::cmp::Ordering::Less,
                        }
                    }
                }
            }
            Self::Name(x) => x,
        };
    
        let other_name = match other {
            Self::Anon(_) => {
                // prefer named over anonymous
                return std::cmp::Ordering::Greater;
            }
            Self::Prim(_) => {
                // prefer primitive over named
                return std::cmp::Ordering::Less;
            }
            Self::Comp(_) => {
                // prefer compound over named
                return std::cmp::Ordering::Less;
            }
            Self::Name(x) => x,
        };
    
        // prefer less whitespace
        let self_spaces = self_name.chars().filter(|c| c.is_whitespace()).count();
        let other_spaces = other_name.chars().filter(|c| c.is_whitespace()).count();
        if self_spaces < other_spaces {
            return std::cmp::Ordering::Greater;
        }
    
        // prefer less < >
        let self_angle = self_name.chars().filter(|c| *c == '<' || *c == '>').count();
        let other_angle = other_name
            .chars()
            .filter(|c| *c == '<' || *c == '>')
            .count();
        if self_angle < other_angle {
            return std::cmp::Ordering::Greater;
        }
    
        // prefer std
        if self_name.starts_with("std::") && !other_name.starts_with("std::") {
            return std::cmp::Ordering::Greater;
        }
    
        // prefer fewer underscores
        let self_underscores = self_name.chars().filter(|c| *c == '_').count();
        let other_underscores = other_name.chars().filter(|c| *c == '_').count();
        if self_underscores < other_underscores {
            return std::cmp::Ordering::Greater;
        }
    
        // prefer shorter
        other_name.len().cmp(&self_name.len())
    }
}
