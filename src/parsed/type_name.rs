use error_stack::{Result, ResultExt};

use super::{Error, NamespaceMap, TypeComp, TypePrim, TypeYaml};

/// Information of resolved type name
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeName {
    /// Primitive
    Prim(TypePrim),
    /// Named type
    Name(String),
    /// Compound, indexing into offset_to_bucket
    Comp(Box<TypeComp<TypeName>>),
}

impl TypeName {
    pub fn anonymous_struct(offset: usize, ns: &NamespaceMap) -> Result<Self, Error> {
        Self::anonymous(offset, ns, "struct")
    }
    pub fn anonymous_enum(offset: usize, ns: &NamespaceMap) -> Result<Self, Error> {
        Self::anonymous(offset, ns, "enum")
    }
    pub fn anonymous_union(offset: usize, ns: &NamespaceMap) -> Result<Self, Error> {
        Self::anonymous(offset, ns, "union")
    }
    /// Create an anonymous type name
    fn anonymous(offset: usize, ns: &NamespaceMap, tag: &str) -> Result<Self, Error> {
        let name = ns
            .get(offset, &format!("anonymous_{}", tag))
            .attach_printable_lazy(|| format!("While creating anonymous {}", tag))?;
        Ok(Self::Name(name))
    }

    pub fn pointer(to: Self) -> Self {
        Self::Comp(Box::new(TypeComp::Ptr(to)))
    }
    pub fn array(to: Self, len: usize) -> Self {
        Self::Comp(Box::new(TypeComp::Array(to, len)))
    }
}

impl TypeYaml for TypeName {
    fn yaml_string(&self) -> String {
        match self {
            TypeName::Prim(p) => p.yaml_string(),
            TypeName::Name(n) => format!("'\"{}\"'", n),
            TypeName::Comp(c) => c.yaml_string(),
        }
    }
}

impl std::fmt::Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeName::Prim(prim) => write!(f, "{}", prim),
            TypeName::Name(name) => write!(f, "\"{}\"", name),
            TypeName::Comp(c) => write!(f, "{}", c),
        }
    }
}

impl std::fmt::Display for TypeComp<TypeName> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let param_ty = match self {
            TypeComp::Ptr(t) => return write!(f, "{}*", t),
            TypeComp::Array(t, n) => return write!(f, "{}[{}]", t, n),
            TypeComp::Subroutine(ret_ty, param_ty) => {
                write!(f, "({ret_ty})(")?;
                param_ty
            }
            TypeComp::Ptmf(this_ty, ret_ty, param_ty) => {
                write!(f, "{this_ty}::({ret_ty})(")?;
                param_ty
            }
        };
        let mut iter = param_ty.iter();
        if let Some(t) = iter.next() {
            write!(f, "{t}")?;
        }
        for t in iter {
            write!(f, ", {t}")?;
        }
        write!(f, ")")
    }
}

impl TypeName {
    /// Return if this type name is primitive, or composed of primitive types
    pub fn is_primitive(&self) -> bool {
        //, name: &BTreeMap<usize, TypeName>) -> bool {
        match self {
            Self::Prim(_) => true,
            Self::Comp(c) => match c.as_ref() {
                TypeComp::Ptr(t) => t.is_primitive(), //name.get(t).unwrap().is_primitive(name),
                TypeComp::Array(t, _) => t.is_primitive(), //name.get(t).unwrap().is_primitive(name),
                TypeComp::Subroutine(ret_ty, param_ty) => {
                    ret_ty.is_primitive()//name.get(ret_ty).unwrap().is_primitive(name)
                        && param_ty.iter().all(|t| t.is_primitive()) //name.get(t).unwrap().is_primitive(name))
                }
                TypeComp::Ptmf(this_ty, ret_ty, param_ty) => {
                    this_ty.is_primitive()//name.get(this_ty).unwrap().is_primitive(name)
                        && ret_ty.is_primitive()//name.get(ret_ty).unwrap().is_primitive(name)
                        && param_ty.iter().all(|t| t.is_primitive()) //name.get(t).unwrap().is_primitive(name))
                }
            },
            _ => false,
        }
    }

    /// Count the number of anonymous types in this type name
    pub fn count_anonymous(&self) -> usize {
        //, name: &BTreeMap<usize, TypeName>) -> usize {
        match self {
            Self::Prim(_) => 0,
            Self::Name(s) => {
                if s.contains("anonymous") {
                    1
                } else {
                    0
                }
            }
            Self::Comp(c) => match c.as_ref() {
                TypeComp::Ptr(t) => t.count_anonymous(), //name.get(t).unwrap().count_anonymous(name),
                TypeComp::Array(t, _) => t.count_anonymous(), //name
                TypeComp::Subroutine(ret_ty, param_ty) => {
                    ret_ty.count_anonymous()//name.get(ret_ty).unwrap().count_anonymous(name)
                        + param_ty.iter().map(|t| t.count_anonymous()).sum::<usize>()
                    //name.get(t).unwrap().count_anonymous(name)).sum::<usize>()
                }
                TypeComp::Ptmf(this_ty, ret_ty, param_ty) => {
                    this_ty.count_anonymous()//name.get(this_ty).unwrap().count_anonymous(name)
                        + ret_ty.count_anonymous()//name.get(ret_ty).unwrap().count_anonymous(name)
                        + param_ty.iter().map(|t| t.count_anonymous()).sum::<usize>()
                    //name.get(t).unwrap().count_anonymous(name)).sum::<usize>()
                }
            },
        }
    }

    /// Return if this name is preferred over other name, based on some heuristics
    pub fn is_preferred_over(&self, other: &Self) -> std::cmp::Ordering {
        // prefer primitive types over non-primitive types
        if self.is_primitive() && !other.is_primitive() {
            return std::cmp::Ordering::Greater;
        }
        // prefer less anonymous types
        if self.count_anonymous() < other.count_anonymous() {
            return std::cmp::Ordering::Greater;
        }
        let self_name = match self {
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
                match c.as_ref() {
                    TypeComp::Ptr(t) | TypeComp::Array(t, _) => {
                        match other_c.as_ref() {
                            TypeComp::Ptr(t_c) | TypeComp::Array(t_c, _) => {
                                // let t_ty = name.get(t).unwrap();
                                // let t_c_ty = name.get(t_c).unwrap();
                                return t.is_preferred_over(t_c);
                            }
                            // prefer more compounded types
                            _ => return std::cmp::Ordering::Less,
                        }
                    }
                    TypeComp::Subroutine(ret_ty, param_ty) => {
                        match other_c.as_ref() {
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
                                    // let t_ty = name.get(t).unwrap();
                                    // let t_c_ty = name.get(t_c).unwrap();
                                    match t.is_preferred_over(t_c) {
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
                        match other_c.as_ref() {
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
                                    // let t_ty = name.get(t).unwrap();
                                    // let t_c_ty = name.get(t_c).unwrap();
                                    match t.is_preferred_over(t_c) {
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
        // prefer ksys/uking
        let self_ksys = self_name.starts_with("ksys") || self_name.starts_with("uking");
        let other_ksys = other_name.starts_with("ksys") || other_name.starts_with("uking");
        if self_ksys && !other_ksys {
            return std::cmp::Ordering::Greater;
        }

        // prefer sead
        let self_sead = self_name.starts_with("sead");
        let other_sead = other_name.starts_with("sead");
        if self_sead && !other_sead {
            return std::cmp::Ordering::Greater;
        }

        // prefer nn
        let self_nn = self_name.starts_with("nn");
        let other_nn = other_name.starts_with("nn");
        if self_nn && !other_nn {
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
