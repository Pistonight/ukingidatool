use std::sync::Arc;

use super::{TypeComp, TypePrim};

pub enum TypeDef {
    /// Primitive type def
    Prim(TypePrim),
    /// Struct or Class
    Struct(StructDef),
    /// Enum
    Enum(EnumDef),
    /// Union
    Union(UnionDef),
    /// Compound type
    Comp(TypeComp<Arc<TypeDef>>),
}

impl TypeDef {
    pub fn is_zst(&self) -> bool {
        match self {
            Self::Prim(_) => false,
            Self::Struct(s) => s.size == 0,
            Self::Enum(_) => false,
            Self::Union(u) => u.size == 0,
            Self::Comp(_) => false,
        }
    }
    
}

impl std::fmt::Display for TypeDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let param_ty = match self {
            Self::Prim(prim) => return write!(f, "{}", prim),
            Self::Struct(s) => return write!(f, "struct {}", s.name.as_ref().map(|x|x.as_str()).unwrap_or("<unnamed>")),
            Self::Enum(e) => return write!(f, "enum {}", e.name.as_ref().map(|x|x.as_str()).unwrap_or("<unnamed>")),
            Self::Union(u) => return write!(f, "union {}", u.name.as_ref().map(|x|x.as_str()).unwrap_or("<unnamed>")),
            Self::Comp(TypeComp::Ptr(t)) => return write!(f, "{t}*"),
            Self::Comp(TypeComp::Array(t, n)) => return write!(f, "{t}[{n}]"),
            Self::Comp(TypeComp::Subroutine(ret_ty, param_ty)) => {
                write!(f, "({ret_ty})(")?;
                param_ty
            }
            Self::Comp(TypeComp::Ptmf(this_ty, ret_ty, param_ty)) => {
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

pub struct StructDef {
    pub name: Option<String>,
    pub vtable: Vec<String>,
    pub size: usize,
    pub members: Vec<MemberDef>,
}

pub struct MemberDef {
    pub offset: usize,
    pub name: Option<String>,
    pub is_base: bool,
    pub ty: Arc<TypeDef>,
}

pub struct EnumDef {
    pub name: Option<String>,
    pub size: usize,
    pub enumerators: Vec<(String, i128)>,
}

pub struct UnionDef {
    pub name: Option<String>,
    pub size: usize,
    pub members: Vec<(Option<String>, Arc<TypeDef>)>,
}
