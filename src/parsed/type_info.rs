use super::{TypeComp, TypePrim};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeInfo {
    /// Primitive type
    Prim(TypePrim),
    /// Typedef <other> name;
    Typedef(String, usize),
    /// Struct or Class
    Struct(StructInfo),
    /// Enum
    Enum(EnumInfo),
    /// Union
    Union(UnionInfo),
    /// Compound type
    Comp(TypeComp<usize>),
}

impl std::fmt::Display for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Prim(p) => write!(f, "{}", p),
            Self::Typedef(name, ty) => write!(f, "typedef <0x{ty:08x}> {name}"),
            Self::Struct(s) => write!(f, "struct {}", s.name.as_ref().map(|x|x.as_str()).unwrap_or("<unnamed>")),
            Self::Enum(e) => write!(f, "enum {}", e.name.as_ref().map(|x|x.as_str()).unwrap_or("<unnamed>")),
            Self::Union(u) => write!(f, "union {}", u.name.as_ref().map(|x|x.as_str()).unwrap_or("<unnamed>")),
            Self::Comp(c) => write!(f, "{}", c),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructInfo {
    pub name: Option<String>,
    pub vtable: Vec<String>, // function names
    pub size: usize,
    pub members: Vec<MemberInfo>,
}

impl StructInfo {
    pub fn zst() -> Self {
        Self {
            name: Some("ZeroSizedType".to_string()),
            vtable: Vec::new(),
            size: 0,
            members: Vec::new(),
        }
    }
    
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemberInfo {
    pub offset: usize,
    pub name: Option<String>,
    pub is_base: bool,
    pub ty_offset: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumInfo {
    pub name: Option<String>,
    pub size: usize,
    pub enumerators: Vec<(String, i128)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnionInfo {
    pub name: Option<String>,
    pub size: usize,
    pub members: Vec<(Option<String>, usize)>,
}

