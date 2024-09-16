use super::{TypeComp, TypePrim};

/// Information of the type linked to the DWARF debug info
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeInfo {
    /// Primitive type
    Prim(TypePrim),
    /// Typedef <other> name; Other is offset in debug info
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
            Self::Struct(s) => write!(
                f,
                "struct {}",
                s.name.as_ref().map(|x| x.as_str()).unwrap_or("<unnamed>")
            ),
            Self::Enum(e) => write!(
                f,
                "enum {}",
                e.name.as_ref().map(|x| x.as_str()).unwrap_or("<unnamed>")
            ),
            Self::Union(u) => write!(
                f,
                "union {}",
                u.name.as_ref().map(|x| x.as_str()).unwrap_or("<unnamed>")
            ),
            Self::Comp(c) => write!(f, "{}", c),
        }
    }
}

/// Struct information linked to the DWARF debug info
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructInfo {
    /// The name of the struct, or an empty string if it is unnamed/anonymous
    pub name: Option<String>,
    /// If the struct is only declared, not defined
    pub is_decl: bool,
    /// The vtable of the struct. It could be incomplete before type resolution
    pub vtable: Vec<String>,
    /// The size of the struct in bytes
    pub size: usize,
    /// The members of the struct
    pub members: Vec<MemberInfo>,
}

impl StructInfo {
    /// Create a true zero-sized type
    ///
    /// zero-sized types in C++ typically have a size of 1 byte, so
    /// this is not that. This is a type that truly is 0 bytes
    pub fn zst() -> Self {
        Self {
            name: Some("ZeroSizedType".to_string()),
            is_decl: false,
            vtable: Vec::new(),
            size: 0,
            members: Vec::new(),
        }
    }
}

/// Information about a member of a struct
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemberInfo {
    /// Offset of the member in the struct
    pub offset: usize,
    /// Name of the member, or None if it is unnamed
    pub name: Option<String>,
    /// If the member is a base type (declared with DW_TAG_inheritance)
    pub is_base: bool,
    /// The type of the member, linked to the DWARF debug info
    pub ty_offset: usize,
}

/// Information about an enum
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumInfo {
    /// The name of the enum, or an empty string if it is unnamed/anonymous
    pub name: Option<String>,
    /// The size of the enum in bytes
    pub size: usize,
    /// If the enum is only declared, not defined
    pub is_decl: bool,
    /// The enumerators of the enum (name, value)
    pub enumerators: Vec<(String, i128)>,
}

/// Information about a union
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnionInfo {
    /// The name of the union, or an empty string if it is unnamed/anonymous
    pub name: Option<String>,
    /// The size of the union in bytes
    pub size: usize,
    /// If the union is only declared, not defined
    pub is_decl: bool,
    /// The members of the union (name, type_offset)
    pub members: Vec<(Option<String>, usize)>,
}
