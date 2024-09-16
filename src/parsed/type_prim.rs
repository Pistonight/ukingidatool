/// Primitive types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum TypePrim {
    #[default]
    /// Empty, void
    Void,
    /// Boolean
    Bool,
    /// Unsigned, 8-bit
    U8,
    /// Unsigned, 16-bit
    U16,
    /// Unsigned, 32-bit
    U32,
    /// Unsigned, 64-bit
    U64,
    /// Unsigned, 128-bit
    U128,
    /// Signed, 8-bit
    I8,
    /// Signed, 16-bit
    I16,
    /// Signed, 32-bit
    I32,
    /// Signed, 64-bit
    I64,
    /// Signed, 128-bit
    I128,
    /// Floating point, 32-bit
    F32,
    /// Floating point, 64-bit
    F64,
    /// Floating point, 128-bit
    F128,
}

pub trait TypeYaml {
    fn yaml_string(&self) -> String;
}

impl TypePrim {
    pub fn size(&self) -> Option<usize> {
        Some(match self {
            Self::Void => return None,
            Self::Bool => 1,
            Self::U8 | Self::I8 => 1,
            Self::U16 | Self::I16 => 2,
            Self::U32 | Self::I32 | Self::F32 => 4,
            Self::U64 | Self::I64 | Self::F64 => 8,
            Self::U128 | Self::I128 | Self::F128 => 16,
        })
    }

    pub fn ida_type(&self) -> &'static str {
        match self {
            Self::Void => "void",
            Self::Bool => "bool",
            Self::U8 => "unsigned char",
            Self::U16 => "unsigned short",
            Self::U32 => "unsigned int",
            Self::U64 => "unsigned long",
            Self::U128 => "unsigned __int128",
            Self::I8 => "char",
            Self::I16 => "short",
            Self::I32 => "int",
            Self::I64 => "long",
            Self::I128 => "__int128",
            Self::F32 => "float",
            Self::F64 => "double",
            Self::F128 => "long double",
        }
    }
    
}

impl std::fmt::Display for TypePrim {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypePrim::Void => write!(f, "void"),
            TypePrim::Bool => write!(f, "bool"),
            TypePrim::U8 => write!(f, "u8"),
            TypePrim::U16 => write!(f, "u16"),
            TypePrim::U32 => write!(f, "u32"),
            TypePrim::U64 => write!(f, "u64"),
            TypePrim::U128 => write!(f, "u128"),
            TypePrim::I8 => write!(f, "i8"),
            TypePrim::I16 => write!(f, "i16"),
            TypePrim::I32 => write!(f, "i32"),
            TypePrim::I64 => write!(f, "i64"),
            TypePrim::I128 => write!(f, "i128"),
            TypePrim::F32 => write!(f, "f32"),
            TypePrim::F64 => write!(f, "f64"),
            TypePrim::F128 => write!(f, "f128"),
        }
    }
}

impl TypeYaml for TypePrim {
    fn yaml_string(&self) -> String {
        self.to_string()
    }
}

/// Composite or compound types
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeComp<T> {
    /// T* or T&
    Ptr(T),
    /// T[elem_count]
    Array(T, usize),
    /// Function type with (ret_ty, param_ty)
    Subroutine(T, Vec<T>),
    /// Ptr-to-member-function (this_ty, ret_ty, param_ty)
    Ptmf(T, T, Vec<T>),
}

impl<T> TypeComp<T> {
    pub fn convert<T2>(self, f: impl Fn(T) -> T2) -> TypeComp<T2> {
        match self {
            TypeComp::Ptr(t) => TypeComp::Ptr(f(t)),
            TypeComp::Array(t, n) => TypeComp::Array(f(t), n),
            TypeComp::Subroutine(ret_ty, param_ty) => {
                TypeComp::Subroutine(f(ret_ty), param_ty.into_iter().map(f).collect())
            }
            TypeComp::Ptmf(this_ty, ret_ty, param_ty) => {
                TypeComp::Ptmf(f(this_ty), f(ret_ty), param_ty.into_iter().map(f).collect())
            }
        }
    }
    
}

impl std::fmt::Display for TypeComp<usize> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let param_ty = match self {
            TypeComp::Ptr(t) => return write!(f, "<0x{t:08x}>*"),
            TypeComp::Array(t, n) => return write!(f, "<0x{t:08x}>[{n}]"),
            TypeComp::Subroutine(ret_ty, param_ty) => {
                write!(f, "(<0x{ret_ty:08x}>)(")?;
                param_ty
            }
            TypeComp::Ptmf(this_ty, ret_ty, param_ty) => {
                write!(f, "<0x{this_ty:08x}>::(<0x{ret_ty:08x}>)(")?;
                param_ty
            }
        };
        let mut iter = param_ty.iter();
        if let Some(t) = iter.next() {
            write!(f, "<0x{t:08x}>")?;
        }
        for t in iter {
            write!(f, ", <0x{t:08x}>")?;
        }
        write!(f, ")")
    }
}

impl<T: TypeYaml> TypeYaml for TypeComp<T> {
    fn yaml_string(&self) -> String {
        let (mut s, param_ty) = match self {
            TypeComp::Ptr(t) => return format!("{},'*'", t.yaml_string()),
            TypeComp::Array(t, n) => return format!("{},[{}]", t.yaml_string(), n),
            TypeComp::Subroutine(ret_ty, param_ty) => {
                let s = format!("{},'()', [", ret_ty.yaml_string());
                (s, param_ty)
            }
            TypeComp::Ptmf(this_ty, ret_ty, param_ty) => {
                let s = format!("{},'(ptmf)', [{}], [", ret_ty.yaml_string(), this_ty.yaml_string());
                (s, param_ty)
            }
        };

        let mut iter = param_ty.iter();
        if let Some(t) = iter.next() {
            s.push_str(&format!("[{}]", t.yaml_string()));
        }
        for t in iter {
            s.push_str(&format!(", [{}]", t.yaml_string()));
        }
        s.push_str("]");
        s
    }
}
