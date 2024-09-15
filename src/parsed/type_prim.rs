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
