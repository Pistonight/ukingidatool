use std::{
    borrow::Borrow,
    collections::{btree_map::Entry, BTreeMap, BTreeSet}, rc::Rc, sync::Arc,
};

use error_stack::{report, Result};


pub struct TypeDefs {
    offset_to_type: BTreeMap<usize, TypeDef>,
}

impl TypeDefs {
    pub fn get_type_def(&self, info: &BaseTypeInfo) -> Option<TypeDef> {
        todo!()
    }

    fn are_type_equiv(&self, a: &TypeDef, b: &TypeDef) -> bool {
    }
}

pub enum TypeDef {
    Struct(StructInfo),
    Enum(EnumInfo),
    Union(UnionInfo),
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Conflicting definition of struct: {0}")]
    StructMismatch(String, StructInfo, StructInfo),
    #[error("Conflicting definition of enum: {0}")]
    EnumMismatch(String, EnumInfo, EnumInfo),
    #[error("Conflicting definition of union: {0}")]
    UnionMismatch(String, UnionInfo, UnionInfo),
    #[error("Conflicting definition at address: 0x{0:x}")]
    AddressMismatch(u64, AddressInfo, AddressInfo),

    #[error("Struct `{0}` is referencing a non-existing type `{1}`")]
    StructRef(String, String),
    #[error("Union `{0}` is referencing a non-existing type `{1}`")]
    UnionRef(String, String),
    #[error("Data `{1}` at 0x{0:x} is referencing a non-existing type `{2}`")]
    DataRef(u64, String, String),
    #[error("Function `{1}` at 0x{0:x} is referencing a non-existing type `{2}`")]
    FuncRef(u64, String, String),
    #[error("Arg {2} of `{1}` at 0x{0:x} is referencing a non-existing type `{3}`")]
    FuncArgRef(u64, String, usize, String),
}

pub struct InfoBuilder {
    structs: BTreeMap<String, StructInfo>,
    enums: BTreeMap<String, EnumInfo>,
    unions: BTreeMap<String, UnionInfo>,
    addresses: BTreeMap<u64, AddressInfo>,
}

impl InfoBuilder {
    /// Add a struct info
    ///
    /// Returns an error if a struct with the same name is already defined with a different info
    pub fn add_struct(&mut self, s: StructInfo) -> Result<(), Error> {
        match self.structs.entry(s.name.clone()) {
            Entry::Occupied(mut entry) => {
                if entry.get() != &s {
                    let old = entry.get().clone();
                    let new = s;
                    let r = report!(Error::StructMismatch(
                        new.name.clone(),
                        old.clone(),
                        new.clone()
                    ))
                    .attach_printable(old)
                    .attach_printable(new);
                    return Err(r);
                }
                *entry.get_mut() = s;
            }
            Entry::Vacant(e) => {
                e.insert(s);
            }
        };
        Ok(())
    }

    /// Add an enum info
    ///
    /// Returns an error if an enum with the same name is already defined with a different info
    pub fn add_enum(&mut self, e: EnumInfo) -> Result<(), Error> {
        match self.enums.entry(e.name.clone()) {
            Entry::Occupied(mut entry) => {
                if entry.get() != &e {
                    let old = entry.get().clone();
                    let new = e;
                    let r = report!(Error::EnumMismatch(
                        new.name.clone(),
                        old.clone(),
                        new.clone()
                    ))
                    .attach_printable(old)
                    .attach_printable(new);
                    return Err(r);
                }
                *entry.get_mut() = e;
            }
            Entry::Vacant(entry) => {
                entry.insert(e);
            }
        };
        Ok(())
    }

    /// Add a union info
    ///
    /// Returns an error if a union with the same name is already defined with a different info
    pub fn add_union(&mut self, u: UnionInfo) -> Result<(), Error> {
        match self.unions.entry(u.name.clone()) {
            Entry::Occupied(mut entry) => {
                if entry.get() != &u {
                    let old = entry.get().clone();
                    let new = u;
                    let r = report!(Error::UnionMismatch(
                        new.name.clone(),
                        old.clone(),
                        new.clone()
                    ))
                    .attach_printable(old)
                    .attach_printable(new);
                    return Err(r);
                }
                *entry.get_mut() = u;
            }
            Entry::Vacant(entry) => {
                entry.insert(u);
            }
        };
        Ok(())
    }

    /// Add an address info
    ///
    /// Returns an error if the address is already defined with a different info
    pub fn add_address(&mut self, a: AddressInfo) -> Result<(), Error> {
        match self.addresses.entry(a.address) {
            Entry::Occupied(mut entry) => {
                if entry.get() != &a {
                    let old = entry.get().clone();
                    let new = a;
                    let r = report!(Error::AddressMismatch(
                        new.address,
                        old.clone(),
                        new.clone()
                    ))
                    .attach_printable(old)
                    .attach_printable(new);
                    return Err(r);
                }
                *entry.get_mut() = a;
            }
            Entry::Vacant(entry) => {
                entry.insert(a);
            }
        };
        Ok(())
    }

    pub fn build(self) -> Result<Info, Error> {
        self.validate_type_refs()?;
        Ok(Info {
            structs: self.structs.into_values().collect(),
            enums: self.enums.into_values().collect(),
            unions: self.unions.into_values().collect(),
            addresses: self.addresses.into_values().collect(),
        })
    }

    /// Verify types referenced in structs, unions, and addresses
    /// are defined
    fn validate_type_refs(&self) -> Result<(), Error> {
        for s in self.structs.values() {
            if let Some(base) = &s.base {
                if !self.type_exists(base) {
                    return Err(report!(Error::StructRef(
                        s.name.clone(), base.clone())));
                }
            }
            for m in &s.members {
                if let Some(ty) = m.arg.ty.get_reference() {
                    if !self.type_exists(ty) {
                        return Err(report!(Error::StructRef(
                            s.name.clone(), ty.to_string())));
                    }
                }
            }
        }

        for u in self.unions.values() {
            for m in &u.members {
                if let Some(ty) = m.ty.get_reference() {
                    if !self.type_exists(ty) {
                        return Err(report!(Error::UnionRef(
                            u.name.clone(), ty.to_string())));
                    }
                }
            }
        }

        for a in self.addresses.values() {
            if let Some(ty) = &a.ty {
                if let Some(ty) = ty.get_reference() {
                    if !self.type_exists(ty) {
                        if a.is_func {
                            return Err(report!(Error::FuncRef(
                                a.address, a.name.clone(), ty.to_string())));
                        } else {
                            return Err(report!(Error::DataRef(
                                a.address, a.name.clone(), ty.to_string())));
                        }
                    }
                }
            }
            for (i, arg) in a.args.iter().enumerate() {
                if let Some(ty) = arg.ty.get_reference() {
                    if !self.type_exists(ty) {
                        return Err(report!(Error::FuncArgRef(
                            a.address, a.name.clone(), i, ty.to_string())));
                    }
                }
            }
        }

        Ok(())
    }

    fn type_exists(&self, name: &str) -> bool {
        self.structs.contains_key(name)
            || self.enums.contains_key(name)
            || self.unions.contains_key(name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Info {
    pub structs: Vec<StructInfo>,
    pub enums: Vec<EnumInfo>,
    pub unions: Vec<UnionInfo>,
    pub addresses: Vec<AddressInfo>,
}

impl std::fmt::Display for Info {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "addresses:")?;
        for a in &self.addresses {
            writeln!(f, "{}", a)?;
        }
        writeln!(f, "structs:")?;
        for s in &self.structs {
            writeln!(f, "{}", s)?;
        }
        writeln!(f, "enums:")?;
        for e in &self.enums {
            writeln!(f, "{}", e)?;
        }
        writeln!(f, "unions:")?;
        for u in &self.unions {
            writeln!(f, "{}", u)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructInfo {
    pub name: String,
    pub base: Option<String>,
    pub size: usize,
    pub members: Vec<StructMember>,
}

impl std::fmt::Display for StructInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "- name: \"{}\"", self.name)?;
        if let Some(base) = &self.base {
            writeln!(f, "  base: \"{}\"", base)?;
        }
        writeln!(f, "  size: 0x{:x}", self.size)?;
        writeln!(f, "  members:")?;
        for member in &self.members {
            writeln!(f, "  {}", member)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumInfo {
    pub name: String,
    pub ty: TypeInfo,
    pub bitfield: bool,
    pub enumerators: Vec<(String, u128)>,
}

impl std::fmt::Display for EnumInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "- name: \"{}\"", self.name)?;
        writeln!(f, "  ty: [{}]", self.ty)?;
        writeln!(f, "  bitfield: {}", self.bitfield)?;
        writeln!(f, "  enumerators:")?;
        for (name, value) in &self.enumerators {
            writeln!(f, "  - {{ name: \"{}\", value: 0x{:x} }}", name, value)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnionInfo {
    pub name: String,
    pub members: Vec<Arg>,
}

impl std::fmt::Display for UnionInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "- name: \"{}\"", self.name)?;
        writeln!(f, "  members:")?;
        for member in &self.members {
            writeln!(f, "  - {}", member)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeInfo {
    /// Pointer (or reference) of a type `ty*`
    Pointer(Box<TypeInfo>),
    /// Array of a type `ty[len]`
    Array(Box<TypeInfo>, u64),
    /// Function pointer type `ty (...)`
    Subroutine(Box<TypeInfo>, Vec<TypeInfo>),
    /// Ptr-to-member function type `ty (Class::*)(...)`
    Ptmf(Box<TypeInfo>, Box<TypeInfo>),
}

impl TypeInfo {
    /// Returns a reference to a named type if this type contains any
    ///
    /// For example, returns the base type of a pointer or array type.
    /// For primitive types, this will return `None`.
    pub fn get_reference(&self) -> Option<&str> {
        match self {
            TypeInfo::Named(name) => Some(name),
            TypeInfo::Pointer(ty) => ty.get_reference(),
            TypeInfo::Array(ty, _) => ty.get_reference(),
            _ => todo!(),
        }
    }

    pub fn is_prim(&self) -> bool {
        match self {
            TypeInfo::Named(_) => false,
            TypeInfo::Anonymous(_) => false,
            TypeInfo::Pointer(ty) => ty.is_prim(),
            TypeInfo::Array(ty, _) => ty.is_prim(),
            TypeInfo::Subroutine(retty, argty) => {
                retty.is_prim() && argty.iter().all(|ty| ty.is_prim())
            },
            _ => true,
        }
    }

    pub fn is_anonymous(&self) -> bool {
        match self {
            TypeInfo::Anonymous(_) => true,
            TypeInfo::Pointer(ty) => ty.is_anonymous(),
            TypeInfo::Array(ty, _) => ty.is_anonymous(),
            TypeInfo::Subroutine(retty, argty) => {
                retty.is_anonymous() || argty.iter().any(|ty| ty.is_anonymous())
            }
            _ => false,
        }
    }

    pub fn named(name: impl Into<String>) -> Self {
        TypeInfo::Named(name.into())
    }

    pub fn is_preferred_over(&self, other: &TypeInfo) -> bool {
        // primitive types are always preferred
        if self.is_prim() && !other.is_prim() {
            return true;
        }
        // non-anonymous types are preferred
        if !self.is_anonymous() && other.is_anonymous() {
            return true;
        }
        // types with no spaces are preferred
        let self_string = self.to_string();
        let other_string = other.to_string();
        let self_spaces = self_string.chars().filter(|c| c.is_whitespace()).count();
        let other_spaces = other_string.chars().filter(|c|c.is_whitespace()).count();
        if self_spaces < other_spaces {
            return true;
        }
        // non parametrized types are preferred
        let self_parametrized = self_string.chars().filter(|c| *c=='<'||*c=='>').count();
        let other_parametrized = other_string.chars().filter(|c| *c=='<'||*c=='>').count();
        if self_parametrized < other_parametrized {
            return true;
        }

        // std types are preferred
        if self_string.starts_with("std::") && !other_string.starts_with("std::") {
            return true;
        }

        // fewer underscore is preferred
        let self_underscore = self_string.chars().filter(|c| *c=='_').count();
        let other_underscore = other_string.chars().filter(|c| *c=='_').count();
        if self_underscore < other_underscore {
            return true;
        }

        // shorter is better
        self_string.len() < other_string.len()
    }
}

/// Base type information (without things like pointers, arrays, etc.)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BaseTypeInfo {
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
    /// Anonymous type at offset of .debug_info
    Anonymous(usize),
    /// A named type
    Named(String),
}
impl std::fmt::Display for BaseTypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => write!(f, "void"),
            Self::Bool => write!(f, "bool"),
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::U128 => write!(f, "u128"),
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::I128 => write!(f, "i128"),
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
            Self::Anonymous(off) => write!(f, "\"anonymous_0x{off:08x}\""),
            Self::Named(name) => write!(f, "\"{}\"", name),
        }
    }
}


impl std::fmt::Display for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeInfo::Void => write!(f, "void"),
            TypeInfo::Bool => write!(f, "bool"),
            TypeInfo::U8 => write!(f, "u8"),
            TypeInfo::U16 => write!(f, "u16"),
            TypeInfo::U32 => write!(f, "u32"),
            TypeInfo::U64 => write!(f, "u64"),
            TypeInfo::U128 => write!(f, "u128"),
            TypeInfo::I8 => write!(f, "i8"),
            TypeInfo::I16 => write!(f, "i16"),
            TypeInfo::I32 => write!(f, "i32"),
            TypeInfo::I64 => write!(f, "i64"),
            TypeInfo::I128 => write!(f, "i128"),
            TypeInfo::F32 => write!(f, "f32"),
            TypeInfo::F64 => write!(f, "f64"),
            TypeInfo::Anonymous(off) => write!(f, "\"anonymous_0x{off:08x}\""),
            TypeInfo::Named(name) => write!(f, "\"{}\"", name),
            TypeInfo::Pointer(ty) => write!(f, "{},'*'", ty),
            TypeInfo::Array(ty, len) => write!(f, "{},{}", ty, len),
            TypeInfo::Subroutine(ret, args) => {
                write!(f, "{},[", ret)?;
                if let Some(arg) = args.first() {
                    write!(f, "{}", arg)?;
                    for arg in &args[1..] {
                        write!(f, ",{}", arg)?;
                    }
                }
                write!(f, "]")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructMember {
    pub offset: usize,
    pub arg: Arg,
}

impl std::fmt::Display for StructMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:x}: {}", self.offset, self.arg)
    }
}
/// Information about an address, which could be:
/// - A decompiled function
/// - A data symbol
/// - A non-decompiled function
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AddressInfo {
    /// The address
    pub address: u64,
    /// If the address contains a function
    pub is_func: bool,
    /// Name of the symbol, either mangled or plain
    pub name: String,
    /// Type of the data symbol or return type of the function
    ///
    /// For non-decompiled function, this will be `None`
    pub ty: Option<TypeInfo>,
    /// Arguments of the function
    ///
    /// For non-decompiled function and data symbol, this will be empty
    pub args: Vec<Arg>,
}

impl AddressInfo {}

impl std::fmt::Display for AddressInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "  x{:x}:", self.address)?;
        let t = if self.is_func { "func" } else { "data" };
        writeln!(f, "    {}: \"{}\"", t, self.name)?;
        if let Some(ty) = &self.ty {
            writeln!(f, "    ty: [{}]", ty)?;
        }
        if self.is_func {
            writeln!(f, "    args:")?;
            for arg in &self.args {
                writeln!(f, "    - {}", arg)?;
            }
        }

        Ok(())
    }
}

/// A function argument or struct member (Type with a Name)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Arg {
    /// The name of the argument
    pub name: String,
    /// The type of the argument
    pub ty: TypeInfo,
}

impl std::fmt::Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ name: \"{}\", ty: [{}] }}", self.name, self.ty)
    }
}
