use std::collections::BTreeMap;

use error_stack::{report, Result, ResultExt};

use crate::parsed::TypeError;

use super::{Offset, TypeDef, TypeYaml, TypesStage1, TypesStage2, TypesStage6};

/// Information about symbol at an address, linked to type offsets in DWARF
#[derive(Clone, Debug)]
pub struct AddressInfo {
    /// The address in the game binary
    pub uking_address: u64,
    /// Name of the symbol
    pub name: String,
    /// Address info
    pub info: AddrType,
}

/// Definition of the a symbol at an address
#[derive(Clone, Debug)]
pub struct AddressDef {
    /// The address in the game binary
    pub uking_address: u64,
    /// Name of the symbol
    pub name: String,
    /// If the symbol is a function
    pub is_func: bool,
    /// Return type or data type of the symbol
    pub ty_yaml: Option<String>,
    /// Arguments of the symbol, if it is a function (name, type)
    pub args: Vec<(Option<String>, Option<String>)>,
}

impl AddressInfo {
    pub fn into_def(
        self,
        types: &TypesStage6,
        defs: &BTreeMap<String, TypeDef>,
    ) -> Result<AddressDef, TypeError> {
        let mut referenced_names = Vec::new();
        let (is_func, ty_yaml, args) = match self.info {
            AddrType::Undecompiled => (true, None, vec![]),
            AddrType::Data(info) => match info.ty_offset {
                Some(ty) => {
                    let ty = types.get_name(&ty);
                    for name in ty.referenced_names() {
                        referenced_names.push(name);
                    }
                    (false, Some(ty.yaml_string()), vec![])
                }
                None => (false, None, vec![]),
            },
            AddrType::Func(info) => {
                let ty = types.get_name(&info.ret_ty_offset);
                for name in ty.referenced_names() {
                    referenced_names.push(name);
                }
                let ty_yaml = ty.yaml_string();
                let mut args = Vec::new();
                for (name, ty) in info.args {
                    let ty = match ty {
                        Some(ty) => {
                            let ty = types.get_name(&ty);
                            for name in ty.referenced_names() {
                                referenced_names.push(name);
                            }
                            Some(ty.yaml_string())
                        }
                        None => None,
                    };
                    args.push((name, ty));
                }
                (true, Some(ty_yaml), args)
            }
        };

        for name in referenced_names {
            if !defs.contains_key(&name) {
                let r = report!(TypeError::BrokenTypeRef(name.clone()))
                    .attach_printable(format!("While creating definition for {}", self.name));
                return Err(r);
            }
        }

        Ok(AddressDef {
            uking_address: self.uking_address,
            name: self.name,
            is_func,
            ty_yaml,
            args,
        })
    }
}

impl std::fmt::Display for AddressInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:016x} {} {}", self.uking_address, self.name, self.info)
    }
}

impl TypeYaml for AddressDef {
    fn yaml_string(&self) -> String {
        let addr = self.uking_address & 0xFFFFFFFF;
        let mut s = format!("  '0x{:08x}':", addr);
        let tag = if self.is_func { "func" } else { "data" };
        if self.ty_yaml.is_none() && self.args.is_empty() {
            s.push_str(&format!(" {{ {tag}: '{}' }}\n", self.name));
            return s;
        }
        s.push_str("\n");
        s.push_str(&format!("    {tag}: '{}'\n", self.name));
        if let Some(t) = &self.ty_yaml {
            s.push_str(&format!("    type: [ {} ] \n", t));
        }
        if self.is_func {
            if !self.args.is_empty() {
                s.push_str("    args:\n");
                for (name, ty) in &self.args {
                    s.push_str("      - { ");
                    if let Some(name) = name {
                        s.push_str(&format!("name: '{}', ", name));
                    }
                    if let Some(ty) = ty {
                        s.push_str(&format!("type: [ {} ]", ty));
                    }
                    s.push_str(" }\n");
                }
            }
        }
        s
    }
}

#[derive(Clone, Debug)]
pub enum AddrType {
    Undecompiled,
    Func(FuncInfo),
    Data(DataInfo),
}

impl std::fmt::Display for AddrType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AddrType::Undecompiled => write!(f, "undecompiled"),
            AddrType::Func(info) => write!(f, "func{}", info),
            AddrType::Data(info) => write!(f, "data{}", info),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FuncInfo {
    pub ret_ty_offset: Offset,
    pub args: Vec<(Option<String>, Option<Offset>)>,
}
impl std::fmt::Display for FuncInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}>)(", self.ret_ty_offset)?;
        let mut iter = self.args.iter();
        if let Some((name, ty)) = iter.next() {
            if let Some(name) = name {
                write!(f, "{}: ", name)?;
            }
            if let Some(ty) = ty {
                write!(f, "{}", ty)?;
            } else {
                write!(f, "<unknown>")?;
            }
        }
        for (name, ty) in iter {
            write!(f, ", ")?;
            if let Some(name) = name {
                write!(f, "{}: ", name)?;
            }
            if let Some(ty) = ty {
                write!(f, "{}", ty)?;
            } else {
                write!(f, "<unknown>")?;
            }
        }
        write!(f, ")")
    }
}
#[derive(Clone, Debug)]
pub struct DataInfo {
    pub ty_offset: Option<Offset>,
}
impl std::fmt::Display for DataInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty_offset {
            Some(ty) => write!(f, "{}", ty),
            None => write!(f, "<unknown>"),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum AddressInfoMismatch {
    #[error("Name mismatch")]
    NameMismatch,
    #[error("Address type mismatch")]
    TypeMismatch,
    #[error("Function return type mismatch")]
    RetTypeMismatch,
    #[error("Function argument count mismatch")]
    ParamCountMismatch,
    #[error("Function argument {0} type mismatch")]
    ParamTypeMismatch(usize),
    #[error("Function argument {0} name mismatch")]
    ParamNameMismatch(usize),
    #[error("Data type mismatch")]
    DataTypeMismatch,
}

impl AddressInfo {
    pub fn check_and_merge(
        &mut self,
        other: AddressInfo,
        types: &mut TypesStage1,
    ) -> Result<(), AddressInfoMismatch> {
        // other wouldn't have address so we don't check
        if self.name != other.name {
            return Err(report!(AddressInfoMismatch::NameMismatch))
                .attach_printable(format!("self: {} != other: {}", self.name, other.name));
        }

        match (&mut self.info, &other.info) {
            (AddrType::Func(a), AddrType::Func(b)) => a.check_and_merge(b, types),
            (AddrType::Data(a), AddrType::Data(b)) => a.check_and_merge(b, types),
            _ => {
                return Err(report!(AddressInfoMismatch::TypeMismatch))
                    .attach_printable(format!("self: {} != other: {}", self.info, other.info));
            }
        }
    }

    pub fn mark_types(&self, types: &mut TypesStage2) {
        match &self.info {
            AddrType::Func(info) => info.mark_types(types),
            AddrType::Data(info) => info.mark_types(types),
            _ => {}
        }
    }
}

impl FuncInfo {
    /// Update self with other if any info is missing. Return false if there are inconsistencies
    pub fn check_and_merge(
        &mut self,
        other: &FuncInfo,
        types: &mut TypesStage1,
    ) -> Result<(), AddressInfoMismatch> {
        if self.args.len() != other.args.len() {
            return Err(report!(AddressInfoMismatch::ParamCountMismatch)).attach_printable(
                format!("self: {} != other: {}", self.args.len(), other.args.len()),
            );
        }
        types
            .check_and_merge(&self.ret_ty_offset, &other.ret_ty_offset)
            .change_context(AddressInfoMismatch::RetTypeMismatch)?;
        for (i, (a, b)) in self.args.iter_mut().zip(&other.args).enumerate() {
            match (&mut a.0, &b.0) {
                (Some(a), Some(b)) => {
                    if a != b {
                        return Err(report!(AddressInfoMismatch::ParamNameMismatch(i)))
                            .attach_printable(format!("self: {} != other: {}", a, b));
                    }
                }
                (None, Some(b)) => {
                    a.0.replace(b.clone());
                }
                _ => {}
            }
            match (&mut a.1, &b.1) {
                (Some(a), Some(b)) => {
                    types
                        .check_and_merge(a, b)
                        .change_context(AddressInfoMismatch::ParamTypeMismatch(i))?;
                }
                (None, Some(b)) => {
                    a.1.replace(*b);
                }
                _ => {}
            }
        }
        Ok(())
    }

    pub fn mark_types(&self, types: &mut TypesStage2) {
        types.mark(&self.ret_ty_offset);
        for (_, ty) in &self.args {
            if let Some(ty) = ty {
                types.mark(ty);
            }
        }
    }
}

impl DataInfo {
    pub fn check_and_merge(
        &mut self,
        other: &DataInfo,
        types: &mut TypesStage1,
    ) -> Result<(), AddressInfoMismatch> {
        match (&mut self.ty_offset, other.ty_offset) {
            (Some(a), Some(b)) => {
                types
                    .check_and_merge(a, &b)
                    .change_context(AddressInfoMismatch::DataTypeMismatch)?;
            }
            (None, Some(b)) => {
                self.ty_offset.replace(b);
            }
            _ => {}
        }
        Ok(())
    }

    pub fn mark_types(&self, types: &mut TypesStage2) {
        if let Some(ty) = self.ty_offset {
            types.mark(&ty);
        }
    }
}
