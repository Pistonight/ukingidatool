use error_stack::{report, Result, ResultExt};

use super::{Error, TypeStore};

#[derive(Clone, Debug)]
pub struct AddressInfo {
    pub uking_address: u64,
    pub name: String,
    pub info: AddrType,
}

impl std::fmt::Display for AddressInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:016x} {} {}", self.uking_address, self.name, self.info)
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
    pub ret_ty_offset: usize,
    pub args: Vec<(Option<String>, Option<usize>)>,
}
impl std::fmt::Display for FuncInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(<0x{:08x}>)(", self.ret_ty_offset)?;
        let mut iter = self.args.iter();
        if let Some((name, ty)) = iter.next() {
            if let Some(name) = name {
                write!(f, "{}: ", name)?;
            }
            if let Some(ty) = ty {
                write!(f, "<0x{:08x}>", ty)?;
            } else {
                write!(f, "<unknown>")?;
            }
        }
        for (name, ty) in iter {
            if let Some(name) = name {
                write!(f, "{}: ", name)?;
            }
            if let Some(ty) = ty {
                write!(f, "<0x{:08x}>", ty)?;
            } else {
                write!(f, "<unknown>")?;
            }
        }
        write!(f, ")")
    }
}
#[derive(Clone, Debug)]
pub struct DataInfo {
    pub ty_offset: Option<usize>,
}
impl std::fmt::Display for DataInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty_offset {
            Some(ty) => write!(f, "<0x{:08x}>", ty),
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
    #[error("Type error")]
    Type,
}

impl AddressInfo {
    pub fn check_and_merge(
        &mut self,
        other: AddressInfo,
        types: &TypeStore,
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

    pub fn mark_types(&self, types: &mut TypeStore) -> Result<(), Error> {
        match &self.info {
            AddrType::Func(info) => info.mark_types(types)?,
            AddrType::Data(info) => info.mark_types(types)?,
            _ => {}
        }
        Ok(())
    }
}

impl FuncInfo {
    /// Update self with other if any info is missing. Return false if there are inconsistencies
    pub fn check_and_merge(
        &mut self,
        other: &FuncInfo,
        types: &TypeStore,
    ) -> Result<(), AddressInfoMismatch> {
        if self.args.len() != other.args.len() {
            return Err(report!(AddressInfoMismatch::ParamCountMismatch)).attach_printable(
                format!("self: {} != other: {}", self.args.len(), other.args.len()),
            );
        }
        if !types
            .are_equal(self.ret_ty_offset, other.ret_ty_offset)
            .change_context(AddressInfoMismatch::Type)?
        {
            let a_bucket = types
                .get_bucket(self.ret_ty_offset)
                .map(|s| format!("0x{s:08x}"))
                .unwrap_or_else(|_| "<unknown>".to_string());
            let b_bucket = types
                .get_bucket(other.ret_ty_offset)
                .map(|s| format!("0x{s:08x}"))
                .unwrap_or_else(|_| "<unknown>".to_string());
            let a = types
                .get_name(self.ret_ty_offset)
                .map(|s| s.to_string())
                .unwrap_or_else(|_| "<unknown>".to_string());
            let b = types
                .get_name(other.ret_ty_offset)
                .map(|s| s.to_string())
                .unwrap_or_else(|_| "<unknown>".to_string());
            return Err(report!(AddressInfoMismatch::RetTypeMismatch))
                .attach_printable(format!("self: {a} != other: {b}"))
                .attach_printable(format!(
                    "self_bucket: {a_bucket} != other_bucket: {b_bucket}"
                ));
        }
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
                    if !types
                        .are_equal(*a, *b)
                        .change_context(AddressInfoMismatch::Type)?
                    {
                        let a_bucket = types
                            .get_bucket(self.ret_ty_offset)
                            .map(|s| format!("0x{s:08x}"))
                            .unwrap_or_else(|_| "<unknown>".to_string());
                        let b_bucket = types
                            .get_bucket(other.ret_ty_offset)
                            .map(|s| format!("0x{s:08x}"))
                            .unwrap_or_else(|_| "<unknown>".to_string());
                        let a = types
                            .get_name(self.ret_ty_offset)
                            .map(|s| s.to_string())
                            .unwrap_or_else(|_| "<unknown>".to_string());
                        let b = types
                            .get_name(other.ret_ty_offset)
                            .map(|s| s.to_string())
                            .unwrap_or_else(|_| "<unknown>".to_string());
                        return Err(report!(AddressInfoMismatch::ParamTypeMismatch(i)))
                            .attach_printable(format!("self: {a} != other: {b}"))
                            .attach_printable(format!(
                                "self_bucket: {a_bucket} != other_bucket: {b_bucket}"
                            ));
                    }
                }
                (None, Some(b)) => {
                    a.1.replace(*b);
                }
                _ => {}
            }
        }
        Ok(())
    }

    pub fn mark_types(&self, types: &mut TypeStore) -> Result<(), Error> {
        types.mark_referenced(self.ret_ty_offset)?;
        for (_, ty) in &self.args {
            if let Some(ty) = ty {
                types.mark_referenced(*ty)?;
            }
        }
        Ok(())
    }
}

impl DataInfo {
    pub fn check_and_merge(
        &mut self,
        other: &DataInfo,
        types: &TypeStore,
    ) -> Result<(), AddressInfoMismatch> {
        match (&mut self.ty_offset, other.ty_offset) {
            (Some(a), Some(b)) => {
                if !types
                    .are_equal(*a, b)
                    .change_context(AddressInfoMismatch::Type)?
                {
                    let a_bucket = types
                        .get_bucket(*a)
                        .map(|s| format!("0x{s:08x}"))
                        .unwrap_or_else(|_| "<unknown>".to_string());
                    let b_bucket = types
                        .get_bucket(b)
                        .map(|s| format!("0x{s:08x}"))
                        .unwrap_or_else(|_| "<unknown>".to_string());
                    let a = types
                        .get_name(*a)
                        .map(|s| s.to_string())
                        .unwrap_or_else(|_| "<unknown>".to_string());
                    let b = types
                        .get_name(b)
                        .map(|s| s.to_string())
                        .unwrap_or_else(|_| "<unknown>".to_string());
                    return Err(report!(AddressInfoMismatch::DataTypeMismatch))
                        .attach_printable(format!("self: {a} != other: {b}"))
                        .attach_printable(format!(
                            "self_bucket: {a_bucket} != other_bucket: {b_bucket}"
                        ));
                }
            }
            (None, Some(b)) => {
                self.ty_offset.replace(b);
            }
            _ => {}
        }
        Ok(())
    }

    pub fn mark_types(&self, types: &mut TypeStore) -> Result<(), Error> {
        if let Some(ty) = self.ty_offset {
            types.mark_referenced(ty)?;
        }
        Ok(())
    }
}
