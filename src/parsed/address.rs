

#[derive(Clone, Debug)]
pub struct AddressInfo {
    pub uking_address: u64,
    pub name: String,
    pub info: AddrType,
}

#[derive(Clone, Debug)]
pub enum AddrType {
    Undecompiled,
    Func(FuncInfo),
    Data(DataInfo),
}

#[derive(Clone, Debug)]
pub struct FuncInfo {
    pub ret_ty_offset: usize,
    pub args: Vec<(Option<String>, Option<usize>)>,
}
#[derive(Clone, Debug)]
pub struct DataInfo {
    pub ty_offset: Option<usize>,
}

impl AddressInfo {
    pub fn check_and_merge(&mut self, other: AddressInfo) -> bool {
        // other wouldn't have address so we don't check
        if self.name != other.name {
            return false;
        }

        match (&mut self.info, other.info) {
            (AddrType::Func(a), AddrType::Func(b)) => a.check_and_merge(b),
            (AddrType::Data(a), AddrType::Data(b)) => a.check_and_merge(b),
            _ => false,
        }
    }
}

impl FuncInfo {
    /// Update self with other if any info is missing. Return false if there are inconsistencies
    pub fn check_and_merge(&mut self, other: FuncInfo) -> bool {
        if self.args.len() != other.args.len() {
            return false;
        }
        if self.ret_ty_offset != other.ret_ty_offset {
            return false;
        }
        for (a, b) in self.args.iter_mut().zip(other.args) {
            match (&mut a.0, b.0) {
                (Some(a), Some(b)) => {
                    if a != &b {
                        return false;
                    }
                }
                (None, Some(b)) => {
                    a.0.replace(b);
                }
                _ => {}
            }
            match (&mut a.1, &b.1) {
                (Some(a), Some(b)) => {
                    if a != b {
                        return false;
                    }
                }
                (None, Some(b)) => {
                    a.1.replace(*b);
                }
                _ => {}
            }
        }
        true
    }
}

impl DataInfo {
    pub fn check_and_merge(&mut self, other: DataInfo) -> bool {
        if self.ty_offset != other.ty_offset {
            return false;
        }
        true
    }
}
