mod address;
pub use address::*;
mod namespace;
pub use namespace::*;
mod type_def;
pub use type_def::*;
mod type_info;
pub use type_info::*;
mod type_name;
pub use type_name::*;
mod type_prim;
pub use type_prim::*;
// mod type_resolve;
// pub use type_resolve::*;
mod type_resolve2;
pub use type_resolve2::*;
mod type_merger;
pub use type_merger::*;
mod type_gc;
pub use type_gc::*;
// mod type_store;
// pub use type_store::*;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("The base type for a PTMF must be a named type")]
    UnexpectedUnnamedPtmfThis,
    #[error("Failed to resolve names")]
    ResolveName,
    #[error("Unresolved name")]
    UnresolvedName,
    #[error("Unresolved size")]
    UnresolvedSize,
    #[error("Types with the same name have different definitions")]
    ConflictingType,
    #[error("Namespace not found: 0x{0:08x}")]
    UnlinkedNamespace(usize),
    #[error("Type not found: 0x{0:08x}")]
    UnlinkedType(usize),
    #[error("Type `{0}` is referenced but not actually generated")]
    BrokenTypeRef(String),
    #[error("Invalid member layout for struct")]
    InvalidLayout,
}
