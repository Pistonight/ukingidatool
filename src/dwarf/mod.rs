#![allow(non_upper_case_globals)]

use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::path::Path;

use elf::ElfBytes;
use error_stack::{Report, Result, ResultExt};
use gimli::{
    AttributeValue, DW_TAG_GNU_template_parameter_pack, DW_TAG_enumerator,
    DW_TAG_template_type_parameter, DW_TAG_variable, DW_TAG_volatile_type, DwAt, DwTag,
    DwarfFileType, EndianSlice,
};
use gimli::{
    DW_ATE_boolean, DW_ATE_float, DW_ATE_signed, DW_ATE_signed_char, DW_ATE_unsigned,
    DW_ATE_unsigned_char, DW_AT_byte_size, DW_AT_encoding, DW_TAG_array_type, DW_TAG_base_type,
    DW_TAG_class_type, DW_TAG_compile_unit, DW_TAG_const_type, DW_TAG_enumeration_type,
    DW_TAG_formal_parameter, DW_TAG_inheritance, DW_TAG_member, DW_TAG_namespace,
    DW_TAG_pointer_type, DW_TAG_ptr_to_member_type, DW_TAG_reference_type, DW_TAG_structure_type,
    DW_TAG_subprogram, DW_TAG_subrange_type, DW_TAG_subroutine_type,
    DW_TAG_template_value_parameter, DW_TAG_typedef, DW_TAG_union_type, DW_TAG_unspecified_type,
    DW_ATE_UTF,
};

use crate::parsed::{
    AddrType, AddressInfo, DataInfo, EnumInfo, FuncInfo, MemberInfo, Namespace, StructInfo,
    TypeComp, TypeInfo, TypePrim, TypeStore, TypeYaml, UnionInfo,
};
use crate::util::{self, ProgressPrinter};

mod entry_integer;
mod entry_name;
mod entry_subprogram;
mod entry_type;
mod unit;
use unit::{bad, err_ctx, opt_ctx, UnitCtx};

pub type In<'i> = EndianSlice<'i, gimli::LittleEndian>;
pub type Unit<'i> = gimli::Unit<In<'i>>;
pub type UnitHeader<'i> = gimli::UnitHeader<In<'i>>;
pub type Tree<'i, 'a, 'u> = gimli::EntriesTree<'a, 'u, In<'i>>;
pub type Node<'i, 'a, 'u, 't> = gimli::EntriesTreeNode<'a, 'u, 't, In<'i>>;
pub type Dwarf<'i> = gimli::Dwarf<In<'i>>;
pub type UnitOffset = gimli::UnitOffset<usize>;
pub type DIE<'i, 'a, 'u> = gimli::DebuggingInformationEntry<'a, 'u, In<'i>, usize>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Failed to read ELF file `{0}`")]
    ReadElf(String),
    #[error("Failed to parse ELF")]
    ParseElf,
    #[error("Failed to read header for section `{0}`")]
    SectionHeader(String),
    #[error("Failed to read unit header")]
    ReadUnitHeader,
    #[error("Failed to create unit")]
    CreateUnit,
    #[error("Failed to create unit abbreviations")]
    CreateUnitAbbrev,
    #[error("Failed to create unit, expecting a DebugInfoOffset")]
    CreateUnitOffset,

    #[error("Failed to create entry tree")]
    CreateTree,
    #[error("Failed to read root of entry tree")]
    ReadRoot,
    #[error("Failed to read child entry")]
    ReadChild,
    #[error("Expecting a child entry, but none found")]
    ExpectingChild,
    #[error("Failed to read entry in unit")]
    ReadEntry,
    #[error("Failed to read DIE attribute `{0}`")]
    ReadEntryAttr(DwAt),
    #[error("Expected DIE attribute `{0}`, but it's not found")]
    MissingEntryAttr(DwAt),
    #[error("Unexpected DIE attribute `{0}`")]
    UnexpectedEntryAttr(DwAt),
    #[error("DIE attribute `{0}` should have type `{1}`")]
    BadEntryAttrType(DwAt, &'static str),
    #[error("Unable to understand the combination of DIE attributes `{0}`")]
    BadEntryAttrTypes(String),

    #[error("Unexpected tag `{0}`")]
    UnexpectedTag(DwTag),
    #[error("Unexpected tag `{0}` for DIE, expecting tag for a type")]
    UnexpectedTagForType(DwTag),
    #[error("Unexpected base type for enumeration")]
    UnexpectedEnumType,
    #[error("Declaration cannot be anonymous")]
    UnexpectedAnonymousDeclaration,
    #[error("Unexpected virtual union")]
    UnexpectedVirtualUnion,
    #[error("Unexpected type name for DW_TAG_unspecified_type")]
    UnspecifiedType,
    #[error("Failed to get namespace for DIE")]
    Namespace,
    // #[error("Failed to get base type of struct/class")]
    // InvalidBaseType,
    #[error("Hole in vtable")]
    MissingVtableEntry,
    #[error("Conflicting entry in vtable")]
    ConflictingVtableEntry,
    #[error("Conflicting address for functions")]
    ConflictingAddress,
    #[error("Conflicting name for functions")]
    ConflictingName,
    #[error("Conflicting type for functions")]
    ConflictingInfo,
    // #[error("Subprogram with DW_AT_abstract_origin does not have a DW_AT_low_pc")]
    // NoAddressInAbstractOrigin,
    // #[error("Subprogram with DW_AT_specification does not have a DW_AT_low_pc")]
    // NoAddressInSpecification,
    // #[error("DIE does not have a linkage name, abstract origin, or specification")]
    // NoLinkageName,
    #[error("DW_TAG_variable doesn't have a DW_AT_specification")]
    NoSpecificationInVariable,

    #[error("Failed to read string attribute")]
    AttrString,
    #[error("Failed to read address attribute")]
    AttrAddress,

    #[error("{0} at 0x{1:08x}")]
    Ctx(&'static str, usize),

    #[error("Failed to deduplicate/resolve types")]
    ResolveType,
    #[error("Failed to garbage collect types")]
    GarbageCollectType,
    #[error("Failed to create type definitions")]
    CreateTypeDefinition,

    #[error("Failed to write types.yaml")]
    WriteFile,
}

macro_rules! process_units {
    ($units:ident, $desc:literal, $unit:ident, $root:ident, $block:block) => {{
        let progress = ProgressPrinter::new($units.len(), $desc);
        for (i, $unit) in $units.iter().enumerate() {
            progress.print(i, $unit.name);
            let mut tree = $unit.tree()?;
            let $root = err_ctx!($unit, Error::ReadRoot, tree.root())?;
            $block
        }
        progress.done();
    }};
}

pub fn extract(
    elf_path: &Path,
    output_path: impl AsRef<Path>,
    uking_symbols: &mut BTreeMap<String, u64>,
    uking_data_symbols: &BTreeSet<String>,
) -> Result<(), Error> {
    println!("Extracting DAWRF from ELF {}", elf_path.display());
    let uking_elf = util::read_as_bytes(elf_path)
        .change_context_lazy(|| Error::ReadElf(elf_path.display().to_string()))?;
    let file = ElfBytes::<elf::endian::LittleEndian>::minimal_parse(&uking_elf)
        .change_context(Error::ParseElf)?;

    let mut dwarf = gimli::Dwarf::load(|section| {
        let header = file
            .section_header_by_name(section.name())
            .change_context_lazy(|| Error::SectionHeader(section.name().to_string()))?;
        match header {
            Some(header) => {
                let start = header.sh_offset as usize;
                let end = start + header.sh_size as usize;
                let slice = EndianSlice::new(&uking_elf[start..end], gimli::LittleEndian);
                Ok::<_, Report<Error>>(slice)
            }
            None => Ok(EndianSlice::new(&[], gimli::LittleEndian)),
        }
    })?;
    dwarf.file_type = DwarfFileType::Main;

    println!("Processing DWARF...");

    let debug_info = dwarf.debug_info;

    let mut iter = debug_info.units();
    let mut units = Vec::new();
    let progress = ProgressPrinter::new(0, "Discover compile units");
    while let Some(unit_header) = iter.next().change_context(Error::ReadUnitHeader)? {
        let unit_ctx = UnitCtx::new(unit_header, &dwarf)?;
        progress.print(units.len(), unit_ctx.name);
        units.push(unit_ctx);
    }
    progress.done();

    // PASS 1 - namespace info
    let offset_to_ns = {
        let mut offset_to_ns = BTreeMap::new();
        let mut namespace = Namespace::default();
        process_units!(units, "Register namespaces", unit, root, {
            pass1(root, unit, &mut namespace, &mut offset_to_ns)?;
        });
        offset_to_ns
    };
    // PASS 2 - type info
    let offset_to_ty = {
        let mut offset_to_ty = BTreeMap::new();
        process_units!(units, "Register types", unit, root, {
            node_typeinfo_pass2(root, unit, &offset_to_ns, &mut offset_to_ty)?;
        });
        offset_to_ty
    };
    let namespaces = offset_to_ns.into();
    let mut types =
        TypeStore::resolve(offset_to_ty, &namespaces).change_context(Error::ResolveType)?;

    // PASS 3 - symbols
    let data_types = {
        let mut data_types = BTreeMap::new();
        let mut addr_to_name = BTreeMap::new();
        process_units!(units, "Processing symbols", unit, root, {
            pass3(
                root,
                unit,
                uking_symbols,
                &mut addr_to_name,
                &mut data_types,
                &types,
            )?;
        });
        for (symbol, address) in std::mem::take(uking_symbols) {
            if data_types.contains_key(&symbol) {
                // already processed, not possible
                panic!(
                    "Symbol `{}` is already processed. This shouldn't be possible",
                    symbol
                );
            }
            if uking_data_symbols.contains(&symbol) {
                // data symbol
                data_types.insert(
                    symbol.clone(),
                    AddressInfo {
                        uking_address: address,
                        name: symbol,
                        info: AddrType::Data(DataInfo { ty_offset: None }),
                    },
                );
            } else {
                // function symbol
                data_types.insert(
                    symbol.clone(),
                    AddressInfo {
                        uking_address: address,
                        name: symbol,
                        info: AddrType::Undecompiled,
                    },
                );
            }
        }
        data_types
    };
    // Type GC
    {
        let progress = ProgressPrinter::new(data_types.len(), "Garbage collect types");
        for (i, (name, info)) in data_types.iter().enumerate() {
            progress.print(i, name);
            info.mark_types(&mut types)
                .change_context(Error::GarbageCollectType)?;
        }
        progress.done();
    }
    // Type Output
    let type_defs = types
        .create_defs()
        .change_context(Error::CreateTypeDefinition)?;
    println!("Extracted {} types", type_defs.len());
    let mut type_yaml = String::new();
    type_yaml.push_str("enums:\n");
    for enum_def in type_defs.values().filter_map(|x| x.as_enum()) {
        type_yaml.push_str(&enum_def.yaml_string());
    }
    type_yaml.push_str("unions:\n");
    for union_def in type_defs.values().filter_map(|x| x.as_union()) {
        type_yaml.push_str(&union_def.yaml_string());
    }
    type_yaml.push_str("structs:\n");
    for struct_def in type_defs.values().filter_map(|x| x.as_struct()) {
        type_yaml.push_str(&struct_def.yaml_string());
    }

    // Write Output
    let output_path_str = output_path.as_ref().display().to_string();
    std::fs::write(output_path, type_yaml).change_context(Error::WriteFile)?;
    println!("Output written to {}", output_path_str);

    Ok(())
}

/////////////// PASS 1 ///////////////
// Create offset -> namespace map

macro_rules! namespace_scope {
    ($namespace:ident, $name:expr, $block:block) => {{
        match $name {
            Some(name) => {
                $namespace.push(name);
                $block
                $namespace.pop();
            }
            None => {
                $block
            }
        }
    }};
}

fn pass1<'d, 'i, 'a, 'u, 't>(
    node: Node<'i, 'a, 'u, 't>,
    unit: &UnitCtx<'d, 'i>,
    namespace: &mut Namespace<'i>,
    offset_to_ns: &mut BTreeMap<usize, Namespace<'i>>,
) -> Result<(), Error> {
    let entry = node.entry();
    match entry.tag() {
        DW_TAG_compile_unit => {
            // top-most case
            unit.for_each_child(node, |child| pass1(child, unit, namespace, offset_to_ns))?;
        }
        DW_TAG_structure_type
        | DW_TAG_class_type
        | DW_TAG_union_type
        | DW_TAG_enumeration_type
        | DW_TAG_typedef
        | DW_TAG_unspecified_type
        | DW_TAG_variable
        | DW_TAG_pointer_type
        | DW_TAG_reference_type
        | DW_TAG_const_type
        | DW_TAG_volatile_type
        | DW_TAG_array_type
        | DW_TAG_subroutine_type
        | DW_TAG_ptr_to_member_type
        | DW_TAG_base_type => {
            let offset = unit.to_global_offset(entry.offset());
            offset_to_ns.insert(offset, namespace.clone());
            // can have inner types, and may be anonymous
            let name = unit.get_entry_name_optional(entry)?;
            namespace_scope!(namespace, name, {
                unit.for_each_child(node, |child| pass1(child, unit, namespace, offset_to_ns))?;
            });
        }
        DW_TAG_subprogram => {
            let offset = unit.to_global_offset(entry.offset());
            offset_to_ns.insert(offset, namespace.clone());
            // subprogram can cause conflict, so we want a unique name
            let name = match unit.get_entry_name_optional(entry)? {
                Some(name) => Cow::Borrowed(name),
                None => Cow::Owned(format!("subprogram_0x{:08x}", offset)),
            };
            namespace_scope!(namespace, Some(name), {
                unit.for_each_child(node, |child| pass1(child, unit, namespace, offset_to_ns))?;
            });
        }
        DW_TAG_namespace => {
            let name = unit.get_entry_name_optional(entry)?;
            // ok to have anonymous namespaces
            namespace_scope!(namespace, name, {
                unit.for_each_child(node, |child| pass1(child, unit, namespace, offset_to_ns))?;
            });
        }
        _ => {
            // ignore
        }
    }

    Ok(())
}

///////////////  PASS 2 ///////////////
// Create offset -> type map

fn node_typeinfo_pass2<'d, 'i, 'a, 'u, 't>(
    node: Node<'i, 'a, 'u, 't>,
    unit: &UnitCtx<'d, 'i>,
    offset_to_ns: &BTreeMap<usize, Namespace<'i>>,
    offset_to_ty: &mut BTreeMap<usize, TypeInfo>,
) -> Result<(), Error> {
    let entry = node.entry();
    match entry.tag() {
        DW_TAG_structure_type
        | DW_TAG_class_type
        | DW_TAG_union_type
        | DW_TAG_enumeration_type
        | DW_TAG_unspecified_type
        | DW_TAG_typedef
        | DW_TAG_pointer_type
        | DW_TAG_reference_type
        | DW_TAG_const_type
        | DW_TAG_volatile_type
        | DW_TAG_array_type
        | DW_TAG_subroutine_type
        | DW_TAG_ptr_to_member_type
        | DW_TAG_base_type => {
            read_type_at_offset(entry.offset(), unit, offset_to_ns, offset_to_ty)?;
            if !offset_to_ty.contains_key(&unit.to_global_offset(entry.offset())) {
                panic!(
                    "Failed to read type at offset 0x{:08x}",
                    unit.to_global_offset(entry.offset())
                );
            }
        }
        _ => {}
    }
    unit.for_each_child(node, |child| {
        node_typeinfo_pass2(child, unit, offset_to_ns, offset_to_ty)
    })?;

    Ok(())
}

/////////////// PASS 3 ///////////////
// Create offset -> address symbol map (function and data)

fn pass3<'d, 'i, 'a, 'u, 't>(
    node: Node<'i, 'a, 'u, 't>,
    unit: &UnitCtx<'d, 'i>,
    uking_symbols: &mut BTreeMap<String, u64>,
    elf_addr_to_name: &mut BTreeMap<u64, String>,
    data_type: &mut BTreeMap<String, AddressInfo>,
    types: &TypeStore,
) -> Result<(), Error> {
    let entry = node.entry();
    match entry.tag() {
        DW_TAG_subprogram => {
            read_subprogram(
                &entry,
                unit,
                uking_symbols,
                elf_addr_to_name,
                data_type,
                None,
                types,
            )?;
        }
        DW_TAG_variable => {
            read_variable(&entry, unit, uking_symbols, data_type, types)?;
        }
        _ => {}
    }
    unit.for_each_child(node, |child| {
        pass3(
            child,
            unit,
            uking_symbols,
            elf_addr_to_name,
            data_type,
            types,
        )
    })?;

    Ok(())
}

fn read_subprogram<'d, 'i, 'a, 'u>(
    entry: &DIE<'i, 'a, 'u>,
    unit: &UnitCtx<'d, 'i>,
    uking_symbols: &mut BTreeMap<String, u64>,
    elf_addr_to_name: &mut BTreeMap<u64, String>,
    data_type: &mut BTreeMap<String, AddressInfo>,
    input_elf_addr: Option<u64>,
    types: &TypeStore,
) -> Result<(), Error> {
    let offset = entry.offset();
    if let Some(linkage_name) = unit.get_entry_linkage_name(&entry)? {
        let addr = match unit.get_entry_low_pc(entry)? {
            Some(addr) => {
                // make sure it's not conflicting with the input elf address
                if let Some(y) = input_elf_addr {
                    if y != addr {
                        return bad!(
                            unit,
                            unit.to_global_offset(offset),
                            Error::ConflictingAddress
                        )
                        .attach_printable(format!("Function `{}`", linkage_name))
                        .attach_printable(format!("0x{:08x} != 0x{:08x}", y, addr));
                    }
                }
                addr
            }
            None => match input_elf_addr {
                Some(addr) => addr,
                None => {
                    // we don't have address to process this entry yet, will come back
                    return Ok(());
                }
            },
        };
        // produce AddressInfo
        // return type
        let ret_ty = match unit.get_entry_type_offset_optional(&entry)? {
            None => usize::MAX, // void
            Some(x) => unit.to_global_offset(x),
        };
        let mut args = Vec::new();
        let mut tree = unit.tree_at(entry.offset())?;
        let node = unit.root_of(entry.offset(), &mut tree)?;
        unit.for_each_child(node, |child| {
            let entry = child.entry();
            match entry.tag() {
                DW_TAG_formal_parameter => {
                    let name = unit.get_entry_name_optional(entry)?.map(|x| x.to_string());
                    let type_offset = unit
                        .get_entry_type_offset_optional(entry)?
                        .map(|x| unit.to_global_offset(x));
                    args.push((name, type_offset));
                }
                _ => {
                    // ignore subtypes
                }
            }
            Ok(())
        })?;
        let func_info = FuncInfo {
            ret_ty_offset: ret_ty,
            args,
        };
        let addr_info = AddressInfo {
            uking_address: 0,
            name: linkage_name.to_string(),
            info: AddrType::Func(func_info),
        };
        // address 0 are not real
        if addr != 0 {
            if let Some(old_name) = elf_addr_to_name.insert(addr, linkage_name.to_string()) {
                if old_name != linkage_name {
                    return bad!(unit, unit.to_global_offset(offset), Error::ConflictingName)
                        .attach_printable(format!("Function `{}`", linkage_name))
                        .attach_printable(format!(
                            "0x{:08x} is already assigned to `{}`",
                            addr, old_name
                        ));
                }
            }
        }
        // if there's some old info, merge it
        if let Some(old_info) = data_type.get_mut(linkage_name) {
            let check = old_info.check_and_merge(addr_info.clone(), types);
            err_ctx!(
                unit,
                unit.to_global_offset(offset),
                Error::ConflictingInfo,
                check
            )
            .attach_printable(format!("Function `{}`", linkage_name))
            .attach_printable(format!("Old Info: {}", old_info))
            .attach_printable(format!("New Info: {}", addr_info))?;
        } else {
            if let Some(uking_address) = uking_symbols.remove(linkage_name) {
                let mut addr_info = addr_info;
                addr_info.uking_address = uking_address;
                data_type.insert(linkage_name.to_string(), addr_info);
            }
            // if there's no uking_address, it's not a symbol that we care
        }
        return Ok(());
    }
    // no linkage_name, use other info to find the function name
    if let Some(abstract_origin) = unit.get_entry_abstract_origin(entry)? {
        let origin_entry = unit.entry_at(abstract_origin)?;
        let addr = match unit.get_entry_low_pc(&entry)? {
            None => {
                // not enough info, skip
                return Ok(());
            }
            Some(addr) => addr,
        };
        return read_subprogram(
            &origin_entry,
            unit,
            uking_symbols,
            elf_addr_to_name,
            data_type,
            Some(addr),
            types,
        );
    }
    if let Some(specification) = unit.get_entry_specification(entry)? {
        let origin_entry = unit.entry_at(specification)?;
        let addr = match unit.get_entry_low_pc(&entry)? {
            None => {
                // not enough info, skip
                return Ok(());
            }
            Some(addr) => addr,
        };
        return read_subprogram(
            &origin_entry,
            unit,
            uking_symbols,
            elf_addr_to_name,
            data_type,
            Some(addr),
            types,
        );
    }
    // ones that don't have name shouldn't matter
    // let external = unit.get_entry_external(entry)?;
    // let inlined = unit.get_entry_inlined(entry)?;
    //
    // if !(external  || inlined){
    //     return bad!(unit, unit.to_global_offset(offset), Error::NoLinkageName);
    // }

    Ok(())
}

fn read_variable<'d, 'i, 'a, 'u>(
    entry: &DIE<'i, 'a, 'u>,
    unit: &UnitCtx<'d, 'i>,
    uking_symbols: &mut BTreeMap<String, u64>,
    data_type: &mut BTreeMap<String, AddressInfo>,
    types: &TypeStore,
) -> Result<(), Error> {
    if let Some(linkage_name) = unit.get_entry_linkage_name(entry)? {
        let ty_offset = {
            match unit.get_entry_type_offset_optional(entry)? {
                Some(x) => x,
                None => {
                    // try specification
                    let specification = unit.get_entry_specification(entry)?;
                    let specification = opt_ctx!(
                        unit,
                        unit.to_global_offset(entry.offset()),
                        Error::NoSpecificationInVariable,
                        specification
                    )?;
                    let spec_entry = unit.entry_at(specification)?;
                    unit.get_entry_type_offset(&spec_entry)?
                }
            }
        };

        let data_info = DataInfo {
            ty_offset: Some(unit.to_global_offset(ty_offset)),
        };
        let addr_info = AddressInfo {
            uking_address: 0,
            name: linkage_name.to_string(),
            info: AddrType::Data(data_info),
        };
        // if there's some old info, merge it
        if let Some(old_info) = data_type.get_mut(linkage_name) {
            let check = old_info.check_and_merge(addr_info.clone(), types);
            err_ctx!(
                unit,
                unit.to_global_offset(entry.offset()),
                Error::ConflictingInfo,
                check
            )
            .attach_printable(format!("Data `{}`", linkage_name))
            .attach_printable(format!("Old Info: {}", old_info))
            .attach_printable(format!("New Info: {}", addr_info))?;
        } else {
            if let Some(uking_address) = uking_symbols.remove(linkage_name) {
                let mut addr_info = addr_info;
                addr_info.uking_address = uking_address;
                data_type.insert(linkage_name.to_string(), addr_info);
            }
            // if there's no uking_address, it's not a symbol that we care
        }
    }

    // ignore no linkage name
    Ok(())
}

/////////////// Helpers ///////////////

// /// Read the DW_AT_type of a DIE.
// fn read_type<'d, 'i, 'a, 'u>(
//     entry: &DIE<'i, 'a, 'u>,
//     unit: &UnitCtx<'d, 'i>,
//     offset_to_ns: &HashMap<usize, Namespace<'i>>,
//     offset_to_info: &mut HashMap<usize, TypeInfo>,
// ) -> Result<TypeInfo, Error> {
//     let type_offset = match unit.get_entry_type_offset_optional(entry)? {
//         None => return Ok(TypeInfo::Prim(TypePrim::Void)),
//         Some(x) => x,
//     };
//     let global_offset = unit.to_global_offset(type_offset);
//     // cached typeinfo from offset
//     if let Some(info) = offset_to_info.get(&global_offset) {
//         return Ok(info.clone());
//     }
//
//     let tyinfo = read_type_at_offset(type_offset, unit, offset_to_ns, offset_to_info)?;
//     offset_to_info.insert(global_offset, tyinfo.clone());
//     Ok(tyinfo)
// }

// fn read_type_optional<'d, 'i, 'a, 'u>(
//     entry: &DIE<'i, 'a, 'u>,
//     unit: &UnitCtx<'d, 'i>,
//     offset_to_ns: &HashMap<usize, Namespace<'i>>,
//     offset_to_info: &mut HashMap<usize, TypeInfo>,
// ) -> Result<Option<(UnitOffset, TypeInfo)>, Error> {
//     let type_offset = unit.get_entry_type_offset_optional(entry)?;
//     match type_offset {
//         None => Ok(None),
//         Some(type_offset) => {
//             let global_offset = unit.to_global_offset(type_offset);
//             // cached typeinfo from offset
//             if let Some(info) = offset_to_info.get(&global_offset) {
//                 return Ok(Some((type_offset, info.clone())));
//             }
//
//             let tyinfo = read_type_at_offset(type_offset, unit, offset_to_ns, offset_to_info)?;
//             offset_to_info.insert(global_offset, tyinfo.clone());
//             Ok(Some((type_offset, tyinfo)))
//         }
//     }
// }
fn read_type_at_offset<'d, 'i>(
    offset: UnitOffset,
    unit: &UnitCtx<'d, 'i>,
    offset_to_ns: &BTreeMap<usize, Namespace<'i>>,
    offset_to_ty: &mut BTreeMap<usize, TypeInfo>,
) -> Result<TypeInfo, Error> {
    let global_offset = unit.to_global_offset(offset);
    if let Some(info) = offset_to_ty.get(&global_offset) {
        return Ok(info.clone());
    }
    let entry = unit.entry_at(offset)?;
    let info = match entry.tag() {
        DW_TAG_structure_type | DW_TAG_class_type => {
            read_structure_type(&entry, unit, offset_to_ns, offset_to_ty)?
        }
        DW_TAG_union_type => read_union_type(&entry, unit, offset_to_ns, offset_to_ty)?,
        DW_TAG_enumeration_type => read_enum_type(&entry, unit, offset_to_ns, offset_to_ty)?,
        DW_TAG_unspecified_type => {
            // guess
            let name = unit.get_entry_name(&entry)?;
            match name {
                // std::nullptr_t
                "decltype(nullptr)" => TypeInfo::Prim(TypePrim::U64),
                _ => {
                    let offset = unit.to_global_offset(offset);
                    return bad!(unit, offset, Error::UnspecifiedType)
                        .attach_printable(format!("the DW_AT_name is: {}", name));
                }
            }
        }
        DW_TAG_typedef => {
            let name = unit.get_entry_name(&entry)?;
            let namespace = unit.get_namespace(offset, offset_to_ns)?;
            match unit.get_entry_type_offset_optional(&entry)? {
                // typedef to void.. just use void
                None => TypeInfo::Prim(TypePrim::Void),
                Some(x) => TypeInfo::Typedef(namespace.get_with(name), unit.to_global_offset(x)),
            }
        }
        // T* or T&
        DW_TAG_pointer_type | DW_TAG_reference_type => {
            // can have void*
            let ty_offset = match unit.get_entry_type_offset_optional(&entry)? {
                None => usize::MAX, // void
                Some(x) => unit.to_global_offset(x),
            };
            TypeInfo::Comp(TypeComp::Ptr(ty_offset))
        }
        // modifiers that don't do anything..
        DW_TAG_const_type | DW_TAG_volatile_type => {
            // is just T
            match unit.get_entry_type_offset_optional(&entry)? {
                None => TypeInfo::Prim(TypePrim::Void),
                Some(x) => read_type_at_offset(x, unit, offset_to_ns, offset_to_ty)?,
            }
        }
        // T[n]
        DW_TAG_array_type => {
            let target = unit.get_entry_type_offset(&entry)?;
            let global_offset = unit.to_global_offset(offset);
            let mut tree = unit.tree_at(offset)?;
            let root = unit.root_of(offset, &mut tree)?;
            let mut children = root.children();
            let subrange = err_ctx!(unit, global_offset, Error::ReadChild, children.next())?;
            let subrange = opt_ctx!(unit, global_offset, Error::ExpectingChild, subrange)?;
            let subrange = subrange.entry();
            unit.check_tag(subrange, DW_TAG_subrange_type)?;
            let count = unit.get_entry_count(&subrange)?;
            match count {
                Some(count) => {
                    TypeInfo::Comp(TypeComp::Array(unit.to_global_offset(target), count))
                }
                None => {
                    // without count, just use ptr type
                    TypeInfo::Comp(TypeComp::Ptr(unit.to_global_offset(target)))
                }
            }
        }
        // T(*)(Args...)
        DW_TAG_subroutine_type => read_subroutine_type(&entry, unit)?,
        // PTM
        DW_TAG_ptr_to_member_type => {
            let this_ty = unit.get_entry_containing_type_offset(&entry)?;
            let this_ty = unit.to_global_offset(this_ty);
            let ty = unit.get_entry_type_offset(&entry)?;
            let entry = unit.entry_at(ty)?;
            if entry.tag() == DW_TAG_subroutine_type {
                // PTMF

                if let TypeInfo::Comp(TypeComp::Subroutine(ret, params)) =
                    read_subroutine_type(&entry, unit)?
                {
                    TypeInfo::Comp(TypeComp::Ptmf(this_ty, ret, params))
                } else {
                    unreachable!()
                }
            } else {
                // just normal pointer
                TypeInfo::Comp(TypeComp::Ptr(unit.to_global_offset(ty)))
            }
        }
        DW_TAG_base_type => TypeInfo::Prim(read_base_type(&entry, unit)?),

        tag => {
            let global_offset = unit.to_global_offset(offset);
            return bad!(unit, global_offset, Error::UnexpectedTagForType(tag));
        }
    };

    offset_to_ty.insert(global_offset, info.clone());
    Ok(info)
}

fn read_structure_type<'d, 'i, 'a, 'u>(
    entry: &DIE<'i, 'a, 'u>,
    unit: &UnitCtx<'d, 'i>,
    offset_to_ns: &BTreeMap<usize, Namespace<'i>>,
    offset_to_ty: &mut BTreeMap<usize, TypeInfo>,
) -> Result<TypeInfo, Error> {
    // structs can be anonymous
    let name = match unit.get_entry_name_optional(&entry)? {
        Some(name) => {
            let namespace = unit.get_namespace(entry.offset(), offset_to_ns)?;
            Some(namespace.get_with(name))
        }
        None => None,
    };
    // is declaration?
    if unit.get_entry_declaration(&entry)? {
        if name.is_none() {
            return bad!(
                unit,
                unit.to_global_offset(entry.offset()),
                Error::UnexpectedAnonymousDeclaration
            );
        }
        return Ok(TypeInfo::Struct(StructInfo {
            name,
            is_decl: true,
            vtable: Vec::new(),
            size: 0,
            members: Vec::new(),
        }));
    }
    // make a vtable type in case we need it
    let mut vtable = Vec::new();
    let mut vtable_locs = HashSet::new();
    let mut vdtor_name = None;
    let byte_size = unit.get_entry_byte_size_optional(&entry)?;
    if byte_size == 0 {
        return Ok(TypeInfo::Struct(StructInfo::zst()));
    }
    let mut tree = unit.tree_at(entry.offset())?;
    let node = unit.root_of(entry.offset(), &mut tree)?;
    let mut members = Vec::new();
    let mut is_first_base = true;
    unit.for_each_child(node, |child| {
        let entry = child.entry();
        match entry.tag() {
            DW_TAG_member => {
                if unit.get_entry_external(&entry)? {
                    return Ok(());
                }
                let name = unit.get_entry_name_optional(&entry)?.map(|s| s.to_string());
                let ty_offset = unit.to_global_offset(unit.get_entry_type_offset(&entry)?);
                let offset = unit.get_entry_data_member_location(&entry)?;
                members.push(MemberInfo {
                    offset,
                    name,
                    is_base: false,
                    ty_offset,
                });
            }
            DW_TAG_inheritance => {
                let offset = unit.get_entry_data_member_location(&entry)?;
                let ty_offset = unit.get_entry_type_offset(&entry)?;
                // derived classes may not have the full vtable
                // so we need to copy the base vtable
                if is_first_base {
                    is_first_base = false;
                    let mut ty_offset = ty_offset;
                    loop {
                        match read_type_at_offset(ty_offset, unit, offset_to_ns, offset_to_ty)? {
                            TypeInfo::Struct(base) => {
                                for (i, vf_name) in base.vtable.iter().enumerate() {
                                    if i >= vtable.len() {
                                        vtable.resize(i + 1, "".to_string());
                                    }
                                    if vtable[i].is_empty() {
                                        vtable[i] = vf_name.clone();
                                    }
                                }
                                break;
                            }
                            TypeInfo::Typedef(_, ty) => ty_offset = unit.to_unit_offset(ty),
                            _ => {
                                // transparent base type
                                break;
                            } // _ => {
                              //     return bad!(unit, unit.to_global_offset(entry.offset()), Error::InvalidBaseType)
                              //         .attach_printable(format!("Base type offset: 0x{:08x}", unit.to_global_offset(ty_offset)));
                              // }
                        }
                    }
                }
                let name = if offset == 0 {
                    "base".to_string()
                } else {
                    format!("base_{:x}", offset)
                };
                members.push(MemberInfo {
                    offset,
                    name: Some(name),
                    is_base: true,
                    ty_offset: unit.to_global_offset(ty_offset),
                });
            }
            DW_TAG_subprogram => {
                if let Some(velem) = unit.get_entry_vtable_elem_location(&entry)? {
                    let name = unit.get_entry_name(&entry)?.to_string();
                    // dtors are weird that they are declared 0th, but might not actually be
                    if name.starts_with("~") {
                        if velem != 0 {
                            return bad!(
                                unit,
                                unit.to_global_offset(entry.offset()),
                                Error::ConflictingVtableEntry
                            )
                            .attach_printable(format!("Vtable entry {}", velem))
                            .attach_printable(format!("Vtable: {:?}", vtable))
                            .attach_printable(format!("Need to insert: {:?}", name))
                            .attach_printable("Expecting dtor to be declared 0th in vtable");
                        }
                        vdtor_name = Some(name.clone());
                    } else {
                        if !vtable_locs.insert(velem) {
                            return bad!(
                                unit,
                                unit.to_global_offset(entry.offset()),
                                Error::ConflictingVtableEntry
                            )
                            .attach_printable(format!("Vtable entry {}", velem))
                            .attach_printable(format!("Vtable: {:?}", vtable))
                            .attach_printable(format!("Need to insert: {:?}", name));
                        }
                        if velem >= vtable.len() {
                            vtable.resize(velem + 1, "".to_string());
                        }
                        vtable[velem] = name;
                    }
                }
            }
            DW_TAG_structure_type
            | DW_TAG_class_type
            | DW_TAG_union_type
            | DW_TAG_enumeration_type
            | DW_TAG_typedef
            | DW_TAG_template_type_parameter
            | DW_TAG_template_value_parameter
            | DW_TAG_GNU_template_parameter_pack => {
                // ignore subtypes
            }
            tag => {
                let global_offset = unit.to_global_offset(entry.offset());
                return bad!(unit, global_offset, Error::UnexpectedTag(tag))
                    .attach_printable("reading DW_TAG_structure_type");
            }
        }
        Ok(())
    })?;
    // place virtual dtor
    if let Some(vdtor_name) = vdtor_name {
        match vtable
            .iter()
            .position(|x| x.starts_with('~') || x.is_empty())
        {
            Some(i) => {
                vtable[i] = vdtor_name;
            }
            None => {
                vtable.push(vdtor_name);
            }
        }
    }
    // handle virtual destructors
    if vtable.len() > 1 {
        for i in 0..vtable.len() - 1 {
            if vtable[i].starts_with('~') && vtable[i + 1].is_empty() {
                vtable[i + 1] = vtable[i].clone();
            }
        }
    }
    // check no holes in vtable
    for (i, name) in vtable.iter().enumerate() {
        if name.is_empty() {
            return bad!(
                unit,
                unit.to_global_offset(entry.offset()),
                Error::MissingVtableEntry
            )
            .attach_printable(format!("Vtable entry {}", i))
            .attach_printable(format!("Vtable: {:?}", vtable));
        }
    }
    // transparent struct
    // if the struct has 1 member, and no vtable, and anonymous, or is a std thing
    if members.len() == 1 && vtable.is_empty() {
        let member = &members[0];
        if member.offset == 0 && !member.is_base {
            // making std structs transparent can merge things like std::array and primitive arrays
            let is_std_struct = match &name {
                Some(name) => name.starts_with("std::"),
                None => false,
            };
            if name.is_none() || member.name.is_none() || is_std_struct {
                match name {
                    None => {
                        // turn the struct into that member
                        let off = unit.to_unit_offset(member.ty_offset);
                        return read_type_at_offset(off, unit, offset_to_ns, offset_to_ty);
                    }
                    Some(name) => {
                        // turn the struct into a typedef
                        let ty_offset = member.ty_offset;
                        return Ok(TypeInfo::Typedef(name, ty_offset));
                    }
                }
            }
        }
    }

    Ok(TypeInfo::Struct(StructInfo {
        name,
        is_decl: false,
        size: byte_size,
        members,
        vtable,
    }))
}

fn read_union_type<'d, 'i, 'a, 'u>(
    entry: &DIE<'i, 'a, 'u>,
    unit: &UnitCtx<'d, 'i>,
    offset_to_ns: &BTreeMap<usize, Namespace<'i>>,
    offset_to_ty: &mut BTreeMap<usize, TypeInfo>,
) -> Result<TypeInfo, Error> {
    // can be anonymous
    let name = match unit.get_entry_name_optional(&entry)? {
        Some(name) => {
            let namespace = unit.get_namespace(entry.offset(), offset_to_ns)?;
            Some(namespace.get_with(name))
        }
        None => None,
    };
    // is declaration?
    if unit.get_entry_declaration(&entry)? {
        if name.is_none() {
            return bad!(
                unit,
                unit.to_global_offset(entry.offset()),
                Error::UnexpectedAnonymousDeclaration
            );
        }
        return Ok(TypeInfo::Union(UnionInfo {
            name,
            is_decl: true,
            size: 0,
            members: Vec::new(),
        }));
    }
    let byte_size = unit.get_entry_byte_size(&entry)?;
    if byte_size == 0 {
        return Ok(TypeInfo::Struct(StructInfo::zst()));
    }
    let mut tree = unit.tree_at(entry.offset())?;
    let node = unit.root_of(entry.offset(), &mut tree)?;
    let mut members = Vec::<(Option<String>, usize)>::new();
    unit.for_each_child(node, |child| {
        let entry = child.entry();
        match entry.tag() {
            DW_TAG_member => {
                let name = unit.get_entry_name_optional(&entry)?.map(|s| s.to_string());
                let ty_offset = unit.to_global_offset(unit.get_entry_type_offset(&entry)?);
                // if type is duplicated, just ignore if
                if !members.iter().any(|x| x.1 == ty_offset) {
                    members.push((name, ty_offset));
                }
            }
            DW_TAG_structure_type
            | DW_TAG_class_type
            | DW_TAG_union_type
            | DW_TAG_enumeration_type
            | DW_TAG_typedef
            | DW_TAG_template_type_parameter
            | DW_TAG_template_value_parameter
            | DW_TAG_GNU_template_parameter_pack => {
                // ignore subtypes
            }
            DW_TAG_subprogram => {
                if unit.get_entry_vtable_elem_location(&entry)?.is_some() {
                    return bad!(
                        unit,
                        unit.to_global_offset(entry.offset()),
                        Error::UnexpectedVirtualUnion
                    );
                }
            }
            tag => {
                let global_offset = unit.to_global_offset(entry.offset());
                return bad!(unit, global_offset, Error::UnexpectedTag(tag))
                    .attach_printable("reading DW_TAG_union_type");
            }
        }
        Ok(())
    })?;

    // transparent union
    // if the union has 1 member
    if members.len() == 1 {
        let member = &members[0];
        if member.0.is_none() {
            match name {
                None => {
                    // turn the union into that member
                    let off = unit.to_unit_offset(member.1);
                    return read_type_at_offset(off, unit, offset_to_ns, offset_to_ty);
                }
                Some(name) => {
                    // turn the union into a typedef
                    let ty_offset = member.1;
                    return Ok(TypeInfo::Typedef(name, ty_offset));
                }
            }
        }
    }

    Ok(TypeInfo::Union(UnionInfo {
        name,
        size: byte_size,
        is_decl: false,
        members,
    }))
}

fn read_enum_type<'d, 'i, 'a, 'u>(
    entry: &DIE<'i, 'a, 'u>,
    unit: &UnitCtx<'d, 'i>,
    offset_to_ns: &BTreeMap<usize, Namespace<'i>>,
    offset_to_ty: &mut BTreeMap<usize, TypeInfo>,
) -> Result<TypeInfo, Error> {
    // can be anonymous
    let name = match unit.get_entry_name_optional(&entry)? {
        Some(name) => {
            let namespace = unit.get_namespace(entry.offset(), offset_to_ns)?;
            Some(namespace.get_with(name))
        }
        None => None,
    };
    // is declaration?
    if unit.get_entry_declaration(&entry)? {
        if name.is_none() {
            return bad!(
                unit,
                unit.to_global_offset(entry.offset()),
                Error::UnexpectedAnonymousDeclaration
            );
        }
        return Ok(TypeInfo::Enum(EnumInfo {
            name,
            is_decl: true,
            size: 0,
            enumerators: Vec::new(),
        }));
    }
    let byte_size = match unit.get_entry_type_offset_optional(&entry)? {
        Some(off) => {
            let mut off = off;
            loop {
                let ty = read_type_at_offset(off, unit, offset_to_ns, offset_to_ty)?;
                match ty {
                    TypeInfo::Prim(p) => break p.size().unwrap(),
                    TypeInfo::Typedef(_, next) => {
                        off = unit.to_unit_offset(next);
                    }
                    _ => {
                        return bad!(unit, unit.to_global_offset(off), Error::UnexpectedEnumType)
                            .attach_printable(format!(
                                "base type is {}, which is not primitive",
                                ty
                            ))
                    }
                }
            }
        }
        None => unit.get_entry_byte_size(&entry)?,
    };
    if byte_size == 0 {
        return Ok(TypeInfo::Struct(StructInfo::zst()));
    }
    let mut tree = unit.tree_at(entry.offset())?;
    let node = unit.root_of(entry.offset(), &mut tree)?;
    let mut members = Vec::new();
    unit.for_each_child(node, |child| {
        let entry = child.entry();
        match entry.tag() {
            DW_TAG_enumerator => {
                let name = unit.get_entry_name(&entry)?;
                let value = unit.get_entry_const_value(&entry)?;
                members.push((name.to_string(), value));
            }
            tag => {
                let global_offset = unit.to_global_offset(entry.offset());
                return bad!(unit, global_offset, Error::UnexpectedTag(tag))
                    .attach_printable("reading DW_TAG_enumeration_type");
            }
        }
        Ok(())
    })?;

    Ok(TypeInfo::Enum(EnumInfo {
        name,
        size: byte_size,
        is_decl: false,
        enumerators: members,
    }))
}

fn read_subroutine_type<'d, 'i, 'a, 'u>(
    entry: &DIE<'i, 'a, 'u>,
    unit: &UnitCtx<'d, 'i>,
) -> Result<TypeInfo, Error> {
    let offset = entry.offset();
    let rettype = unit.get_entry_type_offset_optional(&entry)?;
    let rettype = match rettype {
        None => usize::MAX, // void
        Some(x) => unit.to_global_offset(x),
    };
    let mut arg_types = Vec::new();
    let mut tree = unit.tree_at(offset)?;
    let root = unit.root_of(offset, &mut tree)?;
    unit.for_each_child(root, |child| {
        let entry = child.entry();
        unit.check_tag(entry, DW_TAG_formal_parameter)?;
        let ty = unit.get_entry_type_offset_optional(&entry)?;
        let ty = match ty {
            None => usize::MAX, // void
            Some(x) => unit.to_global_offset(x),
        };
        arg_types.push(ty);
        Ok(())
    })?;

    Ok(TypeInfo::Comp(TypeComp::Subroutine(rettype, arg_types)))
}

/// Get TypeInfo for a DW_TAG_base_type
fn read_base_type<'d, 'i, 'a, 'u>(
    entry: &DIE<'i, 'a, 'u>,
    unit: &UnitCtx<'d, 'i>,
) -> Result<TypePrim, Error> {
    let offset = unit.to_global_offset(entry.offset());
    let encoding = err_ctx!(
        unit,
        offset,
        Error::ReadEntryAttr(DW_AT_encoding),
        entry.attr_value(DW_AT_encoding)
    )?;
    let encoding = opt_ctx!(
        unit,
        offset,
        Error::MissingEntryAttr(DW_AT_encoding),
        encoding
    )?;
    let encoding = match encoding {
        AttributeValue::Encoding(x) => Some(x),
        _ => None,
    };
    let encoding = opt_ctx!(
        unit,
        offset,
        Error::BadEntryAttrType(DW_AT_encoding, "Encoding"),
        encoding
    )?;
    let byte_size = unit.get_entry_byte_size(entry)?;
    match (encoding, byte_size) {
        (DW_ATE_boolean, 0x1) => Ok(TypePrim::Bool),
        (DW_ATE_unsigned, 0x1) => Ok(TypePrim::U8),
        (DW_ATE_unsigned_char, 0x1) => Ok(TypePrim::U8),
        (DW_ATE_signed, 0x1) => Ok(TypePrim::I8),
        (DW_ATE_signed_char, 0x1) => Ok(TypePrim::I8),

        (DW_ATE_unsigned, 0x2) => Ok(TypePrim::U16),
        (DW_ATE_signed, 0x2) => Ok(TypePrim::I16),
        (DW_ATE_UTF, 0x2) => Ok(TypePrim::U16),

        (DW_ATE_unsigned, 0x4) => Ok(TypePrim::U32),
        (DW_ATE_signed, 0x4) => Ok(TypePrim::I32),
        (DW_ATE_float, 0x4) => Ok(TypePrim::F32),

        (DW_ATE_unsigned, 0x8) => Ok(TypePrim::U64),
        (DW_ATE_signed, 0x8) => Ok(TypePrim::I64),
        (DW_ATE_float, 0x8) => Ok(TypePrim::F64),

        (DW_ATE_unsigned, 0x10) => Ok(TypePrim::U128),
        (DW_ATE_signed, 0x10) => Ok(TypePrim::I128),
        (DW_ATE_float, 0x10) => Ok(TypePrim::F128),
        _ => {
            // TODO complete list above
            return bad!(
                unit,
                offset,
                Error::BadEntryAttrTypes(
                    [DW_AT_encoding.to_string(), DW_AT_byte_size.to_string(),].join(", ")
                )
            )
            .attach_printable(format!(
                "Got: encoding={}, bytes={}",
                encoding.to_string(),
                byte_size
            ));
        }
    }
}
