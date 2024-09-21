use std::collections::BTreeMap;

use error_stack::{Result, ResultExt};
use gimli::{
    DW_TAG_GNU_template_parameter_pack, DW_TAG_class_type, DW_TAG_enumeration_type, DW_TAG_member,
    DW_TAG_structure_type, DW_TAG_subprogram, DW_TAG_template_type_parameter,
    DW_TAG_template_value_parameter, DW_TAG_typedef, DW_TAG_union_type,
};

use crate::parsed::{Namespace, Offset, TypeInfo, UnionInfo};

use super::unit::bad;
use super::{read_type_at_offset, Error, UnitCtx, DIE};

pub fn read_union_type<'d, 'i, 'a, 'u>(
    entry: &DIE<'i, 'a, 'u>,
    unit: &UnitCtx<'d, 'i>,
    offset_to_ns: &BTreeMap<usize, Namespace<'i>>,
    offset_to_ty: &mut BTreeMap<Offset, TypeInfo>,
    merges: &mut Vec<(Offset, Offset)>,
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
    // if byte_size == 0 {
    //     return Ok(TypeInfo::Struct(StructInfo::zst()));
    // }
    let mut members = Vec::<(Option<String>, Offset)>::new();
    unit.for_each_child_entry(entry, |child| {
        let entry = child.entry();
        match entry.tag() {
            DW_TAG_member => {
                let name = unit.get_entry_name_optional(&entry)?.map(|s| s.to_string());
                let ty_offset = unit
                    .to_global_offset(unit.get_entry_type_offset(&entry)?)
                    .into();
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
                    let off = unit.to_unit_offset(member.1.into());
                    return read_type_at_offset(off, unit, offset_to_ns, offset_to_ty, merges);
                }
                Some(name) => {
                    // turn the union into a typedef
                    let ty_offset = member.1;
                    return Ok(TypeInfo::Typedef(name, ty_offset.into()));
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
