use std::sync::Arc;

use super::{TypeComp, TypePrim, TypeYaml};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDef {
    // Prim(TypePrim),
    /// Struct or Class
    Struct(StructDef),
    /// Enum
    Enum(EnumDef),
    /// Union
    Union(UnionDef),
    // Comp(Box<TypeComp<TypeDef>>),
}

impl TypeDef {
    pub fn ptmf(name: String) -> Self {
        let func = MemberDef {
            offset: 0,
            name: "func".to_string(),
            is_base: false,
            ty_yaml: TypePrim::U64.yaml_string(),
        };
        let adjustment = MemberDef {
            offset: 8,
            name: "adjustment".to_string(),
            is_base: false,
            ty_yaml: TypePrim::I64.yaml_string(),
        };
        let members = vec![func, adjustment];
        let ptmf = StructDef {
            name,
            size: 16,
            vtable: vec![],
            members
        };
        Self::Struct(ptmf)
    }

    pub fn as_enum(&self) -> Option<&EnumDef> {
        match self {
            Self::Enum(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_struct(&self) -> Option<&StructDef> {
        match self {
            Self::Struct(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_union(&self) -> Option<&UnionDef> {
        match self {
            Self::Union(u) => Some(u),
            _ => None,
        }
    }
}

impl TypeYaml for TypeDef {
    fn yaml_string(&self) -> String {
        match self {
            // TypeDef::Prim(p) => p.yaml_string(),
            TypeDef::Struct(x) => format!("\"{}\"", x.name),
            TypeDef::Enum(x) => format!("\"{}\"", x.name),
            TypeDef::Union(x) => format!("\"{}\"", x.name),
            // TypeDef::Comp(c) => c.yaml_string(),
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub name: String,
    pub vtable: Vec<String>,
    pub size: usize,
    pub members: Vec<MemberDef>,
}
impl TypeYaml for StructDef {
    fn yaml_string(&self) -> String {
        let mut s = format!("  - name: '{}'\n", self.name);
        s.push_str(&format!("    size: 0x{:x}\n", self.size));
        if !self.vtable.is_empty() {
            s.push_str("    vtable:\n");
            for func in &self.vtable {
                s.push_str(&format!("      - '{}'\n", func));
            }
        }
        s.push_str("    members:\n");
        for member in &self.members {
            s.push_str(&member.yaml_string());
            s.push_str("\n");
        }
        s
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberDef {
    pub offset: usize,
    pub name: String,
    pub is_base: bool,
    pub ty_yaml: String,
}
impl TypeYaml for MemberDef {
    fn yaml_string(&self) -> String {
        let mut s = format!("      '0x{:x}': {{ ", self.offset);
        if self.is_base {
            s.push_str("base: true, ");
        }
        s.push_str(&format!("name: '{}', ", self.name));
        s.push_str(&format!("type: [ {} ] }}", self.ty_yaml));
        s
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDef {
    pub name: String,
    pub size: usize,
    pub enumerators: Vec<(String, i128)>,
}
impl TypeYaml for EnumDef {
    fn yaml_string(&self) -> String {
        let mut s = format!("  - name: '{}'\n", self.name);
        s.push_str(&format!("    size: 0x{:x}\n", self.size));
        s.push_str("    enumerators:\n");
        for (name, val) in &self.enumerators {
            s.push_str(&format!("      {name}: {val}\n"))
        }
        s
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionDef {
    pub name: String,
    pub size: usize,
    pub members: Vec<(String, String)>,
}
impl TypeYaml for UnionDef {
    fn yaml_string(&self) -> String {
        let mut s = format!("  - name: '{}'\n", self.name);
        s.push_str(&format!("    size: 0x{:x}\n", self.size));
        s.push_str("    members:\n");
        for (name, ty) in &self.members {
            s.push_str(&format!("      {name}: [ {ty} ]\n"))
        }
        s
    }
}
