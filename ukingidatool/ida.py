import math
import json
import time
import traceback

import ida_typeinf
import ida_nalt
import ida_name

START_TIME = time.time()
VERBOSE = 0
def _set_verbose(level):
    global VERBOSE
    VERBOSE = level

## ////////////// DATA //////////////
class StructDef:
    size = 0
    align = 0
    members = [] # MemberDef[]

def _make_struct(size, align, members):
    struct = StructDef()
    struct.size = size
    struct.align = align
    struct.members = members
    return struct

class MemberDef:
    name = ""
    offset = 0
    tyyaml = []
    is_base = False

def _make_member(name, offset, tyyaml, is_base=False):
    member = MemberDef()
    member.name = name
    member.offset = offset
    member.tyyaml = tyyaml
    member.is_base = is_base
    return member

def _make_union_member(name, tyyaml):
    member = MemberDef()
    member.name = name
    member.offset = 0
    member.tyyaml = tyyaml
    member.is_base = False
    return member

class EnumDef:
    size = 0
    enumerators = [] # (name, int)[]

def _make_enum(size, enumerators):
    enum = EnumDef()
    enum.size = size
    enum.enumerators = enumerators
    return enum

class UnionDef:
    size = 0
    align = 0
    members = [] # MemberDef[]

def _make_union(size, align, members):
    union = UnionDef()
    union.size = size
    union.align = align
    union.members = members
    return union

def _make_vtable(vtable): # vtable: (name, tyyaml)[]
    size = len(vtable) * 8
    align = 1
    members = []
    for (i, (name, tyyaml)) in enumerate(vtable):
        member = _make_member(name, i * 8, tyyaml)
        members.append(member)
    return _make_struct(size, align, members)

class FunctionDef:
    name = "" # may be empty
    tyyaml = [] # may be empty
    args = [] # NameAndType[]

def _make_function(name, tyyaml, args):
    func = FunctionDef()
    func.name = name
    if tyyaml:
        func.tyyaml = tyyaml
    if args:
        func.args = args
    return func

class NameAndType:
    name = "" # may be empty
    tyyaml = [] # may be empty

def _make_name_type(name, tyyaml):
    nt = NameAndType()
    nt.name = name
    if tyyaml:
        nt.tyyaml = tyyaml
    return nt

## ////////////// HEURISTICS //////////////
def _can_ovrd_member(m1: str, m2: str):
    """If m1 can override m2 as a member name"""
    heuristics = [
        # prefers not __placeholder
        lambda m: m != "__placeholder",
        # prefers __vtable
        lambda m: m == "__vtable",
        # prefers not starting with _
        lambda m: not m.startswith("_"),
        # prefers not starting with field_
        lambda m: not m.startswith("field_"),
        # prefers not empty
        lambda m: bool(m),
        # prefers not ending with a hex number (offset)
        lambda m: not m in "0123456789abcdefABCDEF",
    ]
    return _prefers_a_by(m1, m2, heuristics) != False

def _can_ovrd_symbol(m1: str, m2: str):
    """If m1 can override m2 as a symbol (function or data) name"""
    heuristics = [
        # prefers non-empty
        lambda m: bool(m),
        # prefers not starting with sub_, nullsub_ or j_
        lambda m: not m.startswith("sub_") and not m.startswith("nullsub_") and not m.startswith("j_"),
        # prefers mangled
        lambda m: m.startswith("_Z"),
    ]
    r = _prefers_a_by(m1, m2, heuristics)
    if r is not None:
        return r
    # if m1 is mangled name, it can probably always override
    return m1.startswith("_Z")


def _prefers_a_by(a, b, heuristics):
    """
    Compare A and B using the given heuristics
    Return True if A is preferred over B, False if B is preferred over A, None if no preference
    The heuristics can return a bool or a number. 
    For bools, True is more preferred.
    For numbers, greater is more preferred.
    """
    for h in heuristics:
        h_a = h(a)
        h_b = h(b)
        if isinstance(h_a, bool):
            if h_a and not h_b:
                return True
            if h_b and not h_a:
                return False 
        else:
            if h_a > h_b:
                return True
            if h_b > h_a:
                return False
    return None

## ////////////// IDA Helpers //////////////

def _align2sda(align):
    """
    Convert alignment to Declared Structure Alignment value
    See https://hex-rays.com/products/ida/support/sdkdoc/structudt__type__data__t.html 
    """
    return int(math.log2(align)) + 1

def _set_udt_align(udt, align):
    udt.sda = _align2sda(align)
    # udt.effalign = align

def _get_tinfo(name):
    """"""
    _assert(not name.startswith("("), f"Invalid name: {name}")
    tinfo = ida_typeinf.tinfo_t()
    _assert(tinfo.get_named_type(None, name), f"Failed to get type by name: {name}")
    return tinfo


def _explicit_tail_padding(udt, size):
    """Explicitly add tail padding to a struct"""
    if udt.empty():
        return
    last_field = udt.back()
    # bits
    gap_offset = last_field.offset + last_field.size
    gap_size = size - gap_offset
    if gap_size <= 0:
        return
    gap_member = ida_typeinf.udt_member_t()
    gap_member.name = f"__tail_{gap_offset // 8:x}"
    gap_member.size = gap_size
    gap_member.offset = gap_offset
    c = ida_typeinf.tinfo_t(ida_typeinf.BTF_CHAR)
    gap_type = ida_typeinf.tinfo_t()
    _assert(gap_type.create_array(c, gap_size // 8), f"Failed to create tail padding type")
    gap_member.type = gap_type
    udt.push_back(gap_member)


## ////////////// UTILS //////////////
def _fmt_time(seconds):
    seconds = int(seconds)
    h, r = divmod(seconds, 3600)  # 3600 seconds in an hour
    m, s = divmod(r, 60)  # 60 seconds in a minute
    return f"{h:02d}:{m:02d}:{s:02d}"

def _print_depth(level, arg, depth):
    if level > VERBOSE:
        return
    padding = "  " * depth
    if level == 1:
        prefix = "-- "
    elif level == 2:
        prefix = "--> "
    else:
        prefix = ""
    print(f"[ukingidatool]{padding}{prefix}", arg)

def _assert(cond, msg):
    if not cond:
        raise RuntimeError(msg)

def _done():
    t = _fmt_time(time.time() - START_TIME)
    _print_depth(0, f"Done in {t}", 0)

class TypeImporter:
    """
    Type Importer context

    This class is largely adopted from classgen: https://github.com/leoetlino/classgen

    MIT License

    Copyright (c) 2021 leoetlino

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
    """
    skipping: bool = False

    # names that are already imported
    imported = set()
    name2struct = {}
    name2enum = {}
    name2union = {}

    depth = 0
    def _print(self, level, arg):
        _print_depth(level, arg, self.depth)

    def skip(self):
        self.skipping = True

    def add_struct(self, name, struct, vtable):
        """Add a struct definition"""
        self.name2struct[name] = struct
        if vtable:
            self.add_struct(name + "_vtbl", _make_vtable(vtable), [])

    def add_enum(self, name, enum):
        """Add an enum definition"""
        self.name2enum[name] = enum

    def add_union(self, name, union):
        """Add a union definition"""
        self.name2union[name] = union

    def run_import(self, substring_pattern):
        """Import all types whose name contains the given substring"""
        struct_names = [name for name in self.name2struct if not substring_pattern or substring_pattern in name]
        enum_names = [name for name in self.name2enum if not substring_pattern or substring_pattern in name]
        union_names = [name for name in self.name2union if not substring_pattern or substring_pattern in name]
        struct_total = len(struct_names)
        for (i, name) in enumerate(struct_names):
            self._print(0, f"struct {i}/{struct_total}")
            self._import_named(name)
        enum_total = len(enum_names)
        for (i, name) in enumerate(enum_names):
            self._print(0, f"enum {i}/{enum_total}")
            self._import_named(name)
        union_total = len(union_names)
        for (i, name) in enumerate(union_names):
            self._print(0, f"union {i}/{union_total}")
            self._import_named(name)

    def _tyyaml2tinfo(self, tyyaml):
        """Convert a type YAML to a tinfo_t"""
        _assert(isinstance(tyyaml, list), f"Invalid type YAML (not a list): {tyyaml}")
        _assert(len(tyyaml) > 0, f"Invalid type YAML (empty list): {tyyaml}")
        base = self._tyyaml2tinfo_base(tyyaml[0])
        return self._tyyaml2tinfo_recur(tyyaml[1:], base)

    def _tyyaml2tinfo_recur(self, tyyaml, base):
        """Convert a type YAML to a tinfo_t recursively"""
        if not tyyaml:
            # done
            return base
        spec = tyyaml[0]
        # pointer
        if spec == "*": 
            tinfo = ida_typeinf.tinfo_t()
            _assert(tinfo.create_ptr(base), f"Failed to create pointer type: {tyyaml}")
            return self._tyyaml2tinfo_recur(tyyaml[1:], tinfo)
        # array
        if isinstance(spec, list) and len(spec) == 1 and isinstance(spec[0], int):
            tinfo = ida_typeinf.tinfo_t()
            _assert(tinfo.create_array(base, spec[0]), f"Failed to create array type: {tyyaml}")
            return self._tyyaml2tinfo_recur(tyyaml[1:], tinfo)
        # subroutine
        if spec == "()":
            args = tyyaml[1]
            funcargs = self._tyyaml2funcarg(args)
            func = ida_typeinf.func_type_data_t()
            func.cc = ida_typeinf.CM_CC_FASTCALL
            func.rettype = base
            for arg in funcargs:
                func.push_back(arg)
            tinfo = ida_typeinf.tinfo_t()
            _assert(tinfo.create_func(func), f"Failed to create function type: {tyyaml}")
            return self._tyyaml2tinfo_recur(tyyaml[2:], tinfo)
        # PTMF
        if spec == "(ptmf)":
            # PTMF are generated as THISTYPE_ptmf in the data
            # we ignore the base type generated previously
            name_t = tyyaml[1]
            _assert(
                isinstance(name_t, list) and len(name_t) == 1 and isinstance(name_t[0], str), 
                f"Invalid type YAML (expected PTMF class name): {tyyaml}"
            )
            name_t = name_t[0]
            _assert(name_t.startswith("\"") and name_t.endswith("\""), f"Invalid PTMF class name: {name_t}")
            name_t = name_t[1:-1] + "_ptmf"
            base = self._tyyaml2tinfo_base(f"\"{name_t}\"")
            # 0 - ptmf, 1 - class type, 2 - args, start from 3
            return self._tyyaml2tinfo_recur(tyyaml[3:], base)
        raise RuntimeError(f"Unknown type spec: {spec}")



    def _tyyaml2funcarg(self, args):
        _assert(isinstance(args, list), f"Invalid type YAML (not a list of args): {args}")
        args = [self._tyyaml2tinfo(arg) for arg in args]
        funcargs = []
        for arg in args:
            funcarg = ida_typeinf.funcarg_t()
            funcarg.type = arg
            funcargs.append(funcarg)
        return funcargs


    def _tyyaml2tinfo_base(self, ident):
        """Convert a base type YAML to a tinfo_t"""
        _assert(isinstance(ident, str), f"Invalid base type YAML (None)")
        if ident.startswith("\"") and ident.endswith("\""):
            name = ident[1:-1]
            return self._name2tinfo(name)
        # base types
        if ident == "void":
            return ida_typeinf.tinfo_t(ida_typeinf.BTF_VOID)
        if ident == "bool":
            return ida_typeinf.tinfo_t(ida_typeinf.BTF_BOOL)
        if ident == "u8":
            return ida_typeinf.tinfo_t(ida_typeinf.BTF_UCHAR)
        if ident == "u16":
            return ida_typeinf.tinfo_t(ida_typeinf.BTF_UINT16)
        if ident == "u32":
            return ida_typeinf.tinfo_t(ida_typeinf.BTF_UINT32)
        if ident == "u64":
            return ida_typeinf.tinfo_t(ida_typeinf.BTF_UINT64)
        if ident == "u128":
            return ida_typeinf.tinfo_t(ida_typeinf.BTF_UINT128)
        if ident == "i8":
            return ida_typeinf.tinfo_t(ida_typeinf.BTF_INT8)
        if ident == "i16":
            return ida_typeinf.tinfo_t(ida_typeinf.BTF_INT16)
        if ident == "i32":
            return ida_typeinf.tinfo_t(ida_typeinf.BTF_INT32)
        if ident == "i64":
            return ida_typeinf.tinfo_t(ida_typeinf.BTF_INT64)
        if ident == "i128":
            return ida_typeinf.tinfo_t(ida_typeinf.BTF_INT128)
        if ident == "f32":
            return ida_typeinf.tinfo_t(ida_typeinf.BTF_FLOAT)
        if ident == "f64":
            return ida_typeinf.tinfo_t(ida_typeinf.BTF_DOUBLE)
        if ident == "f128":
            return ida_typeinf.tinfo_t(ida_typeinf.BTF_LDOUBLE)
        raise RuntimeError(f"Unknown base type: {ident} (<- this should have quotes if it's a named type)")


    def _name2tinfo(self, name):
        # IDA dislikes names starting with (
        _assert(not name.startswith("("), f"Invalid name: {name}")
        self._import_named(name)
        return _get_tinfo(name)

    def _import_named(self, name):
        if self.skipping:
            return
        if name in self.imported:
            return
        self._print(0, f"Importing {name}")
        self.depth += 1
        self.imported.add(name)
        if name in self.name2enum:
            try:
                self._import_enum(name)
            except:
                self._print(0, f"Failed to import enum {name}")
                raise
        elif name in self.name2struct:
            try:
                self._import_struct(name)
            except:
                self._print(0, f"Failed to import struct {name}")
                raise
        elif name in self.name2union:
            try:
                self._import_union(name)
            except:
                self._print(0, f"Failed to import union {name}")
                raise
        else:
            raise RuntimeError(f"Unknown type: {name}")
        self.depth -= 1

    def _import_enum(self, name):
        self._print(1, f"Enum {name}")
        info: EnumDef = self.name2enum[name]
        # get existing type for name comparision
        existing = ida_typeinf.tinfo_t()
        value2enumeratorname = {}
        if existing.get_named_type(None, name):
            existing_data = ida_typeinf.enum_type_data_t()
            if existing.get_enum_details(existing_data):
                for member in existing_data:
                    value2enumeratorname[int(member.value)] = member.name

        data = ida_typeinf.enum_type_data_t()
        # There could be empty enums, which are invalid (because of unknown size)
        if info.size == 0:
            info.size = 1
        if not info.enumerators:
            info.enumerators = [("UNKNOWN", 0)]

        _assert(1<=info.size<=8, f"Invalid enum size: {info.size}")
        data.bte |= int(math.log2(info.size)) + 1 # set byte size
        _assert(data.calc_nbytes() == info.size, f"Enum size mismatch: Actual: {data.calc_nbytes()} != Expected: {info.size}")
        for (enumerator_name, value) in info.enumerators:
            try:
                member = ida_typeinf.enum_member_t()
                value = int(value)
                member.value = value
                new_name = name + "::" + enumerator_name
                if value in value2enumeratorname:
                    existing_name = value2enumeratorname[value]
                    if new_name != existing_name and _can_ovrd_member(new_name, existing_name):
                        self._print(2, f"Rename enumerator: {existing_name} -> {new_name}")
                        member.name = new_name
                    else:
                        self._print(2, f"Reuse enumerator name: {existing_name}")
                        member.name = existing_name
                else:
                    self._print(2, f"Add enumerator: {new_name}")
                    member.name = new_name
                data.push_back(member)
            except:
                self._print(0, f"Failed to add enum member {enumerator_name} to {name}: value = {value}")
                raise
        tinfo = ida_typeinf.tinfo_t()
        _assert(tinfo.create_enum(data), f"Failed to create enum type: {name}")
        self._set_tinfo(name, tinfo)
        

    def _import_union(self, name):
        self._print(1, f"Union {name}")
        info: UnionDef = self.name2union[name]
        self._create_placeholder(name, info.size, info.align)

        membernames = []
        existing = ida_typeinf.tinfo_t()
        if existing.get_named_type(None, name):
            existing_data = ida_typeinf.udt_type_data_t()
            if existing.get_udt_details(existing_data):
                for member in existing_data:
                    membernames.append(member.name)

        # only reuse name if member count matches
        reuse_name = len(membernames) == len(info.members)
        if not reuse_name:
            self._print(1, f"Not reusing names because member count changed")

        udt = ida_typeinf.udt_type_data_t()
        udt.taudt_bits |= ida_typeinf.TAUDT_CPPOBJ
        udt.is_union = True
        _set_udt_align(udt, info.align)
        for (i, m) in enumerate(info.members):
            m: MemberDef
            member_d = ida_typeinf.udt_member_t()
            _assert(m.offset == 0, f"Union member {m.name} has non-zero offset: {m.offset}")
            member_d.offset = 0

            new_name = m.name
            if reuse_name:
                existing_name = membernames[i]
                if new_name != existing_name and _can_ovrd_member(new_name, existing_name):
                    self._print(2, f"Rename union member: {existing_name} -> {new_name}")
                    member_d.name = new_name
                else:
                    self._print(2, f"Reuse union member name: {existing_name}")
                    member_d.name = existing_name
            else:
                self._print(2, f"Add union member: {new_name}")
                member_d.name = new_name

            member_d.type = self._tyyaml2tinfo(m.tyyaml)
            _assert(member_d.type is not None, f"Failed to set union member type: {m.name}")
            member_size = member_d.type.get_size()
            _assert(member_size != ida_typeinf.BADSIZE, f"Failed to get union member size: {m.name}")
            member_d.size = member_size * 8 # bits
            udt.push_back(member_d)
        tinfo = ida_typeinf.tinfo_t()
        _assert(tinfo.create_udt(udt, ida_typeinf.BTF_UNION), f"Failed to create union type: {name}")
        _assert(tinfo.get_size() == info.size, f"Union size mismatch: Actual: {tinfo.get_size()} != Expected: {info.size}")

        self._set_tinfo(name, tinfo)


    def _import_struct(self, name):
        self._print(1, f"Struct {name}")
        info: StructDef = self.name2struct[name]
        self._create_placeholder(name, info.size, info.align)

        udt = ida_typeinf.udt_type_data_t()
        udt.taudt_bits |= ida_typeinf.TAUDT_CPPOBJ
        udt.is_union = False
        _set_udt_align(udt, info.align)

        off2membername = {}
        existing = ida_typeinf.tinfo_t()
        if existing.get_named_type(None, name):
            existing_data = ida_typeinf.udt_type_data_t()
            if existing.get_udt_details(existing_data):
                for member in existing_data:
                    off2membername[member.offset] = member.name

        for m in info.members:
            m: MemberDef
            member_d = ida_typeinf.udt_member_t()
            offset = m.offset * 8 # bits
            member_d.offset = offset

            if offset in off2membername:
                existing_name = off2membername[offset]
                if m.name != existing_name and _can_ovrd_member(m.name, existing_name):
                    self._print(2, f"Rename struct member: {existing_name} -> {m.name}")
                    member_d.name = m.name
                else:
                    self._print(2, f"Reuse struct member name: {existing_name}")
                    member_d.name = existing_name
            else:
                self._print(2, f"Add struct member: {m.name}")
                member_d.name = m.name

            if member_d.name == "__vtable":
                member_d.set_vftable()

            if m.is_base:
                member_d.set_baseclass()

            member_d.type = self._tyyaml2tinfo(m.tyyaml)
            _assert(member_d.type is not None, f"Failed to set union member type: {m.name}")
            member_size = member_d.type.get_size()
            _assert(member_size != ida_typeinf.BADSIZE, f"Failed to get union member size: {m.name}")
            member_d.size = member_size * 8 # bits
            udt.push_back(member_d)
        tinfo = ida_typeinf.tinfo_t()
        _assert(tinfo.create_udt(udt, ida_typeinf.BTF_STRUCT), f"Failed to create struct type: {name}")
        if tinfo.get_size() != info.size:
            # If size mismatch, try explicit tail padding
            self._print(1, f"Struct size mismatch, trying explicit tail padding")
            _explicit_tail_padding(udt, info.size)
            tinfo = ida_typeinf.tinfo_t()
            _assert(tinfo.create_udt(udt, ida_typeinf.BTF_STRUCT), f"Failed to create struct type: {name}")
            _assert(tinfo.get_size() != info.size, f"Struct size mismatch after tail padding: Actual: {tinfo.get_size()} != Expected: {info.size}")
            self._print(1, f"Struct size OK with explicit tail padding added")

        self._set_tinfo(name, tinfo)

    def _set_tinfo(self, name, tinfo):
        """Set a tinfo_t by name in IDA"""
        _assert(not name.startswith("("), f"Invalid name: {name}")
        self._print(1, f"Setting type: {name}")
        ret = tinfo.set_named_type(None, name, ida_typeinf.NTF_REPLACE)
        _assert(ret == ida_typeinf.TERR_OK, f"Failed to import type: {name}")

    def _create_placeholder(self, name, size, align):
        """Create a placeholder struct with the size and alignment, in case of recursion"""
        existing = ida_typeinf.tinfo_t()
        if existing.get_named_type(None, name):
            existing_data = ida_typeinf.udt_type_data_t()
            if existing.get_udt_details(existing_data):
                ok = True
                existing_size = existing.get_size()
                if existing_size != size:
                    self._print(2, f"Size mismatch: {name}, existing_size={existing_size}, size={size}")
                    ok = False
                if ok:
                    existing_sda = existing_data.sda
                    expected_sda = _align2sda(align)
                    if existing_sda != expected_sda:
                        self._print(2,f"SDA mismatch: {name}, expected={expected_sda}, actual={existing_sda}")
                        ok = False
                if ok:
                    self._print(1, f"Existing type: {name}, size={size}, align={align}")
                    return

        self._print(1,f"Creating placeholder type: {name}, size={size}, align={align}")
        storage_tinfo = ida_typeinf.tinfo_t(ida_typeinf.BTF_CHAR)
        _assert(storage_tinfo.create_array(storage_tinfo, size), f"Failed to create placeholder type: {name}")
        member = ida_typeinf.udt_member_t()
        member.name = "__placeholder"
        member.type = storage_tinfo
        member.offset = 0
        member.size = size * 8 # bits

        udt = ida_typeinf.udt_type_data_t()
        udt.taudt_bits |= ida_typeinf.TAUDT_CPPOBJ
        _set_udt_align(udt, align)
        udt.push_back(member)

        tinfo = ida_typeinf.tinfo_t()
        _assert(tinfo.create_udt(udt, ida_typeinf.BTF_STRUCT), f"Failed to create placeholder type: {name}")
        self._set_tinfo(name, tinfo)


class AddrImporter:
    """Address symbol importer context"""
    ti: TypeImporter
    upper = 0
    name_only = False

    def __init__(self, ti):
        self.ti = ti

    addr2func = {}
    addr2data = {}

    def set_upper(self, upper):
        self.upper = upper

    def add_func(self, addr, func):
        self.addr2func[self._fix_addr(addr)] = func

    def add_data(self, addr, data):
        self.addr2data[self._fix_addr(addr)] = data

    def run_import(self, name_only, name_pattern):
        self.name_only = name_only
        data_addrs = [addr for addr in self.addr2data if not name_pattern or name_pattern in self.addr2data[addr].name]
        func_addrs = [addr for addr in self.addr2func if not name_pattern or name_pattern in self.addr2func[addr].name]
        for addr in data_addrs:
            self._import_data(addr)
        for addr in func_addrs:
            self._import_func(addr)

    def _fix_addr(self, addr):
        return self.upper << 32 | (addr & 0xFFFFFFFF)

    def _import_data(self, addr):
        info: NameAndType = self.addr2data[addr]
        self.ti._print(0, f"Importing Data {hex(addr)}: {info.name}")
        if info.name:
            self._set_name(addr, info.name)
        if self.name_only:
            return
        if info.tyyaml:
            tinfo = self.ti._tyyaml2tinfo(info.tyyaml)
            self._set_tinfo(addr, tinfo)

    def _import_func(self, addr):
        info: FunctionDef = self.addr2func[addr]
        self.ti._print(0, f"Importing Function {hex(addr)}: {info.name}")
        if info.name:
            self._set_name(addr, info.name)
        if self.name_only:
            return

        func = ida_typeinf.func_type_data_t()
        has_existing = False
        existing_names = []
        existing_tinfo = ida_typeinf.tinfo_t()
        existing_func = ida_typeinf.func_type_data_t()
        if ida_nalt.get_tinfo(existing_tinfo, addr):
            if existing_tinfo.get_func_details(existing_func):
                has_existing = True
                for a in existing_func:
                    existing_names.append(a.name)

        reuse_names = len(existing_names) == len(info.args)
        if not reuse_names:
            self.ti._print(1, f"Not reusing names because arg count changed")

        if info.tyyaml:
            ret_tinfo = self.ti._tyyaml2tinfo(info.tyyaml)
            self.ti._print(1, f"Return type: {ret_tinfo}")
            func.rettype = ret_tinfo
            _assert(func.rettype is not None, f"Failed to set function return type")
        elif has_existing:
            t = ida_typeinf.tinfo_t(existing_func.rettype)
            self.ti._print(1, f"Keep existing return type: {t}")
            func.rettype = t
        else:
            self.ti._print(1, f"Use dummy return type u64")
            func.rettype = ida_typeinf.tinfo_t(ida_typeinf.BTF_INT64)

        for (i, arg) in enumerate(info.args):
            arg: NameAndType
            funcarg = ida_typeinf.funcarg_t()
            if reuse_names:
                existing_name = existing_names[i]
                new_name = arg.name
                if new_name != existing_name and _can_ovrd_member(new_name, existing_name):
                    self.ti._print(2, f"Rename arg {i}: {existing_name} -> {new_name}")
                    funcarg.name = new_name
                else:
                    self.ti._print(2, f"Reuse arg name {i}: {existing_name}")
                    funcarg.name = existing_name
            else:
                self.ti._print(2, f"Add arg {i}: {arg.name}")
                funcarg.name = arg.name

            if arg.tyyaml:
                t = self.ti._tyyaml2tinfo(arg.tyyaml)
                self.ti._print(2, f"Set arg {i} type: {t}")
                funcarg.type = t
                _assert(funcarg.type is not None, f"Failed to set func arg {i} type")
            elif has_existing and i < len(existing_func):
                t = ida_typeinf.tinfo_t(existing_func[i].type)
                self.ti._print(2, f"Keep existing arg {i} type: {t}")
                funcarg.type = t
            else:
                self.ti._print(2, f"Use dummy arg {i} type u64")
                funcarg.type = ida_typeinf.tinfo_t(ida_typeinf.BTF_INT64)
            func.push_back(funcarg)
        tinfo = ida_typeinf.tinfo_t()
        _assert(tinfo.create_func(func), f"Failed to create function type")
        self._set_tinfo(addr, tinfo)

    def _set_tinfo(self, addr, tinfo):
        """Set a tinfo_t by address in IDA"""
        _assert(ida_nalt.set_tinfo(addr, tinfo), f"Failed to set type for address: {hex(addr)}")

    def _set_name(self, addr, name):
        """Set the name of an address if it's more preferred than current name"""
        existing_name = ida_name.get_name(addr)
        if existing_name == name:
            return
        if _can_ovrd_symbol(name, existing_name):
            self.ti._print(1, f"Rename: {existing_name} -> {name}")
            ida_name.set_name(addr, name)


def _run_wrapped(func):
    try:
        func()
        _done()
    except Exception as e:
        print(f"[ukingidatool] Error: {e}")
        traceback.print_exception(e)
        _done()
        _print_depth(0, "There were errors!", 0)


if __name__ == "__main__":
    ti = TypeImporter()
    ai = AddrImporter(ti)
    def _run():
        ""
# Generated Scripts Below
