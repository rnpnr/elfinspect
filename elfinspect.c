#define local_persist static
#define global        static
#define function      static

/* TODO(rnp) platform specific */
#define ASSERT(c) do { if (!(c)) asm("int3"); } while (0)
#define TODO(c) ASSERT(c)

#define countof(a) (sizeof(a) / sizeof(*a))

#define static_assert(c, msg) _Static_assert(c, msg)

#define KB(a) ((u64)a << 10ULL)
#define MB(a) ((u64)a << 20ULL)

typedef struct {u8 *beg, *end;} Arena;

#define str8(s) (str8){.len = countof(s) - 1, .data = (u8 *)s}
typedef struct { iz len; u8 *data;} str8;

/* NOTE: platform api; defined here so that other platform symbols are not visible to this TU */
function str8 os_map_file(u8 *);

/* X(ctype, name) */
#define ELF_HEADER_MEMBERS(ptrsize) \
	X(u16,     type)      \
	X(u16,     machine)   \
	X(u32,     version)   \
	X(ptrsize, entry)     \
	X(ptrsize, phoff)     \
	X(ptrsize, shoff)     \
	X(u32,     flags)     \
	X(u16,     ehsize)    \
	X(u16,     phentsize) \
	X(u16,     phnum)     \
	X(u16,     shentsize) \
	X(u16,     shnum)     \
	X(u16,     shstrndx)

#define X(ctype, name) ctype name;
typedef struct { u8 identifier[16]; ELF_HEADER_MEMBERS(u32) } ELFHeader32;
typedef struct { u8 identifier[16]; ELF_HEADER_MEMBERS(u64) } ELFHeader64;
#undef X

typedef enum {
	EK_NONE = 0x00,
	EK_32   = 0x01,
	EK_64   = 0x02,
} ELFKind;

typedef struct {
	union {
		ELFHeader64 eh64;
		ELFHeader32 eh32;
	};
	ELFKind kind;
} ELFHeader;

typedef enum {
	EEK_NONE   = 0x00,
	EEK_LITTLE = 0x01,
	EEK_BIG    = 0x02,
} ELFEndianKind;

typedef enum {
	ESK_NULL         = 0,
	ESK_PROGBITS     = 1,
	ESK_SYMBOL_TABLE = 2,
	ESK_STR_TABLE    = 3,
	ESK_RELA         = 4,
	ESK_HASH         = 5,
	ESK_DYNAMIC      = 6,
	ESK_NOTE         = 7,
	ESK_NOBITS       = 8,
	ESK_REL          = 9,
	ESK_SHLIB        = 10,
	ESK_DYNSYM       = 11,
	ESK_INIT_ARRAY   = 14,
	ESK_FINI_ARRAY   = 15,
	ESK_PREINIT_ARR  = 16,
	ESK_GROUP        = 17,
	ESK_SYMTAB_SHND  = 18,
	ESK_RELR         = 19,
	ESK_NUM          = 20,
	ESK_LOPROC       = 0x70000000,
	ESK_HIPROC       = 0x7fffffff,
	ESK_LOUSER       = 0x80000000,
	ESK_HIUSER       = 0x8fffffff,
} ELFSectionKind;
static_assert(sizeof(ELFSectionKind) == 4, "sizeof(ELFSectionKind) must be 4 bytes");

/* X(ctype, name) */
#define ELF_SECTION_HEADER_MEMBERS(ptrsize) \
	X(u32,            name_table_offset) \
	X(ELFSectionKind, kind)              \
	X(ptrsize,        flags)             \
	X(ptrsize,        addr)              \
	X(ptrsize,        offset)            \
	X(ptrsize,        size)              \
	X(u32,            link)              \
	X(u32,            info)              \
	X(ptrsize,        addralign)         \
	X(ptrsize,        entsize)

#define X(ctype, name) ctype name;
typedef struct {ELF_SECTION_HEADER_MEMBERS(u32)} ELFSectionHeader32;
typedef struct {ELF_SECTION_HEADER_MEMBERS(u64)} ELFSectionHeader64;
#undef X

typedef struct {
	str8 name;
	union {
		ELFSectionHeader64 sh64;
		ELFSectionHeader32 sh32;
	};
} ELFSectionHeader;

typedef struct {
	ELFSectionHeader *header;
	str8 store;
} ELFSection;

typedef enum {
	DUT_UNKNOWN       = 0x00,
	DUT_COMPILE       = 0x01,
	DUT_TYPE          = 0x02,
	DUT_PARTIAL       = 0x03,
	DUT_SKELETON      = 0x04,
	DUT_SPLIT_COMPILE = 0x05,
	DUT_SPLIT_TYPE    = 0x06,
	DUT_LO_USER       = 0x80,
	DUT_HI_USER       = 0xff
} DWARFUnitKind;

typedef struct {
	u64 length;
	u64 abbreviation_offset;
	DWARFUnitKind kind;
	u16 version;
	u8  address_size;
	u8  dwarf_64;
} DWARFUnitHeader;

typedef enum {
	DATK_SIBLING                 = 0x01,
	DATK_LOCATION                = 0x02,
	DATK_NAME                    = 0x03,
	DATK_ORDERING                = 0x09,
	DATK_BYTE_SIZE               = 0x0b,
	DATK_BIT_OFFSET              = 0x0c,
	DATK_BIT_SIZE                = 0x0d,
	DATK_STMT_LIST               = 0x10,
	DATK_LOW_PC                  = 0x11,
	DATK_HIGH_PC                 = 0x12,
	DATK_LANGUAGE                = 0x13,
	DATK_DISCR                   = 0x15,
	DATK_DISCR_VALUE             = 0x16,
	DATK_VISIBILITY              = 0x17,
	DATK_IMPORT                  = 0x18,
	DATK_STRING_LENGTH           = 0x19,
	DATK_COMMON_REFERENCE        = 0x1a,
	DATK_COMP_DIR                = 0x1b,
	DATK_CONST_VALUE             = 0x1c,
	DATK_CONTAINING_TYPE         = 0x1d,
	DATK_DEFAULT_VALUE           = 0x1e,
	DATK_INLINE                  = 0x20,
	DATK_IS_OPTIONAL             = 0x21,
	DATK_LOWER_BOUND             = 0x22,
	DATK_PRODUCER                = 0x25,
	DATK_PROTOTYPED              = 0x27,
	DATK_RETURN_ADDR             = 0x2a,
	DATK_START_SCOPE             = 0x2c,
	DATK_BIT_STRIDE              = 0x2e,
	DATK_UPPER_BOUND             = 0x2f,
	DATK_ABSTRACT_ORIGIN         = 0x31,
	DATK_ACCESSIBILITY           = 0x32,
	DATK_ADDRESS_CLASS           = 0x33,
	DATK_ARTIFICIAL              = 0x34,
	DATK_BASE_TYPES              = 0x35,
	DATK_CALLING_CONVENTION      = 0x36,
	DATK_COUNT                   = 0x37,
	DATK_DATA_MEMBER_LOCATION    = 0x38,
	DATK_DECL_COLUMN             = 0x39,
	DATK_DECL_FILE               = 0x3a,
	DATK_DECL_LINE               = 0x3b,
	DATK_DECLARATION             = 0x3c,
	DATK_DISCR_LIST              = 0x3d,
	DATK_ENCODING                = 0x3e,
	DATK_EXTERNAL                = 0x3f,
	DATK_FRAME_BASE              = 0x40,
	DATK_FRIEND                  = 0x41,
	DATK_IDENTIFIER_CASE         = 0x42,
	DATK_MACRO_INFO              = 0x43,
	DATK_NAMELIST_ITEM           = 0x44,
	DATK_PRIORITY                = 0x45,
	DATK_SEGMENT                 = 0x46,
	DATK_SPECIFICATION           = 0x47,
	DATK_STATIC_LINK             = 0x48,
	DATK_TYPE                    = 0x49,
	DATK_USE_LOCATION            = 0x4a,
	DATK_VARIABLE_PARAMETER      = 0x4b,
	DATK_VIRTUALITY              = 0x4c,
	DATK_VTABLE_ELEM_LOCATION    = 0x4d,
	DATK_ALLOCATED               = 0x4e,
	DATK_ASSOCIATED              = 0x4f,
	DATK_DATA_LOCATION           = 0x50,
	DATK_BYTE_STRIDE             = 0x51,
	DATK_ENTRY_PC                = 0x52,
	DATK_USE_UTF8                = 0x53,
	DATK_EXTENSION               = 0x54,
	DATK_RANGES                  = 0x55,
	DATK_TRAMPOLINE              = 0x56,
	DATK_CALL_COLUMN             = 0x57,
	DATK_CALL_FILE               = 0x58,
	DATK_CALL_LINE               = 0x59,
	DATK_DESCRIPTION             = 0x5a,
	DATK_BINARY_SCALE            = 0x5b,
	DATK_DECIMAL_SCALE           = 0x5c,
	DATK_SMALL                   = 0x5d,
	DATK_DECIMAL_SIGN            = 0x5e,
	DATK_DIGIT_COUNT             = 0x5f,
	DATK_PICTURE_STRING          = 0x60,
	DATK_MUTABLE                 = 0x61,
	DATK_THREADS_SCALED          = 0x62,
	DATK_EXPLICIT                = 0x63,
	DATK_OBJECT_POINTER          = 0x64,
	DATK_ENDIANITY               = 0x65,
	DATK_ELEMENTAL               = 0x66,
	DATK_PURE                    = 0x67,
	DATK_RECURSIVE               = 0x68,
	DATK_SIGNATURE               = 0x69,
	DATK_MAIN_SUBPROGRAM         = 0x6a,
	DATK_DATA_BIT_OFFSET         = 0x6b,
	DATK_CONST_EXPR              = 0x6c,
	DATK_ENUM_CLASS              = 0x6d,
	DATK_LINKAGE_NAME            = 0x6e,
	DATK_STRING_LENGTH_BIT_SIZE  = 0x6f,
	DATK_STRING_LENGTH_BYTE_SIZE = 0x70,
	DATK_RANK                    = 0x71,
	DATK_STR_OFFSETS_BASE        = 0x72,
	DATK_ADDR_BASE               = 0x73,
	DATK_RNGLISTS_BASE           = 0x74,
	DATK_DWO_NAME                = 0x76,
	DATK_REFERENCE               = 0x77,
	DATK_RVALUE_REFERENCE        = 0x78,
	DATK_MACROS                  = 0x79,
	DATK_CALL_ALL_CALLS          = 0x7a,
	DATK_CALL_ALL_SOURCE_CALLS   = 0x7b,
	DATK_CALL_ALL_TAIL_CALLS     = 0x7c,
	DATK_CALL_RETURN_PC          = 0x7d,
	DATK_CALL_VALUE              = 0x7e,
	DATK_CALL_ORIGIN             = 0x7f,
	DATK_CALL_PARAMETER          = 0x80,
	DATK_CALL_PC                 = 0x81,
	DATK_CALL_TAIL_CALL          = 0x82,
	DATK_CALL_TARGET             = 0x83,
	DATK_CALL_TARGET_CLOBBERED   = 0x84,
	DATK_CALL_DATA_LOCATION      = 0x85,
	DATK_CALL_DATA_VALUE         = 0x86,
	DATK_NORETURN                = 0x87,
	DATK_ALIGNMENT               = 0x88,
	DATK_EXPORT_SYMBOLS          = 0x89,
	DATK_DELETED                 = 0x8a,
	DATK_DEFAULTED               = 0x8b,
	DATK_LOCLISTS_BASE           = 0x8c,
	DATK_LO_USER                 = 0x2000,
	DATK_HI_USER                 = 0x3fff,
} DWARFAttributeKind;

typedef enum {
	DFK_ADDR           = 0x01,
	DFK_BLOCK2         = 0x03,
	DFK_BLOCK4         = 0x04,
	DFK_DATA2          = 0x05,
	DFK_DATA4          = 0x06,
	DFK_DATA8          = 0x07,
	DFK_STRING         = 0x08,
	DFK_BLOCK          = 0x09,
	DFK_BLOCK1         = 0x0a,
	DFK_DATA1          = 0x0b,
	DFK_FLAG           = 0x0c,
	DFK_SDATA          = 0x0d,
	DFK_STRP           = 0x0e,
	DFK_UDATA          = 0x0f,
	DFK_REF_ADDR       = 0x10,
	DFK_REF1           = 0x11,
	DFK_REF2           = 0x12,
	DFK_REF4           = 0x13,
	DFK_REF8           = 0x14,
	DFK_REF_UDATA      = 0x15,
	DFK_INDIRECT       = 0x16,
	DFK_SEC_OFFSET     = 0x17,
	DFK_EXPRLOC        = 0x18,
	DFK_FLAG_PRESENT   = 0x19,
	DFK_STRX           = 0x1a,
	DFK_ADDRX          = 0x1b,
	DFK_REF_SUP4       = 0x1c,
	DFK_STRP_SUP       = 0x1d,
	DFK_DATA16         = 0x1e,
	DFK_LINE_STRP      = 0x1f,
	DFK_REF_SIG8       = 0x20,
	DFK_IMPLICIT_CONST = 0x21,
	DFK_LOCLISTX       = 0x22,
	DFK_RNGLISTX       = 0x23,
	DFK_REF_SUP8       = 0x24,
	DFK_STRX1          = 0x25,
	DFK_STRX2          = 0x26,
	DFK_STRX3          = 0x27,
	DFK_STRX4          = 0x28,
	DFK_ADDRX1         = 0x29,
	DFK_ADDRX2         = 0x2a,
	DFK_ADDRX3         = 0x2b,
	DFK_ADDRX4         = 0x2c,
} DWARFFormKind;

typedef struct {
	DWARFAttributeKind kind;
	DWARFFormKind      form_kind;
} DWARFAttribute;

typedef enum {
	DAK_ARRAY_TYPE               = 0x01,
	DAK_CLASS_TYPE               = 0x02,
	DAK_ENTRY_POINT              = 0x03,
	DAK_ENUMERATION_TYPE         = 0x04,
	DAK_FORMAL_PARAMETER         = 0x05,
	DAK_IMPORTED_DECLARATION     = 0x08,
	DAK_LABEL                    = 0x0a,
	DAK_LEXICAL_BLOCK            = 0x0b,
	DAK_MEMBER                   = 0x0d,
	DAK_POINTER_TYPE             = 0x0f,
	DAK_REFERENCE_TYPE           = 0x10,
	DAK_COMPILE_UNIT             = 0x11,
	DAK_STRING_TYPE              = 0x12,
	DAK_STRUCTURE_TYPE           = 0x13,
	DAK_SUBROUTINE_TYPE          = 0x15,
	DAK_TYPEDEF                  = 0x16,
	DAK_UNION_TYPE               = 0x17,
	DAK_UNSPECIFIED_PARAMETERS   = 0x18,
	DAK_VARIANT                  = 0x19,
	DAK_COMMON_BLOCK             = 0x1a,
	DAK_COMMON_INCLUSION         = 0x1b,
	DAK_INHERITANCE              = 0x1c,
	DAK_INLINED_SUBROUTINE       = 0x1d,
	DAK_MODULE                   = 0x1e,
	DAK_PTR_TO_MEMBER_TYPE       = 0x1f,
	DAK_SET_TYPE                 = 0x20,
	DAK_SUBRANGE_TYPE            = 0x21,
	DAK_WITH_STMT                = 0x22,
	DAK_ACCESS_DECLARATION       = 0x23,
	DAK_BASE_TYPE                = 0x24,
	DAK_CATCH_BLOCK              = 0x25,
	DAK_CONST_TYPE               = 0x26,
	DAK_CONSTANT                 = 0x27,
	DAK_ENUMERATOR               = 0x28,
	DAK_FILE_TYPE                = 0x29,
	DAK_FRIEND                   = 0x2a,
	DAK_NAMELIST                 = 0x2b,
	DAK_NAMELIST_ITEM            = 0x2c,
	DAK_PACKED_TYPE              = 0x2d,
	DAK_SUBPROGRAM               = 0x2e,
	DAK_TEMPLATE_TYPE_PARAMETER  = 0x2f,
	DAK_TEMPLATE_VALUE_PARAMETER = 0x30,
	DAK_THROWN_TYPE              = 0x31,
	DAK_TRY_BLOCK                = 0x32,
	DAK_VARIANT_PART             = 0x33,
	DAK_VARIABLE                 = 0x34,
	DAK_VOLATILE_TYPE            = 0x35,
	DAK_DWARF_PROCEDURE          = 0x36,
	DAK_RESTRICT_TYPE            = 0x37,
	DAK_INTERFACE_TYPE           = 0x38,
	DAK_NAMESPACE                = 0x39,
	DAK_IMPORTED_MODULE          = 0x3a,
	DAK_UNSPECIFIED_TYPE         = 0x3b,
	DAK_PARTIAL_UNIT             = 0x3c,
	DAK_IMPORTED_UNIT            = 0x3d,
	DAK_CONDITION                = 0x3f,
	DAK_SHARED_TYPE              = 0x40,
	DAK_TYPE_UNIT                = 0x41,
	DAK_RVALUE_REFERENCE_TYPE    = 0x42,
	DAK_TEMPLATE_ALIAS           = 0x43,
	DAK_COARRAY_TYPE             = 0x44,
	DAK_GENERIC_SUBRANGE         = 0x45,
	DAK_DYNAMIC_TYPE             = 0x46,
	DAK_ATOMIC_TYPE              = 0x47,
	DAK_CALL_SITE                = 0x48,
	DAK_CALL_SITE_PARAMETER      = 0x49,
	DAK_SKELETON_UNIT            = 0x4a,
	DAK_IMMUTABLE_TYPE           = 0x4b,
	DAK_LO_USER                  = 0x4080,
	DAK_HI_USER                  = 0xffff
} DWARFAbbreviationKind;

typedef struct DWARFAbbreviation {
	DWARFAbbreviationKind kind;
	b32    has_children;
	u64    abbreviation_code;

	DWARFAttribute *data;
	s16             count;
	s16             capacity;

	struct DWARFAbbreviation *next;
} DWARFAbbreviation;

#define zero_struct(s) mem_clear(s, 0, sizeof(*s));
function void *
mem_clear(void *restrict _s, u8 byte, iz size)
{
	u8 *s = _s;
	for (iz i = 0; i < size; i++)
		s[i] = byte;
	return s;
}

function void *
mem_copy(void *restrict dest, void *restrict src, iz size)
{
	u8 *s = src, *d = dest;
	for (iz i = 0; i < size; i++)
		d[i] = s[i];
	return dest;
}

#define alloc(a, t, c) (t *)alloc_(a, _Alignof(t), sizeof(t), c)
function void *
alloc_(Arena *a, iz alignment, iz size, iz count)
{
	iz capacity = a->end - a->beg;
	iz padding  = -(uintptr_t)a->beg & alignment;
	if ((capacity - padding) / size  < count) {
		ASSERT(0 && "OOM: buy more ram lol");
	}
	u8 *result = a->beg + padding;
	a->beg += padding + size * count;
	return mem_clear(result, 0, size * count);
}

enum { DA_INITIAL_CAP = 4 };
#define da_reserve(a, s, n) \
  (s)->data = da_reserve_((a), (s)->data, &(s)->capacity, (s)->count + n, \
                          _Alignof(typeof(*(s)->data)), sizeof(*(s)->data))

#define da_push(a, s) \
  ((s)->count == (s)->capacity  \
    ? da_reserve(a, s, 1),      \
      (s)->data + (s)->count++  \
    : (s)->data + (s)->count++)

function void *
da_reserve_(Arena *a, void *data, s16 *capacity, iz needed, iz align, iz size)
{
	iz cap = *capacity;

	/* NOTE(rnp): handle both 0 initialized DAs and DAs that need to be moved (they started
	 * on the stack or someone allocated something in the middle of the arena during usage) */
	if (!data || a->beg != (u8 *)data + cap * size) {
		void *copy = alloc_(a, size, align, cap);
		if (data) mem_copy(copy, data, cap * size);
		data = copy;
	}

	if (!cap) cap = DA_INITIAL_CAP;
	while (cap < needed) cap *= 2;
	alloc_(a, size, align, cap - *capacity);
	*capacity = cap;
	return data;
}

function str8
c_str_to_str8(u8 *c_str)
{
	str8 result = {.data = c_str};
	if (c_str) while (*c_str) c_str++;
	result.len = c_str - result.data;
	return result;
}

function b32
str8_equal(str8 a, str8 b)
{
	b32 result = a.len == b.len;
	for (iz i = 0; result && i < a.len; i++)
		result &= a.data[i] == b.data[i];
	return result;
}

function str8
str8_chop(str8 a, iz length)
{
	str8 result = {0};
	if (length < a.len) {
		result.data = a.data + length;
		result.len  = a.len  - length;
	}
	return result;
}

function iz
str8_read_uleb128(str8 a, u64 *uleb128)
{
	/* TODO(rnp): check for overflow ... */
	iz result = 0;
	iz shift  = 0;
	u64 n     = 0;
	while (result < a.len) {
		u8 byte = a.data[result++];
		n |= (u64)(byte & 0x7F) << shift;
		if ((byte & 0x80) == 0)
			break;
		shift += 7;
	}
	*uleb128 = n;
	return result;
}

function void
print_u64(u64 v)
{
	printf("%zu", v);
}

function void print_u16(u16 v) { print_u64(v); }
function void print_u32(u32 v) { print_u64(v); }

function void
print_elf_header_32(ELFHeader *eh)
{
	ASSERT(eh->kind == EK_32);
	printf("TODO: print 32 bit elf header\n");
}

function void
print_elf_header_64(ELFHeader *eh)
{
	ASSERT(eh->kind == EK_64);
	printf("ELF Header:\nidentifier: 0x");
	for (u32 i = 0; i < countof(eh->eh64.identifier); i++)
		printf(" %02x", eh->eh64.identifier[i]);
	#define X(ctype, name) printf("\n"); printf(#name ": "); print_##ctype(eh->eh64.name);
	ELF_HEADER_MEMBERS(u64)
	#undef X
	printf("\n");
}

function void
print_elf_header(ELFHeader *eh)
{
	switch (eh->kind) {
	case EK_32: print_elf_header_32(eh);     break;
	case EK_64: print_elf_header_64(eh);     break;
	default: printf("invalid elf header\n"); break;
	}
}

function b32
is_elf(str8 file)
{
	b32 result = file.len >= 16;
	result &= file.data[0] == 0x7F;
	result &= file.data[1] == 'E';
	result &= file.data[2] == 'L';
	result &= file.data[3] == 'F';
	return result;
}

function ELFHeader
elf_header_from_file(str8 file)
{
	ELFHeader result = {0};
	ASSERT(file.len > sizeof(ELFHeader64));
	ASSERT(file.data[4] == EK_64);
	ASSERT(file.data[5] == EEK_LITTLE);

	result.eh64 = *(ELFHeader64 *)file.data;
	result.kind = file.data[4];

	return result;
}

function ELFSectionHeader *
elf_extract_section_headers(Arena *a, str8 file, ELFHeader *eh)
{
	TODO(eh->kind == EK_64);
	TODO(eh->eh64.identifier[5] == EEK_LITTLE);
	TODO(file.len >= eh->eh64.shoff + eh->eh64.shentsize * eh->eh64.shnum);
	u32 sections = eh->eh64.shnum;
	ELFSectionHeader *result = alloc(a, ELFSectionHeader, sections);
	for (u32 i = 0; i < sections; i++) {
		iz offset = eh->eh64.shoff + eh->eh64.shentsize * i;
		result[i].sh64 = *(ELFSectionHeader64 *)(file.data + offset);
	}

	u8 *str_tab = file.data + result[eh->eh64.shstrndx].sh64.offset;
	for (u32 i = 0; i < sections; i++)
		result[i].name = c_str_to_str8(str_tab + result[i].sh64.name_table_offset);

	return result;
}

function ELFSection
elf_lookup_section(str8 name, str8 file, ELFSectionHeader *shs, u32 sections_count)
{
	ELFSection result = {0};
	for (u32 i = 0; i < sections_count; i++) {
		if (str8_equal(shs[i].name, name)) {
			result.header     = shs + i;
			result.store.data = file.data + shs[i].sh64.offset;
			result.store.len  = shs[i].sh64.size;
			break;
		}
	}
	return result;
}

function iz
dwarf_read_unit_header(DWARFUnitHeader *duh, str8 store)
{
	iz result = 0;
	/* TODO(rnp): context containing endianess, dwarf size */
	duh->dwarf_64 = *(u32 *)store.data == 0xffffffff;
	if (duh->dwarf_64) { result += 12; duh->length = *(u64 *)(store.data + 4); }
	else               { result +=  4; duh->length = *(u32 *)(store.data);     }
	duh->version = *(u16 *)(store.data + result);
	result += 2;
	if (duh->version == 5) duh->kind = store.data[result++];
	duh->address_size = store.data[result++];
	if (duh->dwarf_64) { duh->abbreviation_offset = *(u64 *)(store.data + result); result += 8; }
	else               { duh->abbreviation_offset = *(u32 *)(store.data + result); result += 4; }
	return result;
}

function iz
dwarf_parse_abbrevation(Arena *a, DWARFAbbreviation *abbrv, str8 table)
{
	iz table_start_size = table.len;
	iz result = 0;
	table = str8_chop(table, str8_read_uleb128(table, &abbrv->abbreviation_code));
	u64 abbrv_kind = 0;
	table = str8_chop(table, str8_read_uleb128(table, &abbrv_kind));
	abbrv->kind = abbrv_kind;
	if (table.len < 1) return table_start_size;
	abbrv->has_children = *table.data;
	table = str8_chop(table, 1);
	for (;;) {
		u64 attr_kind = 0, form_kind = 0;
		table = str8_chop(table, str8_read_uleb128(table, &attr_kind));
		table = str8_chop(table, str8_read_uleb128(table, &form_kind));
		TODO(form_kind != DFK_INDIRECT);
		TODO(form_kind != DFK_IMPLICIT_CONST);
		if (attr_kind) {
			*da_push(a, abbrv) = (DWARFAttribute){attr_kind, form_kind};
		} else {
			ASSERT(form_kind == 0);
			break;
		}
	}
	result = table_start_size - table.len;
	return result;
}

function DWARFAbbreviation
dwarf_lookup_abbreviation(Arena *a, str8 table, u64 key)
{
	DWARFAbbreviation result = {0};
	while (key != result.abbreviation_code && table.len > 1) {
		result.count = 0;
		table = str8_chop(table, dwarf_parse_abbrevation(a, &result, table));
	}
	return result;
}

function b32
elfinspect(Arena arena, str8 file)
{
	b32 result = is_elf(file);

	if (result) {
		ELFHeader header = elf_header_from_file(file);
		ELFSectionHeader *sections = elf_extract_section_headers(&arena, file, &header);
		print_elf_header(&header);
		printf("\nSections:\n");
		for (u32 i = 0; i < header.eh64.shnum; i++) {
			printf("[%u]:", i);
			str8 name = sections[i].name;
			if (name.len) printf(" %.*s", (s32)name.len, name.data);
			printf("\n");
		}

		ELFSection debug_info  = elf_lookup_section(str8(".debug_info"), file,
		                                            sections, header.eh64.shnum);
		ELFSection debug_abbrv = elf_lookup_section(str8(".debug_abbrev"), file,
		                                            sections, header.eh64.shnum);
		ELFSection debug_str_offsets = elf_lookup_section(str8(".debug_str_offsets"), file,
		                                                  sections, header.eh64.shnum);
		ELFSection debug_str = elf_lookup_section(str8(".debug_str"), file,
		                                          sections, header.eh64.shnum);

		str8 d_info_reader = debug_info.store;
		DWARFUnitHeader d_info_header = {0};
		d_info_reader = str8_chop(d_info_reader, dwarf_read_unit_header(&d_info_header, d_info_reader));
		u64 abbreviation_code = 0;
		str8 abbreviation_table = str8_chop(debug_abbrv.store, d_info_header.abbreviation_offset);
		d_info_reader = str8_chop(d_info_reader, str8_read_uleb128(d_info_reader, &abbreviation_code));
		DWARFAbbreviation abbrv = dwarf_lookup_abbreviation(&arena, abbreviation_table, abbreviation_code);

		printf("\nFirst DWARF DIE:\n");
		for (s16 i = 0; i < abbrv.count; i++) {
			DWARFAttribute attr = abbrv.data[i];
			if (attr.kind == 0)
				continue;

			switch (attr.kind) {
			case DATK_PRODUCER:      printf("producer:      "); break;
			case DATK_LANGUAGE:      printf("language:      "); break;
			case DATK_ADDR_BASE:     printf("addr base:     "); break;
			case DATK_LOCLISTS_BASE: printf("loc list base: "); break;
			default: ASSERT(0); break;
			}

			switch (attr.form_kind) {
			case DFK_STRX1: {
				u32 str_offset_offset = *d_info_reader.data;
				d_info_reader = str8_chop(d_info_reader, 1);
				u32 str_offset = *(u32 *)(debug_str_offsets.store.data + str_offset_offset);
				printf("%s", (char *)debug_str.store.data + str_offset);
			} break;
			case DFK_DATA2: {
				u32 data = *(u16 *)d_info_reader.data;
				d_info_reader = str8_chop(d_info_reader, 2);
				printf("%u", data);
			} break;
			case DFK_SEC_OFFSET: {
				u32 data = *(u32 *)d_info_reader.data;
				d_info_reader = str8_chop(d_info_reader, 4);
				printf("%u", data);
			} break;
			default: ASSERT(0); break;
			}
			printf("\n");
		}

		result = 1;
	}

	return result;
}
