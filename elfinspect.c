#define local_persist static
#define global        static
#define function      static

/* TODO(rnp) platform specific */
#define ASSERT(c) do { if (!(c)) asm("int3"); } while (0)
#define TODO(c) ASSERT(c)

#define countof(a) (sizeof(a) / sizeof(*a))

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

/* X(ctype, name) */
#define ELF_SECTION_HEADER_MEMBERS(ptrsize) \
	X(u32,     name_table_offset) \
	X(u32,     kind)              \
	X(ptrsize, flags)             \
	X(ptrsize, addr)              \
	X(ptrsize, offset)            \
	X(ptrsize, size)              \
	X(u32,     link)              \
	X(u32,     info)              \
	X(ptrsize, addralign)         \
	X(ptrsize, entsize)

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

#define zero_struct(s) mem_clear(s, 0, sizeof(*s));
function void *
mem_clear(void *restrict _s, u8 byte, iz size)
{
	u8 *s = _s;
	for (iz i = 0; i < size; i++)
		s[i] = byte;
	return s;
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

function str8
c_str_to_str8(u8 *c_str)
{
	str8 result = {.data = c_str};
	if (c_str) while (*c_str) c_str++;
	result.len = c_str - result.data;
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

function b32
elfinspect(Arena arena, str8 file)
{
	b32 result = 0;

	if (is_elf(file)) {
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
		result = 1;
	}

	return result;
}
