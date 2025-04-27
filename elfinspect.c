#define local_persist static
#define global        static
#define function      static

/* TODO(rnp) platform specific */
#define ASSERT(c) do { if (!(c)) asm("int3"); } while (0)

#define countof(a) (sizeof(a) / sizeof(*a))

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
	EHK_NONE = 0x00,
	EHK_32   = 0x01,
	EHK_64   = 0x02,
} ELFHeaderKind;

typedef struct {
	union {
		ELFHeader64 eh64;
		ELFHeader32 eh32;
	} u;
	ELFHeaderKind kind;
} ELFHeader;

typedef enum {
	EEK_NONE   = 0x00,
	EEK_LITTLE = 0x01,
	EEK_BIG    = 0x02,
} ELFEndianKind;

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
	ASSERT(eh->kind == EHK_32);
	printf("TODO: print 32 bit elf header\n");
}

function void
print_elf_header_64(ELFHeader *eh)
{
	ASSERT(eh->kind == EHK_64);
	printf("ELF Header:\nidentifier: 0x");
	for (u32 i = 0; i < countof(eh->u.eh64.identifier); i++)
		printf(" %02x", eh->u.eh64.identifier[i]);
	#define X(ctype, name) printf("\n"); printf(#name ": "); print_##ctype(eh->u.eh64.name);
	ELF_HEADER_MEMBERS(u64)
	#undef X
	printf("\n");
}

function void
print_elf_header(ELFHeader *eh)
{
	switch (eh->kind) {
	case EHK_32: print_elf_header_32(eh);    break;
	case EHK_64: print_elf_header_64(eh);    break;
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
	ASSERT(file.data[4] == EHK_64);
	ASSERT(file.data[5] == EEK_LITTLE);

	result.u.eh64 = *(ELFHeader64 *)file.data;
	result.kind   = file.data[4];

	return result;
}

function b32
elfinspect(str8 file)
{
	b32 result = 0;

	if (is_elf(file)) {
		ELFHeader header = elf_header_from_file(file);
		print_elf_header(&header);
		result = 1;
	}

	return result;
}
