/* See LICENSE for license details. */
#include <stdint.h>
#include <stddef.h>

typedef uint8_t    u8;
typedef uint16_t  u16;
typedef uint32_t  u32;
typedef uint32_t  b32;
typedef uint64_t  u64;
typedef int8_t     s8;
typedef int16_t   s16;
typedef int32_t   s32;
typedef int64_t   s64;
typedef ptrdiff_t  iz;

#include <stdio.h>

#include "elfinspect.c"

#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

function Arena
os_arena_new(iz size)
{
	Arena result = {0};
	iz page_size = sysconf(_SC_PAGESIZE);
	if (size % page_size) size += page_size - (size % page_size);
	u8 *mem = mmap(0, size, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);
	if (mem != MAP_FAILED) {
		result.beg = mem;
		result.end = mem + size;
	}
	return result;
}

function str8
os_map_file(u8 *name)
{
	str8 result = {0};
	s32 fd = open((char *)name, O_RDONLY);
	struct stat sb;
	if (fd != -1 && fstat(fd, &sb) != -1) {
		result.data = mmap(0, sb.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
		if (result.data != MAP_FAILED)
			result.len = sb.st_size;
	}
	if (fd != -1) close(fd);
	return result;
}

s32
main(s32 argc, char *argv[])
{
	Arena arena = os_arena_new(MB(32));
	str8 file   = os_map_file((u8 *)argv[0]);
	elfinspect(arena, file);
	return 0;
}
