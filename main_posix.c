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
	str8 file = os_map_file((u8 *)argv[0]);
	elfinspect(file);
	return 0;
}
