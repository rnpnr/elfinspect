# elfinspect

work in progress [ELF][] and [DWARF][] binary inspector

## Dependencies

- `C11` Compiler (with support for inline assembly)
- POSIX Compliant C-Runtime

## Usage

Currently the program only supports reading itself and dumping its
headers to `stdout`.

[ELF]:   https://www.man7.org/linux/man-pages/man5/elf.5.html
[DWARF]: https://dwarfstd.org/doc/DWARF5.pdf
