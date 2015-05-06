#include "objfile/linux.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "arith.h"
#include "databuf.h"
#include "objfile/structs.h"
#include "util.h"

enum { kEI_NIDENT = 16 };

PACK_PUSH
struct elf32_Header {
  uint8_t e_ident[kEI_NIDENT];
  /* Type: Relocatable, executable, etc. */
  uint16_t e_type;
  /* Machine: 386, amd64, etc. */
  uint16_t e_machine;
  /* Version, should be "EV_CURRENT". */
  uint32_t e_version;
  /* Entry point (for executables). */
  uint32_t e_entry;
  /* Program header offset, zero for .o files. */
  uint32_t e_phoff;
  /* Section header offset. */
  uint32_t e_shoff;
  /* Flags, none defined. (Zero.) */
  uint32_t e_flags;
  /* Size of this elf header. */
  uint16_t e_ehsize;
  /* Size of a program header entry.  (Zero for .o files?) */
  uint16_t e_phentsize;
  /* Number of program header entries.  (Zero for .o files.) */
  uint16_t e_phnum;
  /* Size of section header entry. */
  uint16_t e_shentsize;
  /* Number of section headers. */
  uint16_t e_shnum;
  /* Section header table index of the entry associated with the
  section name string table.  Or SHN_UNDEF, if the file has no section
  name string table.  Must be less than SHN_LORESERVE (0xff00),
  otherwise the value is stored elsewhere.*/
  uint16_t e_shstrndx;
};
PACK_POP

PACK_PUSH
struct elf32_Section_Header {
  /* An index into the section header string table section. */
  uint32_t sh_name;
  /* SHT_PROGBITS for "information defined by the program", SHT_SYMTAB
  for a symbol table, SHT_STRTAB for a string table, SHT_RELA for
  "relocation entries with explicit addends", SHT_REL for "relocation
  offsets without explicit addends", SHT_NOBITS for .bss section. */
  uint32_t sh_type;
  /* flags: SHF_WRITE: data that should be writable during process
  execution.  SHF_ALLOC: This secton occupies memory during process
  execution.  SHF_EXECINSTR: This section contains executable machine
  instructions. */
  uint32_t sh_flags;
  /* The address at which the section's first byte should reside?  So,
  zero for .o files? */
  uint32_t sh_addr;
  /* File offset of the section. */
  uint32_t sh_offset;
  /* Section's size in bytes. */
  uint32_t sh_size;
  /* A "section header table index link." */
  uint32_t sh_link;
  /* Extra info. */
  uint32_t sh_info;
  /* Section alignment constraint.  0 means the same as 1. */
  uint32_t sh_addralign;
  /* For sections that hold a table of fixed-size entries: The size in
  bytes for each entry. */
  uint32_t sh_entsize;
};
PACK_POP

PACK_PUSH
struct elf32_Symtab_Entry {
  uint32_t st_name;
  uint32_t st_value;
  uint32_t st_size;
  uint8_t st_info;
  uint8_t st_other;
  uint16_t st_shndx;
};
PACK_POP

enum {
  kELFCLASS32 = 1,
  kELFDATA2LSB = 1,
  kEV_CURRENT = 1,
  kELFOSABI_LINUX = 3,
  kET_REL = 1,
  kEM_386 = 3,
  kSHT_NULL = 0,
  kSHT_STRTAB = 3,
  kSHT_SYMTAB = 2,
};

uint32_t strtab_append_c_str(struct databuf *d, const char *s) {
  uint32_t ret = size_to_uint32(d->count);
  databuf_append(d, s, strlen(s) + 1);
  return ret;
}

void pad_to_size(struct databuf *d, uint8_t value, size_t size) {
  CHECK(d->count <= size);
  while (d->count < size) {
    databuf_append(d, &value, 1);
  }
}

void linux32_flatten(struct objfile *f, struct databuf **out) {
  (void)f;  /* TODO */
  struct databuf *d = malloc(sizeof(*d));
  CHECK(d);
  databuf_init(d);

  const uint32_t section_header_offset = sizeof(struct elf32_Header);
  const uint32_t kNumSectionHeaders = 4;
  const uint32_t end_of_section_headers
    = uint32_add(section_header_offset,
                 kNumSectionHeaders * sizeof(struct elf32_Section_Header));
  const uint32_t sh_strtab_offset = end_of_section_headers;

  struct databuf sh_strtab;
  databuf_init(&sh_strtab);
  strtab_append_c_str(&sh_strtab, "");
  const uint32_t dot_shstrtab_index
    = strtab_append_c_str(&sh_strtab, ".shstrtab");
  const uint32_t dot_strtab_index
    = strtab_append_c_str(&sh_strtab, ".strtab");
  const uint32_t dot_symtab_index
    = strtab_append_c_str(&sh_strtab, ".symtab");
  strtab_append_c_str(&sh_strtab, "");

  const uint32_t sym_strtab_offset
    = uint32_add(sh_strtab_offset, sh_strtab.count);

  /* Add 1 for leading, 1 for trailing nul. */
  const uint32_t sym_strtab_size = uint32_add(f->strings.count, 2);
  const uint32_t sym_strtab_end = uint32_add(sym_strtab_offset, sym_strtab_size);

  /* I don't know, align it to 8. */
  const uint32_t symtab_offset = uint32_ceil_aligned(sym_strtab_end, 8);
  /* TODO: Compute symbol table size. */
  const uint32_t symtab_size = 0;


  {
    struct elf32_Header h;
    h.e_ident[0] = 0x7f;
    h.e_ident[1] = 'E';
    h.e_ident[2] = 'L';
    h.e_ident[3] = 'F';
    h.e_ident[4] = kELFCLASS32;
    h.e_ident[5] = kELFDATA2LSB;
    h.e_ident[6] = kEV_CURRENT;
    h.e_ident[7] = 0;  /* kELFOSABI_LINUX; */
    h.e_ident[8] = 0;  /* EI_ABIVERSION (= 8): must use zero. */
    /* padding */
    memset(h.e_ident + 9, 0, kEI_NIDENT - 9);

    h.e_type = kET_REL;
    h.e_machine = kEM_386;
    h.e_version = kEV_CURRENT;
    h.e_entry = 0;
    h.e_phoff = 0;
    h.e_shoff = section_header_offset;
    h.e_flags = 0;
    h.e_ehsize = sizeof(h);
    h.e_phentsize = 0;
    h.e_phnum = 0;
    h.e_shentsize = sizeof(struct elf32_Section_Header);
    h.e_shnum = kNumSectionHeaders;
    /* String table section header is at index 1. */
    h.e_shstrndx = 1;

    databuf_append(d, &h, sizeof(h));
  }

  {
    /* Null section header (for index 0). */
    struct elf32_Section_Header sh;
    sh.sh_name = 0;
    sh.sh_type = kSHT_NULL;
    sh.sh_flags = 0;
    sh.sh_addr = 0;
    sh.sh_offset = 0;
    sh.sh_size = 0;
    sh.sh_link = 0;
    sh.sh_info = 0;
    sh.sh_addralign = 0;
    sh.sh_entsize = 0;
    databuf_append(d, &sh, sizeof(sh));
  }

  {
    /* Strings table section header -- index 1. */
    struct elf32_Section_Header sh;
    sh.sh_name = dot_shstrtab_index;
    sh.sh_type = kSHT_STRTAB;
    sh.sh_flags = 0;
    sh.sh_addr = 0;
    sh.sh_offset = sh_strtab_offset;
    sh.sh_size = sh_strtab.count;
    sh.sh_link = 0;
    sh.sh_info = 0;
    sh.sh_addralign = 0;
    sh.sh_entsize = 0;
    databuf_append(d, &sh, sizeof(sh));
  }

  {
    /* Symbol table strings section header -- index 2. */
    struct elf32_Section_Header sh;
    sh.sh_name = dot_strtab_index;
    sh.sh_type = kSHT_STRTAB;
    sh.sh_flags = 0;
    sh.sh_addr = 0;
    sh.sh_offset = sym_strtab_offset;
    sh.sh_size = sym_strtab_size;
    sh.sh_link = 0;
    sh.sh_info = 0;
    sh.sh_addralign = 0;
    sh.sh_entsize = 0;
    databuf_append(d, &sh, sizeof(sh));
  }

  {
    /* Symbol table section header -- index 3. */
    struct elf32_Section_Header sh;
    sh.sh_name = dot_symtab_index;
    sh.sh_type = kSHT_SYMTAB;
    sh.sh_flags = 0;
    sh.sh_addr = 0;
    sh.sh_offset = symtab_offset;
    sh.sh_size = symtab_size;
    sh.sh_link = 2;  /* The associated string table's section header index. */
    /* TODO: one greater than the symbol table index of the last local symbol. */
    sh.sh_info = 0;
    sh.sh_addralign = 0;
    sh.sh_entsize = sizeof(struct elf32_Symtab_Entry);
    databuf_append(d, &sh, sizeof(sh));
  }

  /* Section headers done -- append string table. */
  CHECK(d->count == sh_strtab_offset);
  databuf_append(d, sh_strtab.buf, sh_strtab.count);

  databuf_destroy(&sh_strtab);

  /* Append symbol string table. */
  CHECK(d->count == sym_strtab_offset);
  databuf_append(d, "\0", 1);
  databuf_append(d, f->strings.buf, f->strings.count);
  databuf_append(d, "\0", 1);
  CHECK(d->count == sym_strtab_end);

  pad_to_size(d, 0, uint32_to_size(symtab_offset));

  *out = d;
}
