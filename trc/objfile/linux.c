#include "objfile/linux.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "databuf.h"
#include "util.h"

enum { ELF_NIDENT = 16 };

PACK_PUSH
struct elf32_Header {
  uint8_t e_ident[ELF_NIDENT];
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

enum {
  kELFCLASS32 = 1,
  kELFDATA2LSB = 1,
  kEV_CURRENT = 1,
  kELFOSABI_LINUX = 3,
  kET_REL = 1,
  kEM_386 = 3,
};

void linux32_flatten(struct objfile *f, struct databuf **out) {
  (void)f;  /* TODO */
  struct databuf *d = malloc(sizeof(*d));
  CHECK(d);
  databuf_init(d);

  /* TODO: uint32_t section_header_offset = sizeof(struct elf32_Header); */

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
    memset(h.e_ident + 9, 0, ELF_NIDENT - 9);

    h.e_type = kET_REL;
    h.e_machine = kEM_386;
    h.e_version = kEV_CURRENT;
    h.e_entry = 0;
    h.e_phoff = 0;
    h.e_shoff = 0; /* TODO: section_header_offset; */
    h.e_flags = 0;
    h.e_ehsize = sizeof(h);
    h.e_phentsize = 0;
    h.e_phnum = 0;
    /* TODO: This should be the size of a section header. */
    h.e_shentsize = 0;
    /* TODO: The number of entries in the secton header table. */
    h.e_shnum = 0;
    /* TODO: Section header table index of the entry associated with
    the section name string table.  (What's a section name string
    table?  Just for section names?) */
    h.e_shstrndx = 0;

    databuf_append(d, &h, sizeof(h));
  }

  /* Section header goes here. */

  *out = d;
}
