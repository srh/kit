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

PACK_PUSH
struct elf32_Rel {
  uint32_t r_offset;
  uint32_t r_info;
};
PACK_POP

/* Seems like a good idea. */
enum { kRel_Section_Alignment = 8 };

enum {
  kELFCLASS32 = 1,
  kELFDATA2LSB = 1,
  kEV_CURRENT = 1,
  kELFOSABI_LINUX = 3,
  kET_REL = 1,
  kEM_386 = 3,
  kSHT_NULL = 0,
  kSHT_PROGBITS = 1,
  kSHT_SYMTAB = 2,
  kSHT_STRTAB = 3,
  kSHT_REL = 9,
  kSTT_OBJECT = 1,
  kSTT_FUNC = 2,
  kSTB_LOCAL = 0,
  kSTB_GLOBAL = 1,
  kSHN_UNDEF = 0,
  kLinux32DataSectionNumber = 5,
  kLinux32RodataSectionNumber = 7,
  kLinux32TextSectionNumber = 9,
  kSHF_WRITE = 1,
  kSHF_ALLOC = 2,
  kSHF_EXEC = 4,
  kR_386_32 = 1,
  kR_386_PC32 = 2,
};

uint32_t strtab_append_c_str(struct databuf *d, const char *s) {
  uint32_t ret = size_to_uint32(d->count);
  databuf_append(d, s, strlen(s) + 1);
  return ret;
}

uint32_t strtab_add(struct databuf *d, const void *buf, size_t count) {
  const uint8_t *ch = buf;
  STATIC_CHECK(sizeof(uint8_t) == 1);
  for (size_t i = 0; i < count; i++) {
    CHECK(ch[i] != 0);
  }
  uint32_t ret = size_to_uint32(d->count);
  databuf_append(d, buf, count);
  databuf_append(d, "\0", 1);
  return ret;
}

void push_symbol(struct identmap *im, struct objfile_symbol_record *symbol,
                 struct databuf *symbols, struct databuf *strings) {
  /* TODO: This and the for loop below is just a copy/paste job. */
  struct elf32_Symtab_Entry ent;
  ident_value name = symbol->name;
  const void *name_buf;
  size_t name_count;
  identmap_lookup(im, name, &name_buf, &name_count);

  uint32_t offset = strtab_add(strings, name_buf, name_count);
  ent.st_name = offset;
  ent.st_value = symbol->value;
  /* TODO: It's OK to just use zero for everything? */
  ent.st_size = 0;
  ent.st_info = (symbol->is_function == IS_FUNCTION_YES ?
                 kSTT_FUNC : kSTT_OBJECT) | ((symbol->is_static ?
                                              kSTB_LOCAL : kSTB_GLOBAL) << 4);
  ent.st_other = 0;
  switch (symbol->section) {
  case OBJFILE_SYMBOL_SECTION_UNDEFINED:
    ent.st_shndx = kSHN_UNDEF;
    break;
  case OBJFILE_SYMBOL_SECTION_DATA:
    ent.st_shndx = kLinux32DataSectionNumber;
    break;
  case OBJFILE_SYMBOL_SECTION_RDATA:
    ent.st_shndx = kLinux32RodataSectionNumber;
    break;
  case OBJFILE_SYMBOL_SECTION_TEXT:
    ent.st_shndx = kLinux32TextSectionNumber;
    break;
  default:
    UNREACHABLE();
  }

  databuf_append(symbols, &ent, sizeof(ent));
}


void linux32_write_symbols_and_strings(struct identmap *im, struct objfile *f,
                                       struct databuf **symbols_out,
                                       struct databuf **strings_out,
                                       uint32_t **sti_map_out,
                                       uint32_t *end_locals_out) {
  struct databuf *symbols = malloc(sizeof(*symbols));
  CHECK(symbols);
  databuf_init(symbols);
  uint32_t symbols_count = 0;
  struct databuf *strings = malloc(sizeof(*strings));
  CHECK(strings);
  databuf_init(strings);

  strtab_add(strings, "", 0);

  /* Maps from f->symbol_table's indexes to symbols indexes, because
  they get reordered (and index zero cannot be used). */
  uint32_t *sti_map = malloc_mul(sizeof(*sti_map), f->symbol_table_count);

  {
    /* Add index zero symbol entry. */
    struct elf32_Symtab_Entry ent;
    ent.st_name = 0;
    ent.st_value = 0;
    ent.st_size = 0;
    ent.st_info = 0;
    ent.st_other = 0;
    ent.st_shndx = kSHN_UNDEF;
    databuf_append(symbols, &ent, sizeof(ent));
    symbols_count = uint32_add(symbols_count, 1);
  }

  struct objfile_symbol_record *symbol_table = f->symbol_table;
  for (size_t i = 0, e = f->symbol_table_count; i < e; i++) {
    if (symbol_table[i].is_static) {
      push_symbol(im, &symbol_table[i], symbols, strings);
      sti_map[i] = symbols_count;
      symbols_count = uint32_add(symbols_count, 1);
    }
  }

  const uint32_t end_locals = symbols_count;

  for (size_t i = 0, e = f->symbol_table_count; i < e; i++) {
    if (!symbol_table[i].is_static) {
      push_symbol(im, &symbol_table[i], symbols, strings);
      sti_map[i] = symbols_count;
      symbols_count = uint32_add(symbols_count, 1);
    }
  }

  strtab_add(strings, "", 0);

  *symbols_out = symbols;
  *strings_out = strings;
  *sti_map_out = sti_map;
  *end_locals_out = end_locals;
}

enum { kSymTabSectionNumber = 3 };

void linux32_section_headers(uint32_t prev_end_offset,
                             int sh_out_index,
                             struct objfile_section *s,
                             uint32_t sh_strtab_rel_index, uint32_t sh_strtab_index,
                             uint32_t sh_flags,
                             struct elf32_Section_Header *rel_out,
                             struct elf32_Section_Header *sh_out,
                             uint32_t *end_out) {
  uint32_t rel_offset = uint32_ceil_aligned(prev_end_offset, kRel_Section_Alignment);
  uint32_t rel_size = uint32_mul(s->relocs_count, sizeof(struct elf32_Rel));
  uint32_t rel_end = uint32_add(rel_offset, rel_size);
  uint32_t sh_alignment = s->max_requested_alignment;
  uint32_t sh_offset = uint32_ceil_aligned(rel_end, sh_alignment);
  uint32_t sh_size = s->raw.count;
  uint32_t sh_end = uint32_add(sh_offset, sh_size);

  rel_out->sh_name = sh_strtab_rel_index;
  rel_out->sh_type = kSHT_REL;
  rel_out->sh_flags = 0;
  rel_out->sh_addr = 0;
  rel_out->sh_offset = rel_offset;
  rel_out->sh_size = rel_size;
  rel_out->sh_link = kSymTabSectionNumber;
  rel_out->sh_info = sh_out_index;
  rel_out->sh_addralign = 0;
  rel_out->sh_entsize = sizeof(struct elf32_Rel);

  sh_out->sh_name = sh_strtab_index;
  sh_out->sh_type = kSHT_PROGBITS;
  sh_out->sh_flags = kSHF_ALLOC | sh_flags;
  sh_out->sh_addr = 0;
  sh_out->sh_offset = sh_offset;
  sh_out->sh_size = sh_size;
  sh_out->sh_link = kSHN_UNDEF;
  sh_out->sh_info = 0;
  sh_out->sh_addralign = sh_alignment;
  sh_out->sh_entsize = 0;

  *end_out = sh_end;
}

/* TODO: Don't mutate the section, plz.  Thanks. */
void linux32_append_relocations_and_mutate_section(
    uint32_t *sti_map, size_t sti_map_count,
    struct databuf *d, struct objfile_section *s) {
  struct objfile_relocation *relocs = s->relocs;
  for (size_t i = 0, e = s->relocs_count; i < e; i++) {
    struct elf32_Rel rel;
    rel.r_offset = relocs[i].virtual_address;
    uint32_t sti = relocs[i].symbol_table_index;
    CHECK(sti < sti_map_count);
    uint32_t new_sti = sti_map[sti];
    CHECK(new_sti <= (UINT32_MAX >> 8));
    static const uint32_t rel_types[] = {
      [OBJFILE_RELOCATION_TYPE_DIR32] = kR_386_32,
      [OBJFILE_RELOCATION_TYPE_REL32] = kR_386_PC32,
    };

    uint32_t rel_type = rel_types[relocs[i].type];
    rel.r_info = (new_sti << 8) | rel_type;
    databuf_append(d, &rel, sizeof(rel));

    static const int32_t rel_addends[] = {
      [OBJFILE_RELOCATION_TYPE_DIR32] = 0,
      [OBJFILE_RELOCATION_TYPE_REL32] = -4,
    };
    uint32_t addend = (uint32_t)rel_addends[relocs[i].type];
    databuf_overwrite(&s->raw, rel.r_offset, &addend, sizeof(addend));
  }
}

void linux32_flatten(struct identmap *im, struct objfile *f, struct databuf **out) {
  struct databuf *d = malloc(sizeof(*d));
  CHECK(d);
  databuf_init(d);

  const uint32_t section_header_offset = sizeof(struct elf32_Header);
  const uint32_t kNumSectionHeaders = 10;
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
  const uint32_t dot_rel_data_index
    = strtab_append_c_str(&sh_strtab, ".rel.data");
  const uint32_t dot_data_index
    = strtab_append_c_str(&sh_strtab, ".data");
  const uint32_t dot_rel_rodata_index
    = strtab_append_c_str(&sh_strtab, ".rel.rodata");
  const uint32_t dot_rodata_index
    = strtab_append_c_str(&sh_strtab, ".rodata");
  const uint32_t dot_rel_text_index
    = strtab_append_c_str(&sh_strtab, ".rel.text");
  const uint32_t dot_text_index
    = strtab_append_c_str(&sh_strtab, ".text");
  strtab_append_c_str(&sh_strtab, "");

  const uint32_t sym_strtab_offset
    = uint32_add(sh_strtab_offset, sh_strtab.count);

  struct databuf *symbols;
  struct databuf *strings;
  uint32_t *sti_map;
  uint32_t end_local_symbols;
  linux32_write_symbols_and_strings(im, f, &symbols, &strings, &sti_map,
                                    &end_local_symbols);

  const uint32_t sym_strtab_size = size_to_uint32(strings->count);
  const uint32_t sym_strtab_end
    = uint32_add(sym_strtab_offset, sym_strtab_size);

  /* I don't know, align it to 16 (that's its entry size). */
  const uint32_t kSymtabAlignment = 16;
  const uint32_t symtab_offset = uint32_ceil_aligned(sym_strtab_end,
                                                     kSymtabAlignment);
  const uint32_t symtab_size = size_to_uint32(symbols->count);

  const uint32_t symtab_end = uint32_add(symtab_offset, symtab_size);

  struct elf32_Section_Header sh_rel_data;
  struct elf32_Section_Header sh_data;
  uint32_t data_end;
  linux32_section_headers(symtab_end, kLinux32DataSectionNumber, &f->data,
                          dot_rel_data_index, dot_data_index,
                          kSHF_WRITE,
                          &sh_rel_data, &sh_data,
                          &data_end);

  struct elf32_Section_Header sh_rel_rodata;
  struct elf32_Section_Header sh_rodata;
  uint32_t rodata_end;
  linux32_section_headers(data_end, kLinux32RodataSectionNumber, &f->rdata,
                          dot_rel_rodata_index, dot_rodata_index,
                          0,
                          &sh_rel_rodata, &sh_rodata,
                          &rodata_end);

  struct elf32_Section_Header sh_rel_text;
  struct elf32_Section_Header sh_text;
  uint32_t text_end;
  linux32_section_headers(rodata_end, kLinux32TextSectionNumber, &f->text,
                          dot_rel_text_index, dot_text_index,
                          kSHF_EXEC,
                          &sh_rel_text, &sh_text,
                          &text_end);

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
    STATIC_CHECK(kSymTabSectionNumber == 3);
    /* Symbol table section header -- index 3. */
    struct elf32_Section_Header sh;
    sh.sh_name = dot_symtab_index;
    sh.sh_type = kSHT_SYMTAB;
    sh.sh_flags = 0;
    sh.sh_addr = 0;
    sh.sh_offset = symtab_offset;
    sh.sh_size = symtab_size;
    sh.sh_link = 2;  /* The associated string table's section header index. */
    sh.sh_info = end_local_symbols;
    sh.sh_addralign = 0;
    sh.sh_entsize = sizeof(struct elf32_Symtab_Entry);
    databuf_append(d, &sh, sizeof(sh));
  }

  STATIC_CHECK(kLinux32DataSectionNumber == 5);
  databuf_append(d, &sh_rel_data, sizeof(sh_rel_data));
  databuf_append(d, &sh_data, sizeof(sh_data));
  STATIC_CHECK(kLinux32RodataSectionNumber == 7);
  databuf_append(d, &sh_rel_rodata, sizeof(sh_rel_rodata));
  databuf_append(d, &sh_rodata, sizeof(sh_rodata));
  STATIC_CHECK(kLinux32TextSectionNumber == 9);
  databuf_append(d, &sh_rel_text, sizeof(sh_rel_text));
  databuf_append(d, &sh_text, sizeof(sh_text));

  /* Section headers done -- append string table. */
  CHECK(d->count == sh_strtab_offset);
  databuf_append(d, sh_strtab.buf, sh_strtab.count);
  databuf_destroy(&sh_strtab);

  CHECK(d->count == sym_strtab_offset);
  databuf_append(d, strings->buf, strings->count);
  databuf_destroy(strings);
  free(strings);
  CHECK(d->count == sym_strtab_end);

  append_zeros_to_align(d, kSymtabAlignment);
  CHECK(d->count == symtab_offset);
  databuf_append(d, symbols->buf, symbols->count);
  databuf_destroy(symbols);
  free(symbols);

  CHECK(d->count == symtab_end);

  append_zeros_to_align(d, kRel_Section_Alignment);
  CHECK(d->count == sh_rel_data.sh_offset);
  linux32_append_relocations_and_mutate_section(
      sti_map, f->symbol_table_count, d, &f->data);
  append_zeros_to_align(d, f->data.max_requested_alignment);
  CHECK(d->count == sh_data.sh_offset);
  databuf_append(d, f->data.raw.buf, f->data.raw.count);

  append_zeros_to_align(d, kRel_Section_Alignment);
  CHECK(d->count == sh_rel_rodata.sh_offset);
  linux32_append_relocations_and_mutate_section(
      sti_map, f->symbol_table_count, d, &f->rdata);
  append_zeros_to_align(d, f->rdata.max_requested_alignment);
  CHECK(d->count == sh_rodata.sh_offset);
  databuf_append(d, f->rdata.raw.buf, f->rdata.raw.count);

  append_zeros_to_align(d, kRel_Section_Alignment);
  CHECK(d->count == sh_rel_text.sh_offset);
  linux32_append_relocations_and_mutate_section(
      sti_map, f->symbol_table_count, d, &f->text);
  append_zeros_to_align(d, f->text.max_requested_alignment);
  CHECK(d->count == sh_text.sh_offset);
  databuf_append(d, f->text.raw.buf, f->text.raw.count);

  *out = d;
  free(sti_map);
}
