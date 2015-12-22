#ifndef OBJFILE_STRUCTS_H_
#define OBJFILE_STRUCTS_H_

#include <stddef.h>
#include <stdint.h>

#include "databuf.h"
#include "identmap.h"
#include "objfile/objfile.h"
#include "util.h"

enum objfile_relocation_type {
  OBJFILE_RELOCATION_TYPE_DIR32,
  OBJFILE_RELOCATION_TYPE_REL32,
  OBJFILE_RELOCATION_TYPE_DIFF32,
};

struct objfile_relocation {
  /* Offset from beginning of section, of the value we're going to
  change. */
  uint32_t virtual_address;
  /* Index into the symbol table. */
  struct sti symbol_table_index;
  /* Only relevant for OBJFILE_RELOCATION_TYPE_DIFF32 -- the offset
  into the relocation's section that the relocation is relative to. */
  uint32_t subtracted_offset;
  /* What kind of relocation should be performed? */
  enum objfile_relocation_type type;
};

struct objfile_section {
  struct databuf raw;

  /* Relevant for .data and .rdata sections.  Not relevant for .text,
  which just sets this to 16. */
  size_t max_requested_alignment;

  struct objfile_relocation *relocs;
  size_t relocs_count;
  size_t relocs_limit;

  /* How many diff32 relocs we have.  Diff relocs (in mach-o) use two
  entries, we use this value to compute nreloc on OS X. */
  size_t diff32_count;
};

/* This overlaps enum section in objfile.h. */
enum objfile_symbol_section {
  /* For external symbols. WINDOWS: Conveniently this value zero means
  undefined on windows. */
  OBJFILE_SYMBOL_SECTION_UNDEFINED = 0,
  /* For non-external symbols. */
  OBJFILE_SYMBOL_SECTION_DATA = 1,
  OBJFILE_SYMBOL_SECTION_RDATA = 2,
  OBJFILE_SYMBOL_SECTION_TEXT = 3,
};

struct objfile_symbol_record {
  ident_value name;
  uint32_t value;
  enum objfile_symbol_section section;
  enum is_function is_function;
  enum is_static is_static;
};

struct objfile {
  struct objfile_section data;
  struct objfile_section rdata;
  struct objfile_section text;

  struct objfile_symbol_record *symbol_table;
  size_t symbol_table_count;
  size_t symbol_table_limit;

  /* Nothing in objfile code (besides objfile_platform) should use this. */
  enum target_platform platform;
};

size_t objfile_section_raw_size(struct objfile_section *s);
uint16_t objfile_section_small_relocations_count(struct objfile_section *s);

#endif /* OBJFILE_STRUCTS_H_ */
