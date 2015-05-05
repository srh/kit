#ifndef OBJFILE_STRUCTS_H_
#define OBJFILE_STRUCTS_H_

#include <stddef.h>
#include <stdint.h>

#include "databuf.h"

enum objfile_relocation_type {
  OBJFILE_RELOCATION_TYPE_DIR32,
  OBJFILE_RELOCATION_TYPE_DIR32NB,
  OBJFILE_RELOCATION_TYPE_REL32,
};

static const size_t OBJFILE_RELOCATION_TYPE_COUNT = 3;

struct objfile_relocation {
  /* Offset from beginning of section. */
  uint32_t virtual_address;
  /* Index into the symbol table. */
  uint32_t symbol_table_index;
  /* What kind of relocation should be performed? */
  enum objfile_relocation_type type;
};

struct objfile_section {
  char Name[8];
  struct databuf raw;

  /* Relavant for .data and .rdata sections.  Not relevant for .text,
  which just uses 16. */
  size_t max_requested_alignment;

  struct objfile_relocation *relocs;
  size_t relocs_count;
  size_t relocs_limit;
};

size_t objfile_section_raw_size(struct objfile_section *s);
uint16_t objfile_section_small_relocations_count(struct objfile_section *s);

#endif /* OBJFILE_STRUCTS_H_ */
