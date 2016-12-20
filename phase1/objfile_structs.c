#include "objfile_structs.h"

#include "util.h"

size_t objfile_section_raw_size(struct objfile_section *s) {
  return s->raw.count;
}

uint16_t objfile_section_small_relocations_count(struct objfile_section *s) {
  /* TODO: (Also in s2:) Support an extended relocations count. */
  CHECK(s->relocs.count <= UINT16_MAX);
  return (uint16_t)s->relocs.count;
}

GEN_SLICE_IMPL_PRIM(objfile_relocation, struct objfile_relocation);
GEN_SLICE_IMPL_PRIM(objfile_symbol_record, struct objfile_symbol_record);
