#include "objfile/structs.h"

#include "util.h"

size_t objfile_section_raw_size(struct objfile_section *s) {
  return s->raw.count;
}

uint16_t objfile_section_small_relocations_count(struct objfile_section *s) {
  /* TODO: Support an extended relocations count. */
  CHECK(s->relocs_count <= UINT16_MAX);
  return (uint16_t)s->relocs_count;
}

