#include "objfile_objfile.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "arith.h"
#include "build.h"
#include "databuf.h"
#include "objfile_structs.h"
#include "objfile_win.h"
#include "slice.h"
#include "util.h"

void objfile_section_init(struct objfile_section *s) {
  databuf_init(&s->raw);

  /* Just start it off with 4, that's good. */
  s->max_requested_alignment = 4;

  s->relocs = NULL;
  s->relocs_count = 0;
  s->relocs_limit = 0;

  s->diff32_count = 0;
}

void objfile_section_destroy(struct objfile_section *s) {
  databuf_destroy(&s->raw);
  s->max_requested_alignment = 0;
  free(s->relocs);
  s->relocs = NULL;
  s->relocs_count = 0;
  s->relocs_limit = 0;
}

void objfile_init(struct objfile *f, enum target_platform platform) {
  objfile_section_init(&f->data);
  objfile_section_init(&f->rdata);
  objfile_section_init(&f->text);

  f->symbol_table = NULL;
  f->symbol_table_count = 0;
  f->symbol_table_limit = 0;

  f->platform = platform;
  f->arch = platform_arch(platform);
}

void objfile_destroy(struct objfile *f) {
  objfile_section_destroy(&f->data);
  objfile_section_destroy(&f->rdata);
  objfile_section_destroy(&f->text);

  free(f->symbol_table);
  f->symbol_table = NULL;
  f->symbol_table_count = 0;
  f->symbol_table_limit = 0;

  f->platform = (enum target_platform)-1;
  f->arch = (enum target_arch)-1;
}

void objfile_alloc(struct objfile **p_out, enum target_platform platform) {
  CHECK(*p_out == NULL);
  struct objfile *p = malloc(sizeof(*p));
  CHECK(p);
  objfile_init(p, platform);
  *p_out = p;
}

void objfile_free(struct objfile **p_ref) {
  CHECK(*p_ref);
  objfile_destroy(*p_ref);
  free(*p_ref);
  *p_ref = NULL;
}

enum target_platform objfile_platform(struct objfile *f) {
  return f->platform;
}

enum target_arch objfile_arch(struct objfile *f) {
  return f->arch;
}

void append_fillercode_to_align(struct databuf *d, size_t alignment) {
  CHECK(alignment > 0);
  size_t n = d->count % alignment;
  if (n != 0) {
    /* X86/X64 */
    static const uint8_t ch[16] = { 0xCC, 0xCC, 0xCC, 0xCC,
                                    0xCC, 0xCC, 0xCC, 0xCC,
                                    0xCC, 0xCC, 0xCC, 0xCC,
                                    0xCC, 0xCC, 0xCC, 0xCC };

    size_t m = size_sub(alignment, n);

    while (m > 16) {
      databuf_append(d, ch, 16);
      m = size_sub(m, 16);
    }
    databuf_append(d, ch, m);
  }
}

void append_zeros(struct databuf *d, size_t count) {
  static const uint8_t ch[16] = { 0 };
  while (count > 16) {
    databuf_append(d, ch, 16);
    count = count - 16;
  }
  databuf_append(d, ch, count);
}

void append_zeros_to_align(struct databuf *d, size_t alignment) {
  CHECK(alignment > 0);
  size_t n = d->count % alignment;
  if (n != 0) {
    append_zeros(d, size_sub(alignment, n));
  }
}

struct sti objfile_add_local_symbol(struct objfile *f,
                                    ident_value name,
                                    uint32_t value,
                                    enum section section,
                                    enum is_static is_static) {
  struct sti ret;
  ret.value = size_to_uint32(f->symbol_table_count);
  struct objfile_symbol_record rec;
  rec.name = name;
  rec.value = value;
  STATIC_CHECK((int)OBJFILE_SYMBOL_SECTION_DATA == (int)SECTION_DATA);
  STATIC_CHECK((int)OBJFILE_SYMBOL_SECTION_RDATA == (int)SECTION_RDATA);
  STATIC_CHECK((int)OBJFILE_SYMBOL_SECTION_TEXT == (int)SECTION_TEXT);
  rec.section = (enum objfile_symbol_section)section;
  rec.is_function = section == SECTION_TEXT ? IS_FUNCTION_YES : IS_FUNCTION_NO;
  rec.is_static = is_static;
  SLICE_PUSH(f->symbol_table, f->symbol_table_count, f->symbol_table_limit, rec);
  return ret;
}

struct sti objfile_add_remote_symbol(struct objfile *f,
                                     ident_value name,
                                     enum is_function is_function) {
  struct sti ret;
  ret.value = size_to_uint32(f->symbol_table_count);
  struct objfile_symbol_record rec;
  rec.name = name;
  rec.value = 0;
  rec.section = OBJFILE_SYMBOL_SECTION_UNDEFINED;
  rec.is_function = is_function;
  rec.is_static = IS_STATIC_NO;
  SLICE_PUSH(f->symbol_table, f->symbol_table_count, f->symbol_table_limit, rec);
  return ret;
}

void objfile_section_append_raw(struct objfile_section *s,
                                const void *buf, size_t n) {
  databuf_append(&s->raw, buf, n);
}

void objfile_section_overwrite_raw(struct objfile_section *s,
                                   size_t offset,
                                   const void *buf, size_t n) {
  databuf_overwrite(&s->raw, offset, buf, n);
}

void objfile_section_align(struct objfile_section *s, uint32_t align) {
  CHECK(0 == (align & (align - 1)));
  append_zeros_to_align(&s->raw, align);
  if (s->max_requested_alignment < align) {
    s->max_requested_alignment = align;
  }
}

/* x86: Just get rid of all uses of this? */
void objfile_section_align_quadword(struct objfile_section *s) {
  objfile_section_align(s, 8);
}

/* x86: Get rid of all uses of this? */
void objfile_section_align_dword(struct objfile_section *s) {
  objfile_section_align(s, 4);
}

void objfile_section_append_zeros(struct objfile_section *s, size_t count) {
  append_zeros(&s->raw, count);
}

void objfile_fillercode_align_double_quadword(struct objfile *f) {
  append_fillercode_to_align(&f->text.raw, 16);
  if (f->text.max_requested_alignment < 16) {
    f->text.max_requested_alignment = 16;
  }
}

void objfile_set_symbol_value(struct objfile *f,
                              struct sti SymbolTableIndex,
                              uint32_t value) {
  CHECK(SymbolTableIndex.value < f->symbol_table_count);
  /* We should only be assigning this once, I think -- and we set it
  to zero before assigning it. */
  CHECK(f->symbol_table[SymbolTableIndex.value].value == 0);
  f->symbol_table[SymbolTableIndex.value].value = value;
}

void objfile_section_append_32bit_reloc(struct objfile_section *s,
                                        struct sti symbol_table_index,
                                        enum objfile_relocation_type type) {
  CHECK(type != OBJFILE_RELOCATION_TYPE_DIFF32);
  struct objfile_relocation reloc;
  reloc.virtual_address = size_to_uint32(s->raw.count);
  reloc.symbol_table_index = symbol_table_index;
  reloc.subtracted_offset = 0;  /* garbage, only used for DIFF32 */
  reloc.type = type;
  SLICE_PUSH(s->relocs, s->relocs_count, s->relocs_limit, reloc);
  struct le_u32 zero = to_le_u32(0);
  objfile_section_append_raw(s, &zero, sizeof(zero));
}

void objfile_section_append_dir32(struct objfile_section *s,
                                  struct sti symbol_table_index) {
  objfile_section_append_32bit_reloc(s, symbol_table_index,
                                     OBJFILE_RELOCATION_TYPE_DIR32);
}

void objfile_section_append_rel32(struct objfile_section *s,
                                  struct sti symbol_table_index) {
  objfile_section_append_32bit_reloc(s, symbol_table_index,
                                     OBJFILE_RELOCATION_TYPE_REL32);
}

void objfile_section_note_diff32(struct objfile_section *s,
                                 struct sti symbol_table_index,
                                 size_t subtracted_offset,
                                 size_t adjusted_offset) {
  struct objfile_relocation reloc;
  reloc.virtual_address = size_to_uint32(adjusted_offset);
  reloc.symbol_table_index = symbol_table_index;
  reloc.subtracted_offset = size_to_uint32(subtracted_offset);
  reloc.type = OBJFILE_RELOCATION_TYPE_DIFF32;
  SLICE_PUSH(s->relocs, s->relocs_count, s->relocs_limit, reloc);
  s->diff32_count = uint32_add(s->diff32_count, 1);
}

int objfile_c_symbol_name(enum target_platform platform,
                          const void *name, size_t name_count,
                          void **c_name_out, size_t *c_name_count_out) {
  char *c_name;
  if (platform_prefix_underscore(platform)) {
    alloc_memcat("_", 1, name, name_count,
                 &c_name, c_name_count_out);
  } else {
    alloc_memcat("", 0, name, name_count,
                 &c_name, c_name_count_out);
  }
  *c_name_out = c_name;
  return 1;
}

struct objfile_section *objfile_data(struct objfile *f) {
  return &f->data;
}
struct objfile_section *objfile_rdata(struct objfile *f) {
  return &f->rdata;
}
struct objfile_section *objfile_text(struct objfile *f) {
  return &f->text;
}

uint32_t objfile_section_size(struct objfile_section *s) {
  return size_to_uint32(s->raw.count);
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

uint32_t strtab_append_c_str(struct databuf *d, const char *s) {
  return strtab_add(d, s, strlen(s));
}
