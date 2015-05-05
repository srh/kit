#include "objfile/objfile.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "arith.h"
#include "databuf.h"
#include "objfile/structs.h"
#include "objfile/win.h"
#include "slice.h"
#include "util.h"

void objfile_section_init(struct objfile_section *s, const char Name[8]) {
  memcpy(s->Name, Name, 8);

  databuf_init(&s->raw);

  /* Just start it off with 4, that's good. */
  s->max_requested_alignment = 4;

  s->relocs = NULL;
  s->relocs_count = 0;
  s->relocs_limit = 0;
}

void objfile_section_destroy(struct objfile_section *s) {
  STATIC_CHECK(sizeof(s->Name) == 8);
  memset(s->Name, 0, sizeof(s->Name));
  databuf_destroy(&s->raw);
  s->max_requested_alignment = 0;
  free(s->relocs);
  s->relocs = NULL;
  s->relocs_count = 0;
  s->relocs_limit = 0;
}

void objfile_init(struct objfile *f) {
  static const char DataName[8] = ".data";
  objfile_section_init(&f->data, DataName);
  static const char ReadDataName[8] = ".rdata";
  objfile_section_init(&f->rdata, ReadDataName);
  static const char TextName[8] = ".text";
  objfile_section_init(&f->text, TextName);

  f->symbol_table = NULL;
  f->symbol_table_count = 0;
  f->symbol_table_limit = 0;

  databuf_init(&f->strings);
}

void objfile_destroy(struct objfile *f) {
  objfile_section_destroy(&f->data);
  objfile_section_destroy(&f->rdata);
  objfile_section_destroy(&f->text);

  free(f->symbol_table);
  f->symbol_table = NULL;
  f->symbol_table_count = 0;
  f->symbol_table_limit = 0;

  databuf_destroy(&f->strings);
}

void objfile_alloc(struct objfile **p_out) {
  CHECK(*p_out == NULL);
  struct objfile *p = malloc(sizeof(*p));
  CHECK(p);
  objfile_init(p);
  *p_out = p;
}

void objfile_free(struct objfile **p_ref) {
  CHECK(*p_ref);
  objfile_destroy(*p_ref);
  free(*p_ref);
  *p_ref = NULL;
}

void append_fillercode_to_align(struct databuf *d, size_t alignment) {
  CHECK(alignment > 0);
  size_t n = d->count % alignment;
  if (n != 0) {
    /* X86 */
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

void append_zeros_to_align(struct databuf *d, size_t alignment) {
  CHECK(alignment > 0);
  size_t n = d->count % alignment;
  if (n != 0) {
    static const uint8_t ch[16] = { 0 };

    size_t m = size_sub(alignment, n);

    while (m > 16) {
      databuf_append(d, ch, 16);
      m = size_sub(m, 16);
    }
    databuf_append(d, ch, m);
  }
}

/* Checks that name doesn't have any null characters (it must be
null-terminatable), and that it's non-empty (the first four bytes of a
Name field can't be zero). */
int is_valid_for_Name(const uint8_t *name, size_t name_count) {
  if (name_count == 0) {
    return 0;
  }
  for (size_t i = 0; i < name_count; i++) {
    if (name[i] == 0) {
      return 0;
    }
  }
  return 1;
}

uint32_t objfile_add_string(struct objfile *f,
                            const void *string, size_t string_count) {
  const uint8_t *ch = string;
  STATIC_CHECK(sizeof(uint8_t) == 1);
  for (size_t i = 0; i < string_count; i++) {
    CHECK(ch[i] != 0);
  }
  /* The docs say we want an offset into the strings table -- that
  offset includes the leading 4 size bytes, which means the minimum
  possible offset is 4. */
  uint32_t ret = uint32_add(size_to_uint32(f->strings.count), 4);
  databuf_append(&f->strings, string, string_count);
  databuf_append(&f->strings, "\0", 1);
  return ret;
}

void munge_to_Name(struct objfile *f,
                   const uint8_t *name,
                   size_t name_count,
                   union name_eight *Name_out) {
  CHECK(is_valid_for_Name(name, name_count));
  if (name_count <= 8) {
    STATIC_CHECK(sizeof(Name_out->ShortName) == 8);
    memset(Name_out->ShortName, 0, 8);
    memcpy(Name_out->ShortName, name, name_count);
  } else {
    uint32_t offset = objfile_add_string(f, name, name_count);
    Name_out->LongName.Zeroes = 0;
    Name_out->LongName.Offset = offset;
  }
}

uint32_t objfile_add_local_symbol(struct objfile *f,
                                  const uint8_t *name,
                                  size_t name_count,
                                  uint32_t Value,
                                  enum section section,
                                  enum is_static is_static) {
  uint32_t ret = size_to_uint32(f->symbol_table_count);
  struct objfile_symbol_record rec;
  munge_to_Name(f, name, name_count, &rec.Name);
  rec.Value = Value;
  STATIC_CHECK((int)OBJFILE_SYMBOL_SECTION_DATA == (int)SECTION_DATA);
  STATIC_CHECK((int)OBJFILE_SYMBOL_SECTION_RDATA == (int)SECTION_RDATA);
  STATIC_CHECK((int)OBJFILE_SYMBOL_SECTION_TEXT == (int)SECTION_TEXT);
  rec.section = (enum objfile_symbol_section)section;
  rec.is_function = section == SECTION_TEXT ? IS_FUNCTION_YES : IS_FUNCTION_NO;
  rec.is_static = is_static;
  SLICE_PUSH(f->symbol_table, f->symbol_table_count, f->symbol_table_limit, rec);
  return ret;
}

uint32_t objfile_add_remote_symbol(struct objfile *f,
                                   const uint8_t *name,
                                   size_t name_count,
                                   enum is_function is_function) {
  uint32_t ret = size_to_uint32(f->symbol_table_count);
  struct objfile_symbol_record rec;
  munge_to_Name(f, name, name_count, &rec.Name);
  rec.Value = 0;
  rec.section = OBJFILE_SYMBOL_SECTION_UNDEFINED;
  rec.is_function = is_function;
  rec.is_static = IS_STATIC_NO;
  SLICE_PUSH(f->symbol_table, f->symbol_table_count, f->symbol_table_limit, rec);
  return ret;
}

/* TODO: Remove -- somewher we decide whether to call win_flatten or
   whatnot. */
void objfile_flatten(struct objfile *f, struct databuf **out) {
  win_flatten(f, out);
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

void objfile_section_align_dword(struct objfile_section *s) {
  append_zeros_to_align(&s->raw, 4);
  if (s->max_requested_alignment < 4) {
    s->max_requested_alignment = 4;
  }
}

void objfile_fillercode_align_double_quadword(struct objfile *f) {
  append_fillercode_to_align(&f->text.raw, 16);
}

void objfile_set_symbol_Value(struct objfile *f,
                              uint32_t SymbolTableIndex,
                              uint32_t Value) {
  CHECK(SymbolTableIndex < f->symbol_table_count);
  /* We should only be assigning this once, I think -- and we set it
  to zero before assigning it. */
  CHECK(f->symbol_table[SymbolTableIndex].Value == 0);
  f->symbol_table[SymbolTableIndex].Value = Value;
}

void objfile_section_append_32bit_reloc(struct objfile_section *s,
                                        uint32_t symbol_table_index,
                                        enum objfile_relocation_type type) {
  struct objfile_relocation reloc;
  reloc.virtual_address = s->raw.count;
  reloc.symbol_table_index = symbol_table_index;
  reloc.type = type;
  SLICE_PUSH(s->relocs, s->relocs_count, s->relocs_limit, reloc);
  uint32_t zero = 0;
  objfile_section_append_raw(s, &zero, sizeof(zero));
}

void objfile_section_append_dir32(struct objfile_section *s,
                                  uint32_t symbol_table_index) {
  objfile_section_append_32bit_reloc(s, symbol_table_index,
                                     OBJFILE_RELOCATION_TYPE_DIR32);
}

void objfile_section_append_dir32nb(struct objfile_section *s,
                                    uint32_t symbol_table_index) {
  objfile_section_append_32bit_reloc(s, symbol_table_index,
                                     OBJFILE_RELOCATION_TYPE_DIR32NB);
}

void objfile_section_append_rel32(struct objfile_section *s,
                                  uint32_t symbol_table_index) {
  objfile_section_append_32bit_reloc(s, symbol_table_index,
                                     OBJFILE_RELOCATION_TYPE_REL32);
}

int objfile_c_symbol_name(const void *name, size_t name_count,
                          void **c_name_out, size_t *c_name_count_out) {
  char *c_name;
  alloc_memcat("_", 1, name, name_count,
               &c_name, c_name_count_out);
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
