#ifndef KIRA_WIN_OBJFILE_H_
#define KIRA_WIN_OBJFILE_H_

#include <stdint.h>

struct databuf;
struct objfile;
struct objfile_section;

enum is_function {
  IS_FUNCTION_NO,
  IS_FUNCTION_YES,
};

enum is_static {
  IS_STATIC_NO,
  IS_STATIC_YES,
};

/* 1-based index into the section header table, for that matter. */
enum section {
  SECTION_DATA = 1,
  SECTION_RDATA = 2,
  SECTION_TEXT = 3,
};

void objfile_alloc(struct objfile **p_out);
void objfile_flatten(struct objfile *f, struct databuf **p_out);
void objfile_free(struct objfile **p_ref);

struct objfile_section *objfile_data(struct objfile *f);
struct objfile_section *objfile_rdata(struct objfile *f);
struct objfile_section *objfile_text(struct objfile *f);

uint32_t objfile_section_size(struct objfile_section *s);

uint32_t objfile_add_string(struct objfile *f,
                            const void *string, size_t string_size);

void objfile_section_append_32bit_reloc(struct objfile_section *s,
                                        uint32_t SymbolTableIndex,
                                        uint16_t Type);

void objfile_section_append_dir32(struct objfile_section *s,
                                  uint32_t SymbolTableIndex);
void objfile_section_append_dir32nb(struct objfile_section *s,
                                    uint32_t SymbolTableIndex);
void objfile_section_append_rel32(struct objfile_section *s,
                                  uint32_t SymbolTableIndex);
void objfile_section_append_raw(struct objfile_section *s,
                                const void *buf, size_t n);
void objfile_section_align_dword(struct objfile_section *s);
void objfile_fillercode_align_double_quadword(struct objfile *f);

void objfile_set_symbol_Value(struct objfile *f,
                              uint32_t SymbolTableIndex,
                              uint32_t Value);

uint32_t objfile_add_local_symbol(struct objfile *f,
                                  const uint8_t *name,
                                  size_t name_count,
                                  uint32_t Value,
                                  enum section section,
                                  enum is_static is_static);

uint32_t objfile_add_remote_symbol(struct objfile *f,
                                   const uint8_t *name,
                                   size_t name_count,
                                   enum is_function is_function);

int objfile_c_symbol_name(const void *name, size_t name_count,
                          void **c_name_out, size_t *c_name_count_out);

int make_almost_blank_objfile(void **buf_out, size_t *count_out);

#endif /* KIRA_WIN_OBJFILE_H_ */
