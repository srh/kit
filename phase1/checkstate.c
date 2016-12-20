#include "checkstate.h"

#include "slice.h"
#include "typecheck.h"

void typetrav_symbol_info_ptr_destroy(struct typetrav_symbol_info **info) {
  ast_typeexpr_destroy(&(*info)->type);
  free(*info);
  *info = NULL;
}

void import_destroy(struct import *imp) {
  imp->global_offset_base = 0;
  ast_file_destroy(imp->file);
  free(imp->file);
  imp->file = NULL;
  free(imp->buf);
  imp->buf = NULL;
  imp->buf_count = 0;
}

GEN_SLICE_IMPL(import, struct import, import_destroy);
GEN_SLICE_IMPL_PRIM(sti, struct sti);
GEN_SLICE_IMPL(typetrav_symbol_info_ptr, struct typetrav_symbol_info *,
               typetrav_symbol_info_ptr_destroy);

struct common_idents compute_common_idents(struct identmap *im) {
  struct common_idents ret;
  ret.ptr = identmap_intern_c_str(im, PTR_TYPE_NAME);
  ret.func = identmap_intern_c_str(im, FUNC_TYPE_NAME);
  ret.boole = identmap_intern_c_str(im, BOOL_TYPE_NAME);
  ret.voide = identmap_intern_c_str(im, VOID_TYPE_NAME);
  ret.size = identmap_intern_c_str(im, SIZE_TYPE_NAME);
  ret.init = identmap_intern_c_str(im, "init");
  ret.destroy = identmap_intern_c_str(im, "destroy");
  ret.move = identmap_intern_c_str(im, "move");
  ret.copy = identmap_intern_c_str(im, "copy");
  ret.do_init = identmap_intern_c_str(im, "do_init");
  ret.do_destroy = identmap_intern_c_str(im, "do_destroy");
  ret.do_move = identmap_intern_c_str(im, "do_move");
  ret.do_copy = identmap_intern_c_str(im, "do_copy");
  ret.array_length_fieldname = identmap_intern_c_str(im, ARRAY_LENGTH_FIELDNAME);
  ret.u8_type_name = identmap_intern_c_str(im, U8_TYPE_NAME);
  ret.i8_type_name = identmap_intern_c_str(im, I8_TYPE_NAME);
  ret.u16_type_name = identmap_intern_c_str(im, U16_TYPE_NAME);
  ret.i16_type_name = identmap_intern_c_str(im, I16_TYPE_NAME);
  ret.u32_type_name = identmap_intern_c_str(im, U32_TYPE_NAME);
  ret.i32_type_name = identmap_intern_c_str(im, I32_TYPE_NAME);
  ret.size_type_name = identmap_intern_c_str(im, SIZE_TYPE_NAME);
  ret.osize_type_name = identmap_intern_c_str(im, OSIZE_TYPE_NAME);
  ret.char_standin_type_name = identmap_intern_c_str(im, CHAR_STANDIN_TYPE_NAME);
  return ret;
}

void checkstate_destroy(struct checkstate *cs) {
  cs->typetrav_symbol_infos_first_ungenerated = 0;
  typetrav_symbol_info_ptr_slice_destroy(&cs->typetrav_symbol_infos);
  identmap_destroy(&cs->typetrav_values);

  sti_slice_destroy(&cs->sli_symbol_table_indexes);
  identmap_destroy(&cs->sli_values);

  name_table_destroy(&cs->nt);
  cs->kit_name_counter = 0;
  CHECK(cs->template_instantiation_recursion_depth == 0);
  cs->total_filesize = 0;
  import_slice_destroy(&cs->imports);
  cs->arch = (enum target_arch)-1;
  cs->platform = (enum target_platform)-1;
  cs->loader = NULL;
  cs->loader_ctx = NULL;
  cs->im = NULL;
}

void checkstate_init(struct checkstate *cs, struct identmap *im,
                     void *loader_ctx, module_loader *loader,
                     enum target_platform platform) {
  cs->im = im;
  cs->cm = compute_common_idents(im);
  cs->loader_ctx = loader_ctx;
  cs->loader = loader;
  cs->platform = platform;
  enum target_arch arch = platform_arch(platform);
  cs->arch = arch;
  cs->imports = import_slice_initializer();
  cs->total_filesize = 0;
  cs->template_instantiation_recursion_depth = 0;
  cs->kit_name_counter = 0;
  name_table_init(&cs->nt, arch);

  identmap_init(&cs->sli_values);
  cs->sli_symbol_table_indexes = sti_slice_initializer();

  identmap_init(&cs->typetrav_values);
  cs->typetrav_symbol_infos = typetrav_symbol_info_ptr_slice_initializer();
  cs->typetrav_symbol_infos_first_ungenerated = 0;
}

size_t checkstate_find_g_o_import(struct checkstate *cs, size_t global_offset) {
  CHECK(cs->imports.count > 0);
  /* b->global_offset_base <= global_offset, and e->global_offset_base
  > global_offset (if e is in range. */
  size_t b = 0;
  size_t e = cs->imports.count;

  for (;;) {
    size_t m = b + (e - b) / 2;
    if (m == b) {
      return b;
    }
    if (cs->imports.ptr[m].global_offset_base <= global_offset) {
      b = m;
    } else {
      e = m;
    }
  }
}

ident_value checkstate_g_o_import_name(struct checkstate *cs,
                                       size_t global_offset) {
  size_t ix = checkstate_find_g_o_import(cs, global_offset);
  return cs->imports.ptr[ix].import_name;
}

ident_value checkstate_g_o_import_filepath(struct checkstate *cs,
                                           size_t global_offset) {
  size_t ix = checkstate_find_g_o_import(cs, global_offset);
  return cs->imports.ptr[ix].import_filepath;
}

size_t checkstate_g_o_line(struct checkstate *cs, size_t global_offset) {
  size_t ix = checkstate_find_g_o_import(cs, global_offset);
  struct import *imp = &cs->imports.ptr[ix];
  size_t offset = size_sub(global_offset, imp->global_offset_base);

  CHECK(offset <= imp->buf_count);
  return compute_line(imp->buf, offset);
}

size_t checkstate_g_o_column(struct checkstate *cs, size_t global_offset) {
  size_t ix = checkstate_find_g_o_import(cs, global_offset);
  struct import *imp = &cs->imports.ptr[ix];
  size_t offset = size_sub(global_offset, imp->global_offset_base);

  CHECK(offset <= imp->buf_count);
  return compute_column(imp->buf, offset);
}

/* Compiler error messages (going by emacs *Compilation* behavior) are supposed to use 1-based column indexes, apparently. */
size_t checkstate_g_o_printed_column(struct checkstate *cs, size_t global_offset) {
  return size_add(1, checkstate_g_o_column(cs, global_offset));
}
