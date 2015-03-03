#include "checkstate.h"

#include "slice.h"
#include "typecheck.h"

void import_destroy(struct import *imp) {
  ast_file_destroy(imp->file);
  free(imp->file);
}

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
  free(cs->sli_symbol_table_indexes);
  cs->sli_symbol_table_indexes = NULL;
  cs->sli_symbol_table_indexes_count = 0;
  cs->sli_symbol_table_indexes_limit = 0;
  identmap_destroy(&cs->sli_values);
  name_table_destroy(&cs->nt);
  cs->kira_name_counter = 0;
  CHECK(cs->template_instantiation_recursion_depth == 0);
  SLICE_FREE(cs->imports, cs->imports_count, import_destroy);
  cs->imports_limit = 0;
  cs->im = NULL;
}

void checkstate_init(struct checkstate *cs, struct identmap *im) {
  cs->im = im;
  cs->cm = compute_common_idents(im);
  cs->imports = NULL;
  cs->imports_count = 0;
  cs->imports_limit = 0;
  cs->template_instantiation_recursion_depth = 0;
  cs->kira_name_counter = 0;
  name_table_init(&cs->nt);

  identmap_init(&cs->sli_values);
  cs->sli_symbol_table_indexes = 0;
  cs->sli_symbol_table_indexes_count = 0;
  cs->sli_symbol_table_indexes_limit = 0;
}

