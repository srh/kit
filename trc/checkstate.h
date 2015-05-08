#ifndef KIT_CHECKSTATE_H_
#define KIT_CHECKSTATE_H_

#include <stddef.h>

#include "databuf.h"
#include "table.h"

struct identmap;
struct import;

struct import {
  ident_value import_name;
  size_t global_offset_base;
  struct ast_file *file;
  uint8_t *buf;
  size_t buf_count;
};

struct common_idents {
  ident_value ptr;
  ident_value func;
  ident_value boole;
  ident_value voide;
  ident_value size;
  ident_value init;
  ident_value destroy;
  ident_value move;
  ident_value copy;
  ident_value do_init;
  ident_value do_destroy;
  ident_value do_move;
  ident_value do_copy;
  ident_value array_length_fieldname;
  ident_value u32_type_name;
  ident_value i32_type_name;
  ident_value u16_type_name;
  ident_value i16_type_name;
  ident_value u8_type_name;
  ident_value i8_type_name;
  ident_value size_type_name;
  ident_value osize_type_name;
  ident_value char_standin_type_name;
};

struct checkstate {
  struct identmap *im;
  struct common_idents cm;

  int target_linux32;

  struct import *imports;
  size_t imports_count;
  size_t imports_limit;

  /* TODO: Rename this, it adds 1 for every file. */
  size_t total_filesize;

  int template_instantiation_recursion_depth;

  uint32_t kit_name_counter;

  struct name_table nt;

  /* sli = string literal */
  struct identmap sli_values;

  uint32_t *sli_symbol_table_indexes;
  size_t sli_symbol_table_indexes_count;
  size_t sli_symbol_table_indexes_limit;
};

void checkstate_init(struct checkstate *cs, struct identmap *im, int target_linux32);

void checkstate_destroy(struct checkstate *cs);

ident_value checkstate_g_o_import_name(struct checkstate *cs, size_t global_offset);
size_t checkstate_g_o_line(struct checkstate *cs, size_t global_offset);
size_t checkstate_g_o_column(struct checkstate *cs, size_t global_offset);

#endif /* KIT_CHECKSTATE_H_ */
