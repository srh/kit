#ifndef KIRA_CHECKSTATE_H_
#define KIRA_CHECKSTATE_H_

#include <stddef.h>

#include "databuf.h"
#include "table.h"

struct identmap;
struct import;

struct import {
  ident_value import_name;
  struct ast_file *file;
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

  struct import *imports;
  size_t imports_count;
  size_t imports_limit;

  int template_instantiation_recursion_depth;

  uint32_t kira_name_counter;

  struct name_table nt;

  /* sli = string literal */
  struct identmap sli_values;

  uint32_t *sli_symbol_table_indexes;
  size_t sli_symbol_table_indexes_count;
  size_t sli_symbol_table_indexes_limit;
};

void checkstate_init(struct checkstate *cs, struct identmap *im);

void checkstate_destroy(struct checkstate *cs);

#endif /* KIRA_CHECKSTATE_H_ */
