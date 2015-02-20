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

struct checkstate {
  struct identmap *im;

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
