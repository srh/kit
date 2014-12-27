#ifndef KIRA_CHECKSTATE_H_
#define KIRA_CHECKSTATE_H_

#include <stddef.h>

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

  struct name_table nt;
};

void checkstate_init(struct checkstate *cs, struct identmap *im);

void checkstate_destroy(struct checkstate *cs);

#endif /* KIRA_CHECKSTATE_H_ */
