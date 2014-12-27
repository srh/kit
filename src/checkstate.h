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

typedef int module_loader(const uint8_t *module_name,
                          size_t module_name_count,
                          uint8_t **data_out,
                          size_t *data_count_out);

struct checkstate {
  module_loader *loader;
  struct identmap *im;

  struct import *imports;
  size_t imports_count;
  size_t imports_limit;

  int template_instantiation_recursion_depth;

  struct name_table nt;
};

void checkstate_init(struct checkstate *cs,
                     module_loader *loader,
                     struct identmap *im);

void checkstate_destroy(struct checkstate *cs);

#endif /* KIRA_CHECKSTATE_H_ */
