#include "checkstate.h"

#include "slice.h"

void import_destroy(struct import *imp) {
  ast_file_destroy(imp->file);
  free(imp->file);
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

