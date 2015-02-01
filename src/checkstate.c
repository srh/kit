#include "checkstate.h"

#include "slice.h"

void import_destroy(struct import *imp) {
  ast_file_destroy(imp->file);
  free(imp->file);
}

void checkstate_destroy(struct checkstate *cs) {
  databuf_destroy(&cs->error_message);
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
  databuf_init(&cs->error_message);
}

