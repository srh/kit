#include "checkstate.h"

#include "slice.h"

void import_destroy(struct import *imp) {
  ast_file_destroy(imp->file);
  free(imp->file);
}

void checkstate_destroy(struct checkstate *cs) {
  name_table_destroy(&cs->nt);
  CHECK(cs->template_instantiation_recursion_depth == 0);
  SLICE_FREE(cs->imports, cs->imports_count, import_destroy);
  cs->imports_limit = 0;
  cs->im = NULL;
}

void checkstate_init(struct checkstate *cs,
                     module_loader *loader,
                     struct identmap *im) {
  cs->loader = loader;
  cs->im = im;
  cs->imports = NULL;
  cs->imports_count = 0;
  cs->imports_limit = 0;
  cs->template_instantiation_recursion_depth = 0;
  name_table_init(&cs->nt);
}

