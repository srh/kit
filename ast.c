#include "ast.h"

#include "slice.h"

void ast_ident_destroy(struct ast_ident *a) {
  (void)a;
  /* Do nothing. */
}

void ast_def_destroy(struct ast_def *a) {
  (void)a;
  /* TODO: Implement. */
}

void ast_module_destroy(struct ast_module *a) {
  ast_ident_destroy(&a->name);
  SLICE_FREE(a->toplevels, a->toplevels_count, ast_toplevel_destroy);
}

void ast_import_destroy(struct ast_import *a) {
  ast_ident_destroy(&a->ident);
}

void ast_toplevel_destroy(struct ast_toplevel *a) {
  switch (a->tag) {
  case AST_TOPLEVEL_IMPORT:
    ast_import_destroy(&a->u.import);
    break;
  case AST_TOPLEVEL_MODULE:
    ast_module_destroy(&a->u.module);
    break;
  case AST_TOPLEVEL_DEF:
    ast_def_destroy(&a->u.def);
    break;
  }
}

void ast_file_init(struct ast_file *a,
		   struct ast_toplevel *toplevels,
		   size_t toplevels_count) {
  a->toplevels = toplevels;
  a->toplevels_count = toplevels_count;
}
