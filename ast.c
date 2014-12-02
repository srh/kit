#include "ast.h"

void ast_toplevel_destroy(struct ast_toplevel *a) {
  (void)a;
  /* TODO: Implement. */
}

void ast_file_init(struct ast_file *a,
		   struct ast_toplevel *toplevels,
		   size_t toplevels_count) {
  a->toplevels = toplevels;
  a->toplevels_count = toplevels_count;
}
