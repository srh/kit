#include "ast.h"

#include "slice.h"

void ast_ident_destroy(struct ast_ident *a) {
  (void)a;
  /* Do nothing. */
}

void ast_numeric_literal_destroy(struct ast_numeric_literal *a) {
  free(a->digits);
}

void ast_funcall_destroy(struct ast_funcall *a) {
  ast_expr_destroy(a->func);
  free(a->func);
  SLICE_FREE(a->args, a->args_count, ast_expr_destroy);
}

void ast_vardecl_destroy(struct ast_vardecl *a) {
  ast_ident_destroy(&a->name);
  ast_typeexpr_destroy(&a->type);
}

void ast_lambda_destroy(struct ast_lambda *a) {
  SLICE_FREE(a->params, a->params_count, ast_vardecl_destroy);
  ast_typeexpr_destroy(&a->return_type);
  ast_expr_destroy(a->body);
  free(a->body);
}

void ast_expr_destroy(struct ast_expr *a) {
  switch (a->tag) {
  case AST_EXPR_NAME:
    ast_ident_destroy(&a->u.name);
    break;
  case AST_EXPR_NUMERIC_LITERAL:
    ast_numeric_literal_destroy(&a->u.numeric_literal);
    break;
  case AST_EXPR_FUNCALL:
    ast_funcall_destroy(&a->u.funcall);
    break;
  case AST_EXPR_LAMBDA:
    ast_lambda_destroy(&a->u.lambda);
    break;
  default:
    UNREACHABLE();
  }
}

void ast_typeapp_destroy(struct ast_typeapp *a) {
  ast_ident_destroy(&a->name);
  SLICE_FREE(a->params, a->params_count, ast_typeexpr_destroy);
}

void ast_typeexpr_destroy(struct ast_typeexpr *a) {
  switch (a->tag) {
  case AST_TYPEEXPR_NAME:
    ast_ident_destroy(&a->u.name);
    break;
  case AST_TYPEEXPR_APP:
    ast_typeapp_destroy(&a->u.app);
    break;
  default:
    UNREACHABLE();
  }
}

void ast_def_destroy(struct ast_def *a) {
  ast_ident_destroy(&a->name);
  ast_typeexpr_destroy(&a->type);
  ast_expr_destroy(&a->rhs);
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
  default:
    UNREACHABLE();
  }
}

void ast_file_destroy(struct ast_file *a) {
  SLICE_FREE(a->toplevels, a->toplevels_count, ast_toplevel_destroy);
}

void ast_file_init(struct ast_file *a,
		   struct ast_toplevel *toplevels,
		   size_t toplevels_count) {
  a->toplevels = toplevels;
  a->toplevels_count = toplevels_count;
}

