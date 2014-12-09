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

void ast_bracebody_destroy(struct ast_bracebody *a) {
  SLICE_FREE(a->statements, a->statements_count, ast_statement_destroy);
}

void ast_var_statement_destroy(struct ast_var_statement *a) {
  ast_ident_destroy(&a->name);
  ast_typeexpr_destroy(&a->type);
  ast_expr_destroy(a->rhs);
  free(a->rhs);
}

void ast_goto_statement_destroy(struct ast_goto_statement *a) {
  ast_ident_destroy(&a->target);
}

void ast_label_statement_destroy(struct ast_label_statement *a) {
  ast_ident_destroy(&a->label);
}

void ast_ifthen_statement_destroy(struct ast_ifthen_statement *a) {
  ast_expr_destroy(a->condition);
  free(a->condition);
  ast_bracebody_destroy(&a->thenbody);
}

void ast_ifthenelse_statement_destroy(struct ast_ifthenelse_statement *a) {
  ast_expr_destroy(a->condition);
  free(a->condition);
  ast_bracebody_destroy(&a->thenbody);
  ast_bracebody_destroy(&a->elsebody);
}

void ast_statement_destroy(struct ast_statement *a) {
  switch (a->tag) {
  case AST_STATEMENT_EXPR:
    ast_expr_destroy(a->u.expr);
    free(a->u.expr);
    break;
  case AST_STATEMENT_RETURN_EXPR:
    ast_expr_destroy(a->u.return_expr);
    free(a->u.return_expr);
    break;
  case AST_STATEMENT_VAR:
    ast_var_statement_destroy(&a->u.var_statement);
    break;
  case AST_STATEMENT_GOTO:
    ast_goto_statement_destroy(&a->u.goto_statement);
    break;
  case AST_STATEMENT_LABEL:
    ast_label_statement_destroy(&a->u.label_statement);
    break;
  case AST_STATEMENT_IFTHEN:
    ast_ifthen_statement_destroy(&a->u.ifthen_statement);
    break;
  case AST_STATEMENT_IFTHENELSE:
    ast_ifthenelse_statement_destroy(&a->u.ifthenelse_statement);
    break;
  default:
    UNREACHABLE();
  }
}

void ast_binop_expr_destroy(struct ast_binop_expr *a) {
  ast_expr_destroy(a->lhs);
  free(a->lhs);
  ast_expr_destroy(a->rhs);
  free(a->rhs);
}

void ast_lambda_destroy(struct ast_lambda *a) {
  SLICE_FREE(a->params, a->params_count, ast_vardecl_destroy);
  ast_typeexpr_destroy(&a->return_type);
  ast_bracebody_destroy(&a->bracebody);
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
  case AST_EXPR_BINOP:
    ast_binop_expr_destroy(&a->u.binop_expr);
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

void ast_structe_destroy(struct ast_structe *a) {
  SLICE_FREE(a->fields, a->fields_count, ast_vardecl_destroy);
}

void ast_unione_destroy(struct ast_unione *a) {
  SLICE_FREE(a->fields, a->fields_count, ast_vardecl_destroy);
}

void ast_typeexpr_destroy(struct ast_typeexpr *a) {
  switch (a->tag) {
  case AST_TYPEEXPR_NAME:
    ast_ident_destroy(&a->u.name);
    break;
  case AST_TYPEEXPR_APP:
    ast_typeapp_destroy(&a->u.app);
    break;
  case AST_TYPEEXPR_STRUCTE:
    ast_structe_destroy(&a->u.structe);
    break;
  case AST_TYPEEXPR_UNIONE:
    ast_unione_destroy(&a->u.unione);
    break;
  default:
    UNREACHABLE();
  }
}

void ast_optional_type_params_destroy(struct ast_optional_type_params *a) {
  if (a->has_type_params) {
    SLICE_FREE(a->params, a->params_count, ast_ident_destroy);
  }
}

void ast_def_destroy(struct ast_def *a) {
  ast_optional_type_params_destroy(&a->generics);
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

void ast_deftype_destroy(struct ast_deftype *a) {
  ast_optional_type_params_destroy(&a->generics);
  ast_ident_destroy(&a->name);
  ast_typeexpr_destroy(&a->type);
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
  case AST_TOPLEVEL_DEFTYPE:
    ast_deftype_destroy(&a->u.deftype);
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

