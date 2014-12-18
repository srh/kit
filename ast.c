#include "ast.h"

#include "slice.h"

struct ast_meta ast_meta_make(size_t pos_start, size_t pos_end) {
  struct ast_meta ret;
  ret.pos_start = pos_start;
  ret.pos_end = pos_end;
  return ret;
}

struct ast_meta ast_meta_make_garbage(void) {
  return ast_meta_make(0, 0);
}

struct ast_meta ast_meta_make_copy(struct ast_meta *c) {
  return *c;
}

void ast_meta_destroy(struct ast_meta *a) {
  a->pos_start = SIZE_MAX;
  a->pos_end = SIZE_MAX;
  /* Do nothing useful. */
}

void ast_ident_init(struct ast_ident *a, struct ast_meta meta,
		    ident_value value) {
  a->meta = meta;
  a->value = value;
}

void ast_ident_init_copy(struct ast_ident *a, struct ast_ident *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  a->value = c->value;
}

void ast_ident_destroy(struct ast_ident *a) {
  ast_meta_destroy(&a->meta);
  a->value = IDENT_VALUE_INVALID;
}


void ast_numeric_literal_init(struct ast_numeric_literal *a,
			      struct ast_meta meta, int8_t *digits,
			      size_t digits_count) {
  a->meta = meta;
  a->digits = digits;
  a->digits_count = digits_count;
}

void ast_numeric_literal_destroy(struct ast_numeric_literal *a) {
  ast_meta_destroy(&a->meta);
  free(a->digits);
  a->digits = NULL;
  a->digits_count = 0;
}

void ast_funcall_init(struct ast_funcall *a, struct ast_meta meta,
		      struct ast_expr *func, struct ast_expr *args,
		      size_t args_count) {
  a->meta = meta;
  a->func = func;
  a->args = args;
  a->args_count = args_count;
}

void ast_funcall_destroy(struct ast_funcall *a) {
  ast_meta_destroy(&a->meta);
  ast_expr_destroy(a->func);
  free(a->func);
  a->func = NULL;
  SLICE_FREE(a->args, a->args_count, ast_expr_destroy);
}

void ast_vardecl_init_copy(struct ast_vardecl *a, struct ast_vardecl *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_ident_init_copy(&a->name, &c->name);
  ast_typeexpr_init_copy(&a->type, &c->type);
}

void ast_vardecl_init(struct ast_vardecl *a, struct ast_meta meta,
		      struct ast_ident name, struct ast_typeexpr type) {
  a->meta = meta;
  a->name = name;
  a->type = type;
}

void ast_vardecl_destroy(struct ast_vardecl *a) {
  ast_meta_destroy(&a->meta);
  ast_ident_destroy(&a->name);
  ast_typeexpr_destroy(&a->type);
}

void ast_bracebody_init(struct ast_bracebody *a,
			struct ast_meta meta,
			struct ast_statement *statements,
			size_t statements_count) {
  a->meta = meta;
  a->statements = statements;
  a->statements_count = statements_count;
}

void ast_bracebody_destroy(struct ast_bracebody *a) {
  ast_meta_destroy(&a->meta);
  SLICE_FREE(a->statements, a->statements_count, ast_statement_destroy);
}


void ast_var_statement_init(struct ast_var_statement *a, struct ast_meta meta,
			    struct ast_ident name, struct ast_typeexpr type,
			    struct ast_expr *rhs) {
  a->meta = meta;
  a->name = name;
  a->type = type;
  a->rhs = rhs;
}

void ast_var_statement_destroy(struct ast_var_statement *a) {
  ast_meta_destroy(&a->meta);
  ast_ident_destroy(&a->name);
  ast_typeexpr_destroy(&a->type);
  ast_expr_destroy(a->rhs);
  free(a->rhs);
  a->rhs = NULL;
}

void ast_goto_statement_init(struct ast_goto_statement *a,
			     struct ast_meta meta, struct ast_ident target) {
  a->meta = meta;
  a->target = target;
}

void ast_goto_statement_destroy(struct ast_goto_statement *a) {
  ast_meta_destroy(&a->meta);
  ast_ident_destroy(&a->target);
}

void ast_label_statement_init(struct ast_label_statement *a,
			      struct ast_meta meta, struct ast_ident label) {
  a->meta = meta;
  a->label = label;
}

void ast_label_statement_destroy(struct ast_label_statement *a) {
  ast_meta_destroy(&a->meta);
  ast_ident_destroy(&a->label);
}

void ast_ifthen_statement_init(struct ast_ifthen_statement *a,
			       struct ast_meta meta,
			       struct ast_expr *condition,
			       struct ast_bracebody thenbody) {
  a->meta = meta;
  a->condition = condition;
  a->thenbody = thenbody;
}

void ast_ifthen_statement_destroy(struct ast_ifthen_statement *a) {
  ast_meta_destroy(&a->meta);
  ast_expr_destroy(a->condition);
  free(a->condition);
  a->condition = NULL;
  ast_bracebody_destroy(&a->thenbody);
}

void ast_ifthenelse_statement_init(struct ast_ifthenelse_statement *a,
				   struct ast_meta meta,
				   struct ast_expr *condition,
				   struct ast_bracebody thenbody,
				   struct ast_bracebody elsebody) {
  a->meta = meta;
  a->condition = condition;
  a->thenbody = thenbody;
  a->elsebody = elsebody;
}

void ast_ifthenelse_statement_destroy(struct ast_ifthenelse_statement *a) {
  ast_meta_destroy(&a->meta);
  ast_expr_destroy(a->condition);
  free(a->condition);
  a->condition = NULL;
  ast_bracebody_destroy(&a->thenbody);
  ast_bracebody_destroy(&a->elsebody);
}

void ast_statement_destroy(struct ast_statement *a) {
  switch (a->tag) {
  case AST_STATEMENT_EXPR:
    ast_expr_destroy(a->u.expr);
    free(a->u.expr);
    a->u.expr = NULL;
    break;
  case AST_STATEMENT_RETURN_EXPR:
    ast_expr_destroy(a->u.return_expr);
    free(a->u.return_expr);
    a->u.return_expr = NULL;
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
  a->tag = (enum ast_statement_tag)-1;
}

void ast_unop_expr_init(struct ast_unop_expr *a, struct ast_meta meta,
			enum ast_unop operator, struct ast_expr *rhs) {
  a->meta = meta;
  a->operator = operator;
  a->rhs = rhs;
}

void ast_unop_expr_destroy(struct ast_unop_expr *a) {
  ast_meta_destroy(&a->meta);
  a->operator = (enum ast_unop)-1;
  ast_expr_destroy(a->rhs);
  free(a->rhs);
  a->rhs = NULL;
}

void ast_binop_expr_init(struct ast_binop_expr *a, struct ast_meta meta,
			 enum ast_binop operator, struct ast_expr *lhs,
			 struct ast_expr *rhs) {
  a->meta = meta;
  a->operator = operator;
  a->lhs = lhs;
  a->rhs = rhs;
}

void ast_binop_expr_destroy(struct ast_binop_expr *a) {
  ast_expr_destroy(a->lhs);
  a->operator = (enum ast_binop)-1;
  free(a->lhs);
  a->lhs = NULL;
  ast_expr_destroy(a->rhs);
  free(a->rhs);
  a->rhs = NULL;
}

void ast_lambda_init(struct ast_lambda *a, struct ast_meta meta,
		     struct ast_vardecl *params, size_t params_count,
		     struct ast_typeexpr return_type,
		     struct ast_bracebody bracebody) {
  a->meta = meta;
  a->params = params;
  a->params_count = params_count;
  a->return_type = return_type;
  a->bracebody = bracebody;
}

void ast_lambda_destroy(struct ast_lambda *a) {
  ast_meta_destroy(&a->meta);
  SLICE_FREE(a->params, a->params_count, ast_vardecl_destroy);
  ast_typeexpr_destroy(&a->return_type);
  ast_bracebody_destroy(&a->bracebody);
}

void ast_local_field_access_init(struct ast_local_field_access *a,
				 struct ast_meta meta,
				 struct ast_ident fieldname) {
  a->meta = meta;
  a->fieldname = fieldname;
}

void ast_local_field_access_destroy(struct ast_local_field_access *a) {
  ast_meta_destroy(&a->meta);
  ast_ident_destroy(&a->fieldname);
}

void ast_deref_field_access_init(struct ast_deref_field_access *a,
				 struct ast_meta meta,
				 struct ast_ident fieldname) {
  a->meta = meta;
  a->fieldname = fieldname;
}

void ast_deref_field_access_destroy(struct ast_deref_field_access *a) {
  ast_meta_destroy(&a->meta);
  ast_ident_destroy(&a->fieldname);
}

struct ast_meta ast_expr_meta(struct ast_expr *a) {
  switch (a->tag) {
  case AST_EXPR_NAME: return a->u.name.meta;
  case AST_EXPR_NUMERIC_LITERAL: return a->u.numeric_literal.meta;
  case AST_EXPR_FUNCALL: return a->u.funcall.meta;
  case AST_EXPR_UNOP: return a->u.unop_expr.meta;
  case AST_EXPR_BINOP: return a->u.binop_expr.meta;
  case AST_EXPR_LAMBDA: return a->u.lambda.meta;
  case AST_EXPR_LOCAL_FIELD_ACCESS: return a->u.local_field_access.meta;
  case AST_EXPR_DEREF_FIELD_ACCESS: return a->u.deref_field_access.meta;
  default: UNREACHABLE();
  }
}

size_t ast_expr_pos_end(struct ast_expr *a) {
  return ast_expr_meta(a).pos_end;
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
  case AST_EXPR_UNOP:
    ast_unop_expr_destroy(&a->u.unop_expr);
    break;
  case AST_EXPR_BINOP:
    ast_binop_expr_destroy(&a->u.binop_expr);
    break;
  case AST_EXPR_LAMBDA:
    ast_lambda_destroy(&a->u.lambda);
    break;
  case AST_EXPR_LOCAL_FIELD_ACCESS:
    ast_local_field_access_destroy(&a->u.local_field_access);
    break;
  case AST_EXPR_DEREF_FIELD_ACCESS:
    ast_deref_field_access_destroy(&a->u.deref_field_access);
    break;
  default:
    UNREACHABLE();
  }
  a->tag = (enum ast_expr_tag)-1;
}

void ast_typeapp_init(struct ast_typeapp *a, struct ast_meta meta,
		      struct ast_ident name, struct ast_typeexpr *params,
		      size_t params_count) {
  a->meta = meta;
  a->name = name;
  a->params = params;
  a->params_count = params_count;
}

void ast_typeapp_init_copy(struct ast_typeapp *a, struct ast_typeapp *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_ident_init_copy(&a->name, &c->name);
  size_t params_count = c->params_count;
  struct ast_typeexpr *params = malloc(size_mul(params_count, sizeof(*params)));
  CHECK(params);
  for (size_t i = 0; i < params_count; i++) {
    ast_typeexpr_init_copy(&params[i], &c->params[i]);
  }
  a->params = params;
  a->params_count = params_count;
}

void ast_typeapp_destroy(struct ast_typeapp *a) {
  ast_meta_destroy(&a->meta);
  ast_ident_destroy(&a->name);
  SLICE_FREE(a->params, a->params_count, ast_typeexpr_destroy);
}

void ast_fields_alloc_copy(struct ast_vardecl *fields, size_t fields_count,
			   struct ast_vardecl **p_out, size_t *count_out) {
  struct ast_vardecl *p = malloc(size_mul(fields_count, sizeof(*p)));
  CHECK(p);
  for (size_t i = 0; i < fields_count; i++) {
    ast_vardecl_init_copy(&p[i], &fields[i]);
  }
  *p_out = p;
  *count_out = fields_count;
}

void ast_structe_init(struct ast_structe *a, struct ast_meta meta,
		      struct ast_vardecl *fields, size_t fields_count) {
  a->meta = meta;
  a->fields = fields;
  a->fields_count = fields_count;
}

void ast_structe_init_copy(struct ast_structe *a, struct ast_structe *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_fields_alloc_copy(c->fields, c->fields_count, &a->fields, &a->fields_count);
}

void ast_structe_destroy(struct ast_structe *a) {
  ast_meta_destroy(&a->meta);
  SLICE_FREE(a->fields, a->fields_count, ast_vardecl_destroy);
}

void ast_unione_init(struct ast_unione *a, struct ast_meta meta,
		     struct ast_vardecl *fields, size_t fields_count) {
  a->meta = meta;
  a->fields = fields;
  a->fields_count = fields_count;
}

void ast_unione_init_copy(struct ast_unione *a, struct ast_unione *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_fields_alloc_copy(c->fields, c->fields_count, &a->fields, &a->fields_count);
}

void ast_unione_destroy(struct ast_unione *a) {
  ast_meta_destroy(&a->meta);
  SLICE_FREE(a->fields, a->fields_count, ast_vardecl_destroy);
}

void ast_typeexpr_init_copy(struct ast_typeexpr *a,
			    struct ast_typeexpr *c) {
  a->tag = c->tag;
  switch (c->tag) {
  case AST_TYPEEXPR_NAME:
    ast_ident_init_copy(&a->u.name, &c->u.name);
    break;
  case AST_TYPEEXPR_APP:
    ast_typeapp_init_copy(&a->u.app, &c->u.app);
    break;
  case AST_TYPEEXPR_STRUCTE:
    ast_structe_init_copy(&a->u.structe, &c->u.structe);
    break;
  case AST_TYPEEXPR_UNIONE:
    ast_unione_init_copy(&a->u.unione, &c->u.unione);
    break;
  default:
    UNREACHABLE();
  }
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
  case AST_TYPEEXPR_UNKNOWN:
    break;
  default:
    UNREACHABLE();
  }
  a->tag = (enum ast_typeexpr_tag)-1;
}

void ast_generics_init_no_params(struct ast_generics *a) {
  a->has_type_params = 0;
  /* Dummy values for irrelevant fields. */
  a->meta = ast_meta_make(SIZE_MAX, SIZE_MAX);
  a->params = NULL;
  a->params_count = 0;
}
void ast_generics_init_has_params(struct ast_generics *a,
				  struct ast_meta meta,
				  struct ast_ident *params,
				  size_t params_count) {
  a->has_type_params = 1;
  a->meta = meta;
  a->params = params;
  a->params_count = params_count;
}

void ast_generics_init_copy(struct ast_generics *a,
			    struct ast_generics *c) {
  if (!c->has_type_params) {
    ast_generics_init_no_params(a);
  } else {
    a->has_type_params = 1;
    a->meta = ast_meta_make_copy(&c->meta);
    size_t params_count = c->params_count;
    struct ast_ident *params = malloc(size_mul(params_count, sizeof(*params)));
    CHECK(params);
    for (size_t i = 0; i < params_count; i++) {
      ast_ident_init_copy(&params[i], &c->params[i]);
    }
    a->params = params;
    a->params_count = params_count;
  }
}

void ast_generics_destroy(struct ast_generics *a) {
  if (a->has_type_params) {
    a->has_type_params = 0;
    ast_meta_destroy(&a->meta);
    SLICE_FREE(a->params, a->params_count, ast_ident_destroy);
  }
}

void ast_def_init(struct ast_def *a, struct ast_meta meta,
		  struct ast_generics generics,
		  struct ast_ident name, struct ast_typeexpr type,
		  struct ast_expr rhs) {
  a->meta = meta;
  a->generics = generics;
  a->name = name;
  a->type = type;
  a->rhs = rhs;
}

void ast_def_destroy(struct ast_def *a) {
  ast_meta_destroy(&a->meta);
  ast_generics_destroy(&a->generics);
  ast_ident_destroy(&a->name);
  ast_typeexpr_destroy(&a->type);
  ast_expr_destroy(&a->rhs);
}

void ast_import_init(struct ast_import *a, struct ast_meta meta,
		     struct ast_ident name) {
  a->meta = meta;
  a->name = name;
}

void ast_import_destroy(struct ast_import *a) {
  ast_meta_destroy(&a->meta);
  ast_ident_destroy(&a->name);
}

void ast_deftype_init(struct ast_deftype *a, struct ast_meta meta,
		      struct ast_generics generics,
		      struct ast_ident name, struct ast_typeexpr type) {
  a->meta = meta;
  a->generics = generics;
  a->name = name;
  a->type = type;
}

void ast_deftype_destroy(struct ast_deftype *a) {
  ast_meta_destroy(&a->meta);
  ast_generics_destroy(&a->generics);
  ast_ident_destroy(&a->name);
  ast_typeexpr_destroy(&a->type);
}

void ast_toplevel_destroy(struct ast_toplevel *a) {
  switch (a->tag) {
  case AST_TOPLEVEL_IMPORT:
    ast_import_destroy(&a->u.import);
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
  a->tag = (enum ast_toplevel_tag)-1;
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

