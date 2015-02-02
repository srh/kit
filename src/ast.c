#include "ast.h"

#include "slice.h"
#include "table.h"  /* TODO: For typelists_equal.  Reorganize locations. */

struct ast_meta ast_meta_make(struct pos pos_start, struct pos pos_end) {
  struct ast_meta ret;
  ret.pos_start = pos_start;
  ret.pos_end = pos_end;
  return ret;
}

struct ast_meta ast_meta_make_garbage(void) {
  return ast_meta_make(make_pos(0, 0, 0), make_pos(0, 0, 0));
}

struct ast_meta ast_meta_make_copy(struct ast_meta *c) {
  return *c;
}

void ast_meta_destroy(struct ast_meta *a) {
  a->pos_start = make_pos(SIZE_MAX, SIZE_MAX, SIZE_MAX);
  a->pos_end = a->pos_start;
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
                              size_t digits_count,
                              enum ast_numeric_type numeric_type) {
  a->meta = meta;
  a->digits = digits;
  a->digits_count = digits_count;
  a->numeric_type = numeric_type;
}

void ast_numeric_literal_init_copy(struct ast_numeric_literal *a,
                                   struct ast_numeric_literal *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  int8_t *digits = malloc_mul(sizeof(*digits), c->digits_count);
  for (size_t i = 0, e = c->digits_count; i < e; i++) {
    digits[i] = c->digits[i];
  }
  a->digits = digits;
  a->digits_count = c->digits_count;
  a->numeric_type = c->numeric_type;
}

void ast_numeric_literal_destroy(struct ast_numeric_literal *a) {
  ast_meta_destroy(&a->meta);
  free(a->digits);
  a->digits = NULL;
  a->digits_count = 0;
  a->numeric_type = (enum ast_numeric_type)-1;
}

void ast_funcall_init(struct ast_funcall *a, struct ast_meta meta,
                      struct ast_expr func,
                      struct ast_expr *args, size_t args_count) {
  a->meta = meta;
  ast_expr_alloc_move(func, &a->func);
  a->args = args;
  a->args_count = args_count;
}

void ast_expr_alloc_init_copy(struct ast_expr *c, struct ast_expr **out) {
  struct ast_expr *a = malloc(sizeof(*a));
  CHECK(a);
  ast_expr_init_copy(a, c);
  *out = a;
}

void ast_funcall_init_copy(struct ast_funcall *a, struct ast_funcall *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_expr_alloc_init_copy(c->func, &a->func);

  struct ast_expr *args = malloc_mul(sizeof(*args), c->args_count);
  for (size_t i = 0, e = c->args_count; i < e; i++) {
    ast_expr_init_copy(&args[i], &c->args[i]);
  }
  a->args = args;
  a->args_count = c->args_count;
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

void ast_bracebody_init_copy(struct ast_bracebody *a,
                             struct ast_bracebody *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  struct ast_statement *statements = malloc_mul(sizeof(*statements),
                                                c->statements_count);
  for (size_t i = 0, e = c->statements_count; i < e; i++) {
    ast_statement_init_copy(&statements[i], &c->statements[i]);
  }
  a->statements = statements;
  a->statements_count = c->statements_count;
}

void ast_bracebody_destroy(struct ast_bracebody *a) {
  ast_meta_destroy(&a->meta);
  SLICE_FREE(a->statements, a->statements_count, ast_statement_destroy);
}

void ast_var_statement_info_init(struct ast_var_statement_info *a) {
  a->info_valid = 0;
}

void ast_var_statement_info_init_copy(struct ast_var_statement_info *a,
                                      struct ast_var_statement_info *c) {
  a->info_valid = c->info_valid;
  if (c->info_valid) {
    ast_typeexpr_init_copy(&a->concrete_type, &c->concrete_type);
  }
}

void ast_var_statement_info_destroy(struct ast_var_statement_info *a) {
  if (a->info_valid) {
    ast_typeexpr_destroy(&a->concrete_type);
    a->info_valid = 0;
  }
}

void ast_var_statement_info_note_type(struct ast_var_statement_info *a,
                                      struct ast_typeexpr concrete_type) {
  CHECK(!a->info_valid);
  a->info_valid = 1;
  a->concrete_type = concrete_type;
}


void ast_var_statement_init_with_rhs(struct ast_var_statement *a, struct ast_meta meta,
                                     struct ast_vardecl decl, struct ast_expr rhs) {
  a->meta = meta;
  ast_var_statement_info_init(&a->info);
  a->decl = decl;
  a->has_rhs = 1;
  ast_expr_alloc_move(rhs, &a->rhs_);
}

void ast_var_statement_init_without_rhs(struct ast_var_statement *a, struct ast_meta meta,
                                        struct ast_vardecl decl) {
  a->meta = meta;
  ast_var_statement_info_init(&a->info);
  a->decl = decl;
  a->has_rhs = 0;
  a->rhs_ = NULL;
}

struct ast_typeexpr *ast_var_statement_type(struct ast_var_statement *a) {
  CHECK(a->info.info_valid);
  return &a->info.concrete_type;
}

void ast_var_statement_init_copy(struct ast_var_statement *a,
                                 struct ast_var_statement *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_var_statement_info_init_copy(&a->info, &c->info);
  ast_vardecl_init_copy(&a->decl, &c->decl);
  a->has_rhs = c->has_rhs;
  if (c->has_rhs) {
    ast_expr_alloc_init_copy(c->rhs_, &a->rhs_);
  } else {
    c->rhs_ = NULL;
  }
}

void ast_var_statement_destroy(struct ast_var_statement *a) {
  ast_meta_destroy(&a->meta);
  ast_var_statement_info_destroy(&a->info);
  ast_vardecl_destroy(&a->decl);
  if (a->has_rhs) {
    a->has_rhs = 0;
    ast_expr_destroy(a->rhs_);
    free(a->rhs_);
    a->rhs_ = NULL;
  }
}

void ast_goto_statement_init(struct ast_goto_statement *a,
                             struct ast_meta meta, struct ast_ident target) {
  a->meta = meta;
  a->target = target;
}

void ast_goto_statement_init_copy(struct ast_goto_statement *a,
                                  struct ast_goto_statement *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_ident_init_copy(&a->target, &c->target);
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

void ast_label_statement_init_copy(struct ast_label_statement *a,
                                   struct ast_label_statement *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_ident_init_copy(&a->label, &c->label);
}

void ast_label_statement_destroy(struct ast_label_statement *a) {
  ast_meta_destroy(&a->meta);
  ast_ident_destroy(&a->label);
}

void ast_ifthen_statement_init(struct ast_ifthen_statement *a,
                               struct ast_meta meta,
                               struct ast_expr condition,
                               struct ast_bracebody thenbody) {
  a->meta = meta;
  ast_expr_alloc_move(condition, &a->condition);
  a->thenbody = thenbody;
}

void ast_ifthen_statement_init_copy(struct ast_ifthen_statement *a,
                                    struct ast_ifthen_statement *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_expr_alloc_init_copy(c->condition, &a->condition);
  ast_bracebody_init_copy(&a->thenbody, &c->thenbody);
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
                                   struct ast_expr condition,
                                   struct ast_bracebody thenbody,
                                   struct ast_bracebody elsebody) {
  a->meta = meta;
  ast_expr_alloc_move(condition, &a->condition);
  a->thenbody = thenbody;
  a->elsebody = elsebody;
}

void ast_ifthenelse_statement_init_copy(struct ast_ifthenelse_statement *a,
                                        struct ast_ifthenelse_statement *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_expr_alloc_init_copy(c->condition, &a->condition);
  ast_bracebody_init_copy(&a->thenbody, &c->thenbody);
  ast_bracebody_init_copy(&a->elsebody, &c->elsebody);
}

void ast_ifthenelse_statement_destroy(struct ast_ifthenelse_statement *a) {
  ast_meta_destroy(&a->meta);
  ast_expr_destroy(a->condition);
  free(a->condition);
  a->condition = NULL;
  ast_bracebody_destroy(&a->thenbody);
  ast_bracebody_destroy(&a->elsebody);
}

void ast_statement_init_copy(struct ast_statement *a,
                             struct ast_statement *c) {
  a->tag = c->tag;
  switch (c->tag) {
  case AST_STATEMENT_EXPR:
    ast_expr_alloc_init_copy(c->u.expr, &a->u.expr);
    break;
  case AST_STATEMENT_RETURN_EXPR:
    ast_expr_alloc_init_copy(c->u.return_expr, &a->u.return_expr);
    break;
  case AST_STATEMENT_VAR:
    ast_var_statement_init_copy(&a->u.var_statement, &c->u.var_statement);
    break;
  case AST_STATEMENT_GOTO:
    ast_goto_statement_init_copy(&a->u.goto_statement, &c->u.goto_statement);
    break;
  case AST_STATEMENT_LABEL:
    ast_label_statement_init_copy(&a->u.label_statement,
                                  &c->u.label_statement);
    break;
  case AST_STATEMENT_IFTHEN:
    ast_ifthen_statement_init_copy(&a->u.ifthen_statement,
                                   &c->u.ifthen_statement);
    break;
  case AST_STATEMENT_IFTHENELSE:
    ast_ifthenelse_statement_init_copy(&a->u.ifthenelse_statement,
                                       &c->u.ifthenelse_statement);
    break;
  default:
    UNREACHABLE();
  }
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

int is_magic_unop(enum ast_unop unop) {
  return unop == AST_UNOP_DEREFERENCE || unop == AST_UNOP_ADDRESSOF;
}

void ast_unop_expr_init(struct ast_unop_expr *a, struct ast_meta meta,
                        enum ast_unop operator, struct ast_expr rhs) {
  a->meta = meta;
  a->operator = operator;
  ast_expr_alloc_move(rhs, &a->rhs);
}

void ast_unop_expr_init_copy(struct ast_unop_expr *a,
                             struct ast_unop_expr *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  a->operator = c->operator;
  ast_expr_alloc_init_copy(c->rhs, &a->rhs);
}

void ast_unop_expr_destroy(struct ast_unop_expr *a) {
  ast_meta_destroy(&a->meta);
  a->operator = (enum ast_unop)-1;
  ast_expr_destroy(a->rhs);
  free(a->rhs);
  a->rhs = NULL;
}

int is_magic_binop(enum ast_binop binop) {
  return binop == AST_BINOP_ASSIGN || binop == AST_BINOP_LOGICAL_OR
    || binop == AST_BINOP_LOGICAL_AND;
}

void ast_binop_expr_init(struct ast_binop_expr *a, struct ast_meta meta,
                         enum ast_binop operator, struct ast_expr lhs,
                         struct ast_expr rhs) {
  a->meta = meta;
  a->operator = operator;
  ast_expr_alloc_move(lhs, &a->lhs);
  ast_expr_alloc_move(rhs, &a->rhs);
}

void ast_binop_expr_init_copy(struct ast_binop_expr *a,
                              struct ast_binop_expr *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  a->operator = c->operator;
  ast_expr_alloc_init_copy(c->lhs, &a->lhs);
  ast_expr_alloc_init_copy(c->rhs, &a->rhs);
}

void ast_binop_expr_destroy(struct ast_binop_expr *a) {
  ast_meta_destroy(&a->meta);
  a->operator = (enum ast_binop)-1;
  ast_expr_destroy(a->lhs);
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

void ast_lambda_init_copy(struct ast_lambda *a, struct ast_lambda *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  struct ast_vardecl *params = malloc_mul(sizeof(*params), c->params_count);
  for (size_t i = 0, e = c->params_count; i < e; i++) {
    ast_vardecl_init_copy(&params[i], &c->params[i]);
  }
  a->params = params;
  a->params_count = c->params_count;
  ast_typeexpr_init_copy(&a->return_type, &c->return_type);
  ast_bracebody_init_copy(&a->bracebody, &c->bracebody);
}

void ast_lambda_destroy(struct ast_lambda *a) {
  ast_meta_destroy(&a->meta);
  SLICE_FREE(a->params, a->params_count, ast_vardecl_destroy);
  ast_typeexpr_destroy(&a->return_type);
  ast_bracebody_destroy(&a->bracebody);
}

void ast_local_field_access_init(struct ast_local_field_access *a,
                                 struct ast_meta meta,
                                 struct ast_expr lhs,
                                 struct ast_ident fieldname) {
  a->meta = meta;
  ast_expr_alloc_move(lhs, &a->lhs);
  a->fieldname = fieldname;
}

void ast_local_field_access_init_copy(struct ast_local_field_access *a,
                                      struct ast_local_field_access *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_expr_alloc_init_copy(c->lhs, &a->lhs);
  ast_ident_init_copy(&a->fieldname, &c->fieldname);
}

void ast_local_field_access_destroy(struct ast_local_field_access *a) {
  ast_meta_destroy(&a->meta);
  ast_expr_destroy(a->lhs);
  free(a->lhs);
  ast_ident_destroy(&a->fieldname);
}

void ast_deref_field_access_init(struct ast_deref_field_access *a,
                                 struct ast_meta meta,
                                 struct ast_expr lhs,
                                 struct ast_ident fieldname) {
  a->meta = meta;
  ast_expr_alloc_move(lhs, &a->lhs);
  a->fieldname = fieldname;
}

void ast_deref_field_access_init_copy(struct ast_deref_field_access *a,
                                      struct ast_deref_field_access *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_expr_alloc_init_copy(c->lhs, &a->lhs);
  ast_ident_init_copy(&a->fieldname, &c->fieldname);
}

void ast_deref_field_access_destroy(struct ast_deref_field_access *a) {
  ast_meta_destroy(&a->meta);
  ast_expr_destroy(a->lhs);
  free(a->lhs);
  ast_ident_destroy(&a->fieldname);
}

void ast_name_expr_info_init(struct ast_name_expr_info *a) {
  a->info_valid = 0;
}

void ast_name_expr_info_init_copy(struct ast_name_expr_info *a,
                                  struct ast_name_expr_info *c) {
  *a = *c;
}

void ast_name_expr_info_destroy(struct ast_name_expr_info *a) {
  a->info_valid = 0;
}

void ast_name_expr_info_mark_inst(struct ast_name_expr_info *a,
                                  struct def_instantiation *inst_or_null) {
  CHECK(!a->info_valid);
  a->info_valid = 1;
  a->inst_or_null = inst_or_null;
}

int ast_name_expr_info_get_inst(struct ast_name_expr_info *a,
                                struct def_instantiation **inst_or_null_out) {
  if (a->info_valid) {
    *inst_or_null_out = a->inst_or_null;
    return 1;
  } else {
    return 0;
  }
}

void ast_name_expr_init(struct ast_name_expr *a, struct ast_ident ident) {
  ast_name_expr_info_init(&a->info);
  a->ident = ident;
}

void ast_name_expr_init_copy(struct ast_name_expr *a,
                             struct ast_name_expr *c) {
  ast_name_expr_info_init_copy(&a->info, &c->info);
  ast_ident_init_copy(&a->ident, &c->ident);
}

void ast_name_expr_destroy(struct ast_name_expr *a) {
  ast_name_expr_info_destroy(&a->info);
  ast_ident_destroy(&a->ident);
}

void ast_index_expr_init(struct ast_index_expr *a,
                         struct ast_meta meta,
                         struct ast_expr lhs,
                         struct ast_expr rhs) {
  a->meta = meta;
  ast_expr_alloc_move(lhs, &a->lhs);
  ast_expr_alloc_move(rhs, &a->rhs);
}

void ast_index_expr_init_copy(struct ast_index_expr *a,
                              struct ast_index_expr *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_expr_alloc_init_copy(c->lhs, &a->lhs);
  ast_expr_alloc_init_copy(c->rhs, &a->rhs);
}

void ast_index_expr_destroy(struct ast_index_expr *a) {
  ast_meta_destroy(&a->meta);
  ast_expr_destroy(a->lhs);
  free(a->lhs);
  a->lhs = NULL;
  ast_expr_destroy(a->rhs);
  free(a->rhs);
  a->rhs = NULL;
}

void ast_typed_expr_init(struct ast_typed_expr *a,
                         struct ast_meta meta,
                         struct ast_expr lhs,
                         struct ast_typeexpr type) {
  a->meta = meta;
  ast_expr_alloc_move(lhs, &a->lhs);
  a->type = type;
}

void ast_typed_expr_init_copy(struct ast_typed_expr *a,
                              struct ast_typed_expr *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_expr_alloc_init_copy(c->lhs, &a->lhs);
  ast_typeexpr_init_copy(&a->type, &c->type);
}

void ast_typed_expr_destroy(struct ast_typed_expr *a) {
  ast_meta_destroy(&a->meta);
  ast_expr_destroy(a->lhs);
  free(a->lhs);
  a->lhs = NULL;
  ast_typeexpr_destroy(&a->type);
}


struct ast_expr_info ast_expr_info_default(void) {
  struct ast_expr_info ret;
  ret.is_typechecked = 0;
  return ret;
}

struct ast_expr_info ast_expr_info_typechecked(
    struct ast_typeexpr concrete_type) {
  struct ast_expr_info ret;
  ret.is_typechecked = 1;
  ret.concrete_type = concrete_type;
  return ret;
}

void ast_expr_info_init_copy(struct ast_expr_info *a, struct ast_expr_info *c) {
  a->is_typechecked = c->is_typechecked;
  if (c->is_typechecked) {
    ast_typeexpr_init_copy(&a->concrete_type, &c->concrete_type);
  }
}

void ast_expr_info_destroy(struct ast_expr_info *m) {
  if (m->is_typechecked) {
    ast_typeexpr_destroy(&m->concrete_type);
    m->is_typechecked = 0;
  }
}

struct ast_meta *ast_expr_ast_meta(struct ast_expr *a) {
  switch (a->tag) {
  case AST_EXPR_NAME: return &a->u.name.ident.meta;
  case AST_EXPR_NUMERIC_LITERAL: return &a->u.numeric_literal.meta;
  case AST_EXPR_FUNCALL: return &a->u.funcall.meta;
  case AST_EXPR_INDEX: return &a->u.index_expr.meta;
  case AST_EXPR_UNOP: return &a->u.unop_expr.meta;
  case AST_EXPR_BINOP: return &a->u.binop_expr.meta;
  case AST_EXPR_LAMBDA: return &a->u.lambda.meta;
  case AST_EXPR_LOCAL_FIELD_ACCESS: return &a->u.local_field_access.meta;
  case AST_EXPR_DEREF_FIELD_ACCESS: return &a->u.deref_field_access.meta;
  case AST_EXPR_TYPED: return &a->u.typed_expr.meta;
  default: UNREACHABLE();
  }
}

struct pos ast_expr_pos_end(struct ast_expr *a) {
  return ast_expr_ast_meta(a)->pos_end;
}

void ast_expr_partial_init(struct ast_expr *a,
                           enum ast_expr_tag tag,
                           struct ast_expr_info info) {
  a->tag = tag;
  a->info = info;
}

void ast_expr_init_copy(struct ast_expr *a, struct ast_expr *c) {
  a->tag = c->tag;
  ast_expr_info_init_copy(&a->info, &c->info);
  switch (c->tag) {
  case AST_EXPR_NAME:
    ast_name_expr_init_copy(&a->u.name, &c->u.name);
    break;
  case AST_EXPR_NUMERIC_LITERAL:
    ast_numeric_literal_init_copy(&a->u.numeric_literal,
                                  &c->u.numeric_literal);
    break;
  case AST_EXPR_FUNCALL:
    ast_funcall_init_copy(&a->u.funcall, &c->u.funcall);
    break;
  case AST_EXPR_INDEX:
    ast_index_expr_init_copy(&a->u.index_expr, &c->u.index_expr);
    break;
  case AST_EXPR_UNOP:
    ast_unop_expr_init_copy(&a->u.unop_expr, &c->u.unop_expr);
    break;
  case AST_EXPR_BINOP:
    ast_binop_expr_init_copy(&a->u.binop_expr, &c->u.binop_expr);
    break;
  case AST_EXPR_LAMBDA:
    ast_lambda_init_copy(&a->u.lambda, &c->u.lambda);
    break;
  case AST_EXPR_LOCAL_FIELD_ACCESS:
    ast_local_field_access_init_copy(&a->u.local_field_access,
                                     &c->u.local_field_access);
    break;
  case AST_EXPR_DEREF_FIELD_ACCESS:
    ast_deref_field_access_init_copy(&a->u.deref_field_access,
                                     &c->u.deref_field_access);
    break;
  case AST_EXPR_TYPED:
    ast_typed_expr_init_copy(&a->u.typed_expr, &c->u.typed_expr);
    break;
  default:
    UNREACHABLE();
  }
}

void ast_expr_destroy(struct ast_expr *a) {
  switch (a->tag) {
  case AST_EXPR_NAME:
    ast_name_expr_destroy(&a->u.name);
    break;
  case AST_EXPR_NUMERIC_LITERAL:
    ast_numeric_literal_destroy(&a->u.numeric_literal);
    break;
  case AST_EXPR_FUNCALL:
    ast_funcall_destroy(&a->u.funcall);
    break;
  case AST_EXPR_INDEX:
    ast_index_expr_destroy(&a->u.index_expr);
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
  case AST_EXPR_TYPED:
    ast_typed_expr_destroy(&a->u.typed_expr);
    break;
  default:
    UNREACHABLE();
  }
  ast_expr_info_destroy(&a->info);
  a->tag = (enum ast_expr_tag)-1;
}

void malloc_move_ast_expr(struct ast_expr movee, struct ast_expr **out) {
  struct ast_expr *p = malloc(sizeof(*p));
  CHECK(p);
  *p = movee;
  *out = p;
}

struct ast_typeexpr *ast_expr_type(struct ast_expr *a) {
  CHECK(a->info.is_typechecked);
  return &a->info.concrete_type;
}

void ast_expr_alloc_move(struct ast_expr movee, struct ast_expr **out) {
  struct ast_expr *p = malloc(sizeof(*p));
  CHECK(p);
  *p = movee;
  *out = p;
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
  struct ast_typeexpr *params = malloc_mul(params_count, sizeof(*params));
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
  struct ast_vardecl *p = malloc_mul(fields_count, sizeof(*p));
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
  ast_fields_alloc_copy(c->fields, c->fields_count,
                        &a->fields, &a->fields_count);
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
  ast_fields_alloc_copy(c->fields, c->fields_count,
                        &a->fields, &a->fields_count);
}

void ast_unione_destroy(struct ast_unione *a) {
  ast_meta_destroy(&a->meta);
  SLICE_FREE(a->fields, a->fields_count, ast_vardecl_destroy);
}

void ast_arraytype_init(struct ast_arraytype *a, struct ast_meta meta,
                        uint32_t count, struct ast_typeexpr param) {
  a->meta = meta;
  a->count = count;
  a->param = malloc(sizeof(*a->param));
  CHECK(a->param);
  *a->param = param;
}

void ast_arraytype_init_copy(struct ast_arraytype *a, struct ast_arraytype *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  a->count = c->count;
  a->param = malloc(sizeof(*a->param));
  CHECK(a->param);
  ast_typeexpr_init_copy(a->param, c->param);
}

void ast_arraytype_destroy(struct ast_arraytype *a) {
  ast_meta_destroy(&a->meta);
  a->count = 0;
  ast_typeexpr_destroy(a->param);
  free(a->param);
  a->param = NULL;
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
  case AST_TYPEEXPR_ARRAY:
    ast_arraytype_init_copy(&a->u.arraytype, &c->u.arraytype);
    break;
  case AST_TYPEEXPR_UNKNOWN:
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
  case AST_TYPEEXPR_ARRAY:
    ast_arraytype_destroy(&a->u.arraytype);
    break;
  case AST_TYPEEXPR_UNKNOWN:
    break;
  default:
    UNREACHABLE();
  }
  a->tag = (enum ast_typeexpr_tag)-1;
}

struct ast_meta *ast_typeexpr_meta(struct ast_typeexpr *a) {
  switch (a->tag) {
  case AST_TYPEEXPR_NAME: return &a->u.name.meta;
  case AST_TYPEEXPR_APP: return &a->u.app.meta;
  case AST_TYPEEXPR_STRUCTE: return &a->u.structe.meta;
  case AST_TYPEEXPR_UNIONE: return &a->u.unione.meta;
  case AST_TYPEEXPR_ARRAY: return &a->u.arraytype.meta;
  case AST_TYPEEXPR_UNKNOWN:
    CRASH("No meta data for \"unknown\" typeexpr.\n");
  default:
    UNREACHABLE();
  }
}

void ast_generics_init_no_params(struct ast_generics *a) {
  a->has_type_params = 0;
  /* Dummy values for irrelevant fields. */
  a->meta = ast_meta_make_garbage();
  ast_meta_destroy(&a->meta);
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
    struct ast_ident *params = malloc_mul(params_count, sizeof(*params));
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

void ast_extern_def_init(struct ast_extern_def *a, struct ast_meta meta,
                         struct ast_ident name, struct ast_typeexpr type) {
  a->meta = meta;
  a->name = name;
  a->type = type;
}

void ast_extern_def_destroy(struct ast_extern_def *a) {
  ast_meta_destroy(&a->meta);
  ast_ident_destroy(&a->name);
  ast_typeexpr_destroy(&a->type);
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
  case AST_TOPLEVEL_EXTERN_DEF:
    ast_extern_def_destroy(&a->u.extern_def);
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

