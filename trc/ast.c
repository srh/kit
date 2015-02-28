#include "ast.h"

#include "slice.h"

struct ast_meta ast_meta_make(struct pos pos_start, struct pos pos_end) {
  struct ast_meta ret;
  ret.pos_start = pos_start;
  ret.pos_end = pos_end;
  return ret;
}

struct ast_meta ast_meta_make_garbage(void) {
  return ast_meta_make(make_pos(0, 0, 0, IDENT_VALUE_INVALID),
                       make_pos(0, 0, 0, IDENT_VALUE_INVALID));
}

struct ast_meta ast_meta_make_copy(struct ast_meta *c) {
  return *c;
}

void ast_meta_destroy(struct ast_meta *a) {
  a->pos_start = make_pos(SIZE_MAX, SIZE_MAX, SIZE_MAX, IDENT_VALUE_INVALID);
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
                              struct ast_meta meta,
                              enum ast_numeric_literal_tag tag,
                              int8_t *digits,
                              size_t digits_count) {
  a->meta = meta;
  a->tag = tag;
  a->digits = digits;
  a->digits_count = digits_count;
}

void ast_numeric_literal_init_copy(struct ast_numeric_literal *a,
                                   struct ast_numeric_literal *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  a->tag = c->tag;
  SLICE_INIT_COPY_PRIM(a->digits, a->digits_count, c->digits, c->digits_count);
}

void ast_numeric_literal_destroy(struct ast_numeric_literal *a) {
  ast_meta_destroy(&a->meta);
  a->tag = (enum ast_numeric_literal_tag)-1;
  free(a->digits);
  a->digits = NULL;
  a->digits_count = 0;
}

void ast_bool_literal_init(struct ast_bool_literal *a,
                           struct ast_meta meta, int value) {
  CHECK(value == 0 || value == 1);
  a->meta = meta;
  a->value = value;
}

void ast_bool_literal_init_copy(struct ast_bool_literal *a,
                                struct ast_bool_literal *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  a->value = c->value;
}

void ast_bool_literal_destroy(struct ast_bool_literal *a) {
  ast_meta_destroy(&a->meta);
  a->value = 0;
}

void ast_void_literal_init(struct ast_void_literal *a,
                           struct ast_meta meta) {
  a->meta = meta;
}

void ast_void_literal_init_copy(struct ast_void_literal *a,
                                struct ast_void_literal *c) {
  a->meta = ast_meta_make_copy(&c->meta);
}

void ast_void_literal_destroy(struct ast_void_literal *a) {
  ast_meta_destroy(&a->meta);
}

void ast_null_literal_init(struct ast_null_literal *a,
                           struct ast_meta meta) {
  a->meta = meta;
}

void ast_null_literal_init_copy(struct ast_null_literal *a,
                                struct ast_null_literal *c) {
  a->meta = ast_meta_make_copy(&c->meta);
}

void ast_null_literal_destroy(struct ast_null_literal *a) {
  ast_meta_destroy(&a->meta);
}

void ast_char_literal_init(struct ast_char_literal *a,
                           struct ast_meta meta, uint8_t value) {
  a->meta = meta;
  a->value = value;
}

void ast_char_literal_init_copy(struct ast_char_literal *a,
                                struct ast_char_literal *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  a->value = c->value;
}

void ast_char_literal_destroy(struct ast_char_literal *a) {
  ast_meta_destroy(&a->meta);
  a->value = 0;
}

void ast_string_literal_init(struct ast_string_literal *a,
                             struct ast_meta meta,
                             uint8_t *values, size_t values_count) {
  a->meta = meta;
  a->values = values;
  a->values_count = values_count;
}

void ast_string_literal_init_copy(struct ast_string_literal *a,
                                  struct ast_string_literal *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  SLICE_INIT_COPY_PRIM(a->values, a->values_count, c->values, c->values_count);
}

void ast_string_literal_destroy(struct ast_string_literal *a) {
  ast_meta_destroy(&a->meta);
  free(a->values);
  a->values = NULL;
  a->values_count = 0;
}

void ast_funcall_init(struct ast_funcall *a, struct ast_meta meta,
                      struct ast_exprcall func,
                      struct ast_exprcall *args, size_t args_count) {
  a->meta = meta;
  ast_exprcall_alloc_move(func, &a->func);
  a->args = args;
  a->args_count = args_count;
}

void ast_funcall_init_copy(struct ast_funcall *a, struct ast_funcall *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_exprcall_alloc_init_copy(c->func, &a->func);

  SLICE_INIT_COPY(a->args, a->args_count, c->args, c->args_count, ast_exprcall_init_copy);
}

void ast_funcall_destroy(struct ast_funcall *a) {
  ast_meta_destroy(&a->meta);
  ast_exprcall_destroy(a->func);
  free(a->func);
  a->func = NULL;
  SLICE_FREE(a->args, a->args_count, ast_exprcall_destroy);
}

void ast_var_info_init(struct ast_var_info *a) {
  a->info_valid = 0;
}

void ast_var_info_init_copy(struct ast_var_info *a, struct ast_var_info *c) {
  a->info_valid = c->info_valid;
  if (c->info_valid) {
    a->varnum = c->varnum;
    ast_typeexpr_init_copy(&a->concrete_type, &c->concrete_type);
  }
}

void ast_var_info_destroy(struct ast_var_info *a) {
  if (a->info_valid) {
    a->varnum.value = SIZE_MAX;
    ast_typeexpr_destroy(&a->concrete_type);
    a->info_valid = 0;
  }
}

void ast_var_info_specify(struct ast_var_info *a, struct varnum varnum,
                          struct ast_typeexpr concrete_type) {
  CHECK(!a->info_valid);
  a->info_valid = 1;
  a->varnum = varnum;
  a->concrete_type = concrete_type;
}

struct varnum ast_var_info_varnum(struct ast_var_info *a) {
  CHECK(a->info_valid);
  return a->varnum;
}

struct ast_typeexpr *ast_var_info_type(struct ast_var_info *a) {
  CHECK(a->info_valid);
  return &a->concrete_type;
}

void ast_vardecl_init(struct ast_vardecl *a, struct ast_meta meta,
                      struct ast_ident name, struct ast_typeexpr type) {
  a->meta = meta;
  ast_var_info_init(&a->var_info);
  a->name = name;
  a->type = type;
}

void ast_vardecl_init_copy(struct ast_vardecl *a, struct ast_vardecl *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_var_info_init_copy(&a->var_info, &c->var_info);
  ast_ident_init_copy(&a->name, &c->name);
  ast_typeexpr_init_copy(&a->type, &c->type);
}

void ast_vardecl_destroy(struct ast_vardecl *a) {
  ast_meta_destroy(&a->meta);
  ast_var_info_destroy(&a->var_info);
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
  SLICE_INIT_COPY(a->statements, a->statements_count,
                  c->statements, c->statements_count,
                  ast_statement_init_copy);
}

void ast_bracebody_destroy(struct ast_bracebody *a) {
  ast_meta_destroy(&a->meta);
  SLICE_FREE(a->statements, a->statements_count, ast_statement_destroy);
}

void ast_var_statement_init_with_rhs(
    struct ast_var_statement *a, struct ast_meta meta,
    struct ast_vardecl decl, struct ast_exprcall rhs) {
  a->meta = meta;
  a->decl = decl;
  a->has_rhs = 1;
  ast_exprcall_alloc_move(rhs, &a->rhs);
}

void ast_var_statement_init_without_rhs(
    struct ast_var_statement *a, struct ast_meta meta,
    struct ast_vardecl decl) {
  a->meta = meta;
  a->decl = decl;
  a->has_rhs = 0;
  a->rhs = NULL;
}

struct ast_typeexpr *ast_var_statement_type(struct ast_var_statement *a) {
  return ast_var_info_type(&a->decl.var_info);
}

void ast_var_statement_init_copy(struct ast_var_statement *a,
                                 struct ast_var_statement *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_vardecl_init_copy(&a->decl, &c->decl);
  a->has_rhs = c->has_rhs;
  if (c->has_rhs) {
    ast_exprcall_alloc_init_copy(c->rhs, &a->rhs);
  } else {
    c->rhs = NULL;
  }
}

void ast_var_statement_destroy(struct ast_var_statement *a) {
  ast_meta_destroy(&a->meta);
  ast_vardecl_destroy(&a->decl);
  if (a->has_rhs) {
    a->has_rhs = 0;
    ast_exprcall_destroy(a->rhs);
    free(a->rhs);
    a->rhs = NULL;
  }
}

void ast_ifthen_statement_init(struct ast_ifthen_statement *a,
                               struct ast_meta meta,
                               struct ast_expr condition,
                               struct ast_bracebody body) {
  a->meta = meta;
  ast_expr_alloc_move(condition, &a->condition);
  a->body = body;
}

void ast_ifthen_statement_init_copy(struct ast_ifthen_statement *a,
                                    struct ast_ifthen_statement *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_expr_alloc_init_copy(c->condition, &a->condition);
  ast_bracebody_init_copy(&a->body, &c->body);
}

void ast_ifthen_statement_destroy(struct ast_ifthen_statement *a) {
  ast_meta_destroy(&a->meta);
  ast_expr_destroy(a->condition);
  free(a->condition);
  a->condition = NULL;
  ast_bracebody_destroy(&a->body);
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

void ast_while_statement_init(struct ast_while_statement *a,
                              struct ast_meta meta,
                              struct ast_expr condition,
                              struct ast_bracebody body) {
  a->meta = meta;
  ast_expr_alloc_move(condition, &a->condition);
  a->body = body;
}

void ast_while_statement_init_copy(struct ast_while_statement *a,
                                   struct ast_while_statement *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_expr_alloc_init_copy(c->condition, &a->condition);
  ast_bracebody_init_copy(&a->body, &c->body);
}

void ast_while_statement_destroy(struct ast_while_statement *a) {
  ast_meta_destroy(&a->meta);
  ast_expr_destroy(a->condition);
  free(a->condition);
  a->condition = NULL;
  ast_bracebody_destroy(&a->body);
}

void ast_for_statement_init(struct ast_for_statement *a,
                            struct ast_meta meta,
                            int has_initializer,
                            struct ast_statement *initializer,
                            int has_condition,
                            struct ast_expr *condition,
                            int has_increment,
                            struct ast_expr *increment,
                            struct ast_bracebody body) {
  a->meta = meta;
  CHECK(has_initializer || initializer == NULL);
  a->has_initializer = has_initializer;
  a->initializer = initializer;
  CHECK(has_condition || condition == NULL);
  a->has_condition = has_condition;
  a->condition = condition;
  CHECK(has_increment || increment == NULL);
  a->has_increment = has_increment;
  a->increment = increment;
  a->body = body;
}

void ast_for_statement_init_copy(struct ast_for_statement *a,
                                 struct ast_for_statement *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  a->has_initializer = c->has_initializer;
  if (c->has_initializer) {
    struct ast_statement *initializer = malloc(sizeof(*initializer));
    ast_statement_init_copy(initializer, c->initializer);
    a->initializer = initializer;
  } else {
    a->initializer = NULL;
  }

  a->has_condition = c->has_condition;
  if (c->has_condition) {
    ast_expr_alloc_init_copy(c->condition, &a->condition);
  } else {
    a->condition = NULL;
  }

  a->has_increment = c->has_increment;
  if (c->has_increment) {
    ast_expr_alloc_init_copy(c->increment, &a->increment);
  } else {
    a->increment = NULL;
  }

  ast_bracebody_init_copy(&a->body, &c->body);
}

void ast_for_statement_destroy(struct ast_for_statement *a) {
  ast_meta_destroy(&a->meta);
  if (a->has_initializer) {
    ast_statement_destroy(a->initializer);
    free(a->initializer);
    a->initializer = NULL;
  }
  if (a->has_condition) {
    ast_expr_destroy(a->condition);
    free(a->condition);
    a->condition = NULL;
  }
  if (a->has_increment) {
    ast_expr_destroy(a->increment);
    free(a->increment);
    a->increment = NULL;
  }
  ast_bracebody_destroy(&a->body);
}

void ast_switch_statement_init(struct ast_switch_statement *a,
                               struct ast_meta meta,
                               struct ast_expr swartch,
                               struct ast_cased_statement *cased_statements,
                               size_t cased_statements_count) {
  a->meta = meta;
  ast_expr_alloc_move(swartch, &a->swartch);
  a->cased_statements = cased_statements;
  a->cased_statements_count = cased_statements_count;
}

void ast_switch_statement_init_copy(struct ast_switch_statement *a,
                                    struct ast_switch_statement *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_expr_alloc_init_copy(c->swartch, &a->swartch);
  SLICE_INIT_COPY(a->cased_statements, a->cased_statements_count,
                  c->cased_statements, c->cased_statements_count,
                  ast_cased_statement_init_copy);
}

void ast_switch_statement_destroy(struct ast_switch_statement *a) {
  ast_meta_destroy(&a->meta);
  ast_expr_destroy(a->swartch);
  free(a->swartch);
  a->swartch = NULL;
  SLICE_FREE(a->cased_statements, a->cased_statements_count,
             ast_cased_statement_destroy);
}

void ast_return_statement_init(struct ast_return_statement *a,
                               struct ast_meta meta,
                               struct ast_expr expr) {
  a->meta = meta;
  a->has_expr = 1;
  ast_expr_alloc_move(expr, &a->expr);
}

void ast_return_statement_init_no_expr(struct ast_return_statement *a,
                                       struct ast_meta meta) {
  a->meta = meta;
  a->has_expr = 0;
}

void ast_return_statement_init_copy(struct ast_return_statement *a,
                                    struct ast_return_statement *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  a->has_expr = c->has_expr;
  if (c->has_expr) {
    ast_expr_alloc_init_copy(c->expr, &a->expr);
  }
}

void ast_return_statement_destroy(struct ast_return_statement *a) {
  ast_meta_destroy(&a->meta);
  if (a->has_expr) {
    a->has_expr = 0;
    ast_expr_destroy(a->expr);
    free(a->expr);
    a->expr = NULL;
  }
}

void ast_statement_init_copy(struct ast_statement *a,
                             struct ast_statement *c) {
  a->tag = c->tag;
  switch (c->tag) {
  case AST_STATEMENT_EXPR:
    ast_expr_alloc_init_copy(c->u.expr, &a->u.expr);
    break;
  case AST_STATEMENT_RETURN:
    ast_return_statement_init_copy(&a->u.return_statement,
                                   &c->u.return_statement);
    break;
  case AST_STATEMENT_VAR:
    ast_var_statement_init_copy(&a->u.var_statement, &c->u.var_statement);
    break;
  case AST_STATEMENT_IFTHEN:
    ast_ifthen_statement_init_copy(&a->u.ifthen_statement,
                                   &c->u.ifthen_statement);
    break;
  case AST_STATEMENT_IFTHENELSE:
    ast_ifthenelse_statement_init_copy(&a->u.ifthenelse_statement,
                                       &c->u.ifthenelse_statement);
    break;
  case AST_STATEMENT_WHILE:
    ast_while_statement_init_copy(&a->u.while_statement,
                                  &c->u.while_statement);
    break;
  case AST_STATEMENT_FOR:
    ast_for_statement_init_copy(&a->u.for_statement,
                                &c->u.for_statement);
    break;
  case AST_STATEMENT_SWITCH:
    ast_switch_statement_init_copy(&a->u.switch_statement,
                                   &c->u.switch_statement);
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
  case AST_STATEMENT_RETURN:
    ast_return_statement_destroy(&a->u.return_statement);
    break;
  case AST_STATEMENT_VAR:
    ast_var_statement_destroy(&a->u.var_statement);
    break;
  case AST_STATEMENT_IFTHEN:
    ast_ifthen_statement_destroy(&a->u.ifthen_statement);
    break;
  case AST_STATEMENT_IFTHENELSE:
    ast_ifthenelse_statement_destroy(&a->u.ifthenelse_statement);
    break;
  case AST_STATEMENT_WHILE:
    ast_while_statement_destroy(&a->u.while_statement);
    break;
  case AST_STATEMENT_FOR:
    ast_for_statement_destroy(&a->u.for_statement);
    break;
  case AST_STATEMENT_SWITCH:
    ast_switch_statement_destroy(&a->u.switch_statement);
    break;
  default:
    UNREACHABLE();
  }
  a->tag = (enum ast_statement_tag)-1;
}

void ast_statement_alloc_move(struct ast_statement movee,
                              struct ast_statement **out) {
  struct ast_statement *p = malloc(sizeof(*p));
  CHECK(p);
  *p = movee;
  *out = p;
}

void ast_case_pattern_info_init(struct ast_case_pattern_info *a) {
  a->info_valid = 0;
}

void ast_case_pattern_info_init_copy(struct ast_case_pattern_info *a,
                                     struct ast_case_pattern_info *c) {
  a->info_valid = c->info_valid;
  if (c->info_valid) {
    a->constructor_number = c->constructor_number;
  }
}

void ast_case_pattern_info_destroy(struct ast_case_pattern_info *a) {
  if (a->info_valid) {
    a->info_valid = 0;
    a->constructor_number = 0;
  }
}

size_t ast_case_pattern_info_constructor_number(struct ast_case_pattern_info *a) {
  CHECK(a->info_valid);
  return a->constructor_number;
}

void ast_case_pattern_info_specify(struct ast_case_pattern_info *a,
                                   size_t constructor_number) {
  CHECK(!a->info_valid);
  a->info_valid = 1;
  a->constructor_number = constructor_number;
}

void ast_case_pattern_init(struct ast_case_pattern *a,
                           struct ast_meta meta,
                           struct ast_ident constructor_name,
                           struct ast_vardecl decl) {
  a->meta = meta;
  ast_case_pattern_info_init(&a->info);
  a->is_default = 0;
  a->constructor_name = constructor_name;
  a->decl = decl;
}

void ast_case_pattern_init_default(struct ast_case_pattern *a,
                                   struct ast_meta meta) {
  a->meta = meta;
  ast_case_pattern_info_init(&a->info);
  a->is_default = 1;
}

void ast_case_pattern_init_copy(struct ast_case_pattern *a,
                                struct ast_case_pattern *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_case_pattern_info_init_copy(&a->info, &c->info);
  a->is_default = c->is_default;
  if (!c->is_default) {
    ast_ident_init_copy(&a->constructor_name, &c->constructor_name);
    ast_vardecl_init_copy(&a->decl, &c->decl);
  }
}

void ast_case_pattern_destroy(struct ast_case_pattern *a) {
  ast_meta_destroy(&a->meta);
  ast_case_pattern_info_destroy(&a->info);
  if (!a->is_default) {
    ast_ident_destroy(&a->constructor_name);
    ast_vardecl_destroy(&a->decl);
  }
}

void ast_cased_statement_init(struct ast_cased_statement *a,
                              struct ast_meta meta,
                              struct ast_case_pattern pattern,
                              struct ast_bracebody body) {
  a->meta = meta;
  a->pattern = pattern;
  a->body = body;
}

void ast_cased_statement_init_copy(struct ast_cased_statement *a,
                                   struct ast_cased_statement *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_case_pattern_init_copy(&a->pattern, &c->pattern);
  ast_bracebody_init_copy(&a->body, &c->body);
}

void ast_cased_statement_destroy(struct ast_cased_statement *a) {
  ast_meta_destroy(&a->meta);
  ast_case_pattern_destroy(&a->pattern);
  ast_bracebody_destroy(&a->body);
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
  SLICE_INIT_COPY(a->params, a->params_count, c->params, c->params_count,
                  ast_vardecl_init_copy);
  ast_typeexpr_init_copy(&a->return_type, &c->return_type);
  ast_bracebody_init_copy(&a->bracebody, &c->bracebody);
}

void ast_lambda_destroy(struct ast_lambda *a) {
  ast_meta_destroy(&a->meta);
  SLICE_FREE(a->params, a->params_count, ast_vardecl_destroy);
  ast_typeexpr_destroy(&a->return_type);
  ast_bracebody_destroy(&a->bracebody);
}

void ast_fieldname_init(struct ast_fieldname *a,
                        struct ast_meta meta,
                        struct ast_ident ident) {
  a->meta = meta;
  a->whole_field = 0;
  a->ident = ident;
}

void ast_fieldname_init_copy(struct ast_fieldname *a,
                             struct ast_fieldname *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  a->whole_field = c->whole_field;
  if (!c->whole_field) {
    ast_ident_init_copy(&a->ident, &c->ident);
  }
}

void ast_fieldname_init_whole(struct ast_fieldname *a,
                              struct ast_meta meta) {
  a->meta = meta;
  a->whole_field = 1;
}

void ast_fieldname_destroy(struct ast_fieldname *a) {
  ast_meta_destroy(&a->meta);
  if (!a->whole_field) {
    ast_ident_destroy(&a->ident);
  }
  a->whole_field = 0;
}

void ast_local_field_access_init(struct ast_local_field_access *a,
                                 struct ast_meta meta,
                                 struct ast_expr lhs,
                                 struct ast_fieldname fieldname) {
  a->meta = meta;
  ast_expr_alloc_move(lhs, &a->lhs);
  a->fieldname = fieldname;
}

void ast_local_field_access_init_copy(struct ast_local_field_access *a,
                                      struct ast_local_field_access *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_expr_alloc_init_copy(c->lhs, &a->lhs);
  ast_fieldname_init_copy(&a->fieldname, &c->fieldname);
}

void ast_local_field_access_destroy(struct ast_local_field_access *a) {
  ast_meta_destroy(&a->meta);
  ast_expr_destroy(a->lhs);
  free(a->lhs);
  ast_fieldname_destroy(&a->fieldname);
}

void ast_deref_field_access_init(struct ast_deref_field_access *a,
                                 struct ast_meta meta,
                                 struct ast_expr lhs,
                                 struct ast_fieldname fieldname) {
  a->meta = meta;
  ast_expr_alloc_move(lhs, &a->lhs);
  a->fieldname = fieldname;
}

void ast_deref_field_access_init_copy(struct ast_deref_field_access *a,
                                      struct ast_deref_field_access *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_expr_alloc_init_copy(c->lhs, &a->lhs);
  ast_fieldname_init_copy(&a->fieldname, &c->fieldname);
}

void ast_deref_field_access_destroy(struct ast_deref_field_access *a) {
  ast_meta_destroy(&a->meta);
  ast_expr_destroy(a->lhs);
  free(a->lhs);
  ast_fieldname_destroy(&a->fieldname);
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

void ast_name_expr_init(struct ast_name_expr *a,
                        struct ast_ident ident) {
  a->meta = ast_meta_make_copy(&ident.meta);
  ast_name_expr_info_init(&a->info);
  a->ident = ident;
  a->has_params = 0;
}

void ast_name_expr_init_with_params(struct ast_name_expr *a,
                                    struct ast_meta meta,
                                    struct ast_ident ident,
                                    struct ast_typeexpr *params,
                                    size_t params_count) {
  a->meta = meta;
  ast_name_expr_info_init(&a->info);
  a->ident = ident;
  a->has_params = 1;
  a->params = params;
  a->params_count = params_count;
}

void ast_name_expr_init_copy(struct ast_name_expr *a,
                             struct ast_name_expr *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_name_expr_info_init_copy(&a->info, &c->info);
  ast_ident_init_copy(&a->ident, &c->ident);
  a->has_params = c->has_params;
  if (c->has_params) {
    SLICE_INIT_COPY(a->params, a->params_count, c->params, c->params_count,
                    ast_typeexpr_init_copy);
  }
}

void ast_name_expr_destroy(struct ast_name_expr *a) {
  ast_meta_destroy(&a->meta);
  ast_name_expr_info_destroy(&a->info);
  ast_ident_destroy(&a->ident);
  if (a->has_params) {
    a->has_params = 0;
    SLICE_FREE(a->params, a->params_count, ast_typeexpr_destroy);
  }
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
                         struct ast_typeexpr type,
                         struct ast_expr expr) {
  a->meta = meta;
  a->type = type;
  ast_expr_alloc_move(expr, &a->expr);
}

void ast_typed_expr_init_copy(struct ast_typed_expr *a,
                              struct ast_typed_expr *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  ast_typeexpr_init_copy(&a->type, &c->type);
  ast_expr_alloc_init_copy(c->expr, &a->expr);
}

void ast_typed_expr_destroy(struct ast_typed_expr *a) {
  ast_meta_destroy(&a->meta);
  ast_typeexpr_destroy(&a->type);
  ast_expr_destroy(a->expr);
  free(a->expr);
  a->expr = NULL;
}

void ast_strinit_init(struct ast_strinit *a,
                      struct ast_meta meta,
                      struct ast_expr *exprs,
                      size_t exprs_count) {
  a->meta = meta;
  a->info.info_valid = 0;
  a->exprs = exprs;
  a->exprs_count = exprs_count;
}

void ast_strinit_init_copy(struct ast_strinit *a,
                           struct ast_strinit *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  a->info.info_valid = c->info.info_valid;
  if (c->info.info_valid) {
    ast_typeexpr_init_copy(&a->info.concrete_structe_type,
                           &c->info.concrete_structe_type);
  }
  SLICE_INIT_COPY(a->exprs, a->exprs_count, c->exprs, c->exprs_count,
                  ast_expr_init_copy);
}

void ast_strinit_destroy(struct ast_strinit *a) {
  ast_meta_destroy(&a->meta);
  if (a->info.info_valid) {
    ast_typeexpr_destroy(&a->info.concrete_structe_type);
    a->info.info_valid = 0;
  }
  SLICE_FREE(a->exprs, a->exprs_count, ast_expr_destroy);
}

struct ast_typeexpr *ast_strinit_struct_type(struct ast_strinit *a) {
  CHECK(a->info.info_valid);
  return &a->info.concrete_structe_type;
}

void ast_strinit_set_struct_type(struct ast_strinit *a,
                                 struct ast_typeexpr struct_type) {
  CHECK(struct_type.tag == AST_TYPEEXPR_STRUCTE);
  CHECK(struct_type.u.structe.fields_count == a->exprs_count);
  CHECK(!a->info.info_valid);
  a->info.info_valid = 1;
  a->info.concrete_structe_type = struct_type;
}

struct ast_expr_info ast_expr_info_default(void) {
  struct ast_expr_info ret;
  ret.typechecked = AST_TYPECHECKED_NO;
  return ret;
}

struct ast_expr_info ast_expr_info_incomplete_typed(struct ast_typeexpr partial_type) {
  struct ast_expr_info ret;
  ret.typechecked = AST_TYPECHECKED_INCOMPLETE;
  ret.type = partial_type;
  return ret;
}

struct ast_expr_info ast_expr_info_incomplete(void) {
  return ast_expr_info_incomplete_typed(ast_unknown_garbage());
}

struct ast_expr_info ast_expr_info_typechecked_no_temporary(
    int is_lvalue,
    struct ast_typeexpr concrete_type) {
  struct ast_expr_info ret;
  ret.typechecked = AST_TYPECHECKED_YES;
  ret.is_lvalue = is_lvalue;
  ret.type = concrete_type;
  ret.temporary_exists = 0;
  return ret;
}

struct ast_expr_info ast_expr_info_typechecked_trivial_temporary(
    int is_lvalue,
    struct ast_typeexpr concrete_type) {
  return ast_expr_info_typechecked_no_temporary(is_lvalue, concrete_type);
}

struct ast_expr_info ast_expr_info_typechecked_no_or_trivial_temporary(
    int is_lvalue,
    struct ast_typeexpr concrete_type) {
  return ast_expr_info_typechecked_no_temporary(is_lvalue, concrete_type);
}

struct ast_expr_info ast_expr_info_typechecked_temporary(
    int is_lvalue,
    struct ast_typeexpr concrete_type,
    struct ast_typeexpr temporary_type,
    int whole_thing,
    size_t temptag) {
  struct ast_expr_info ret;
  ret.typechecked = AST_TYPECHECKED_YES;
  ret.is_lvalue = is_lvalue;
  ret.type = concrete_type;
  ret.temporary_exists = 1;
  ret.temporary_type = temporary_type;
  ret.whole_thing = whole_thing;
  ret.temptag = temptag;
  return ret;
}

void ast_expr_info_init_copy(struct ast_expr_info *a, struct ast_expr_info *c) {
  a->typechecked = c->typechecked;
  switch (c->typechecked) {
  case AST_TYPECHECKED_NO:
    break;
  case AST_TYPECHECKED_YES: {
    a->is_lvalue = c->is_lvalue;
    ast_typeexpr_init_copy(&a->type, &c->type);
    a->temporary_exists = c->temporary_exists;
    if (c->temporary_exists) {
      ast_typeexpr_init_copy(&a->temporary_type, &c->temporary_type);
      a->whole_thing = c->whole_thing;
      a->temptag = c->temptag;
    }
  } break;
  case AST_TYPECHECKED_INCOMPLETE:
    ast_typeexpr_init_copy(&a->type, &c->type);
    break;
  default:
    UNREACHABLE();
  }
}

struct ast_expr_info ast_expr_info_typechecked_identical(
    struct ast_expr_info *info) {
  struct ast_expr_info ret;
  ast_expr_info_init_copy(&ret, info);
  return ret;
}

void ast_expr_info_destroy(struct ast_expr_info *a) {
  switch (a->typechecked) {
  case AST_TYPECHECKED_NO:
    break;
  case AST_TYPECHECKED_YES: {
    a->is_lvalue = 0;
    ast_typeexpr_destroy(&a->type);
    if (a->temporary_exists) {
      ast_typeexpr_destroy(&a->temporary_type);
      a->whole_thing = 0;
      a->temptag = 0;
      a->temporary_exists = 0;
    }
    a->typechecked = AST_TYPECHECKED_NO;
  } break;
  case AST_TYPECHECKED_INCOMPLETE:
    ast_typeexpr_destroy(&a->type);
    break;
  default:
    UNREACHABLE();
  }
}

struct ast_meta *ast_expr_ast_meta(struct ast_expr *a) {
  switch (a->tag) {
  case AST_EXPR_NAME: return &a->u.name.meta;
  case AST_EXPR_NUMERIC_LITERAL: return &a->u.numeric_literal.meta;
  case AST_EXPR_BOOL_LITERAL: return &a->u.bool_literal.meta;
  case AST_EXPR_NULL_LITERAL: return &a->u.null_literal.meta;
  case AST_EXPR_VOID_LITERAL: return &a->u.void_literal.meta;
  case AST_EXPR_CHAR_LITERAL: return &a->u.char_literal.meta;
  case AST_EXPR_STRING_LITERAL: return &a->u.string_literal.meta;
  case AST_EXPR_FUNCALL: return &a->u.funcall.meta;
  case AST_EXPR_INDEX: return &a->u.index_expr.meta;
  case AST_EXPR_UNOP: return &a->u.unop_expr.meta;
  case AST_EXPR_BINOP: return &a->u.binop_expr.meta;
  case AST_EXPR_LAMBDA: return &a->u.lambda.meta;
  case AST_EXPR_LOCAL_FIELD_ACCESS: return &a->u.local_field_access.meta;
  case AST_EXPR_DEREF_FIELD_ACCESS: return &a->u.deref_field_access.meta;
  case AST_EXPR_TYPED: return &a->u.typed_expr.meta;
  case AST_EXPR_STRINIT: return &a->u.strinit.meta;
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
  case AST_EXPR_BOOL_LITERAL:
    ast_bool_literal_init_copy(&a->u.bool_literal,
                               &c->u.bool_literal);
    break;
  case AST_EXPR_NULL_LITERAL:
    ast_null_literal_init_copy(&a->u.null_literal,
                               &c->u.null_literal);
    break;
  case AST_EXPR_VOID_LITERAL:
    ast_void_literal_init_copy(&a->u.void_literal,
                               &c->u.void_literal);
    break;
  case AST_EXPR_CHAR_LITERAL:
    ast_char_literal_init_copy(&a->u.char_literal,
                               &c->u.char_literal);
    break;
  case AST_EXPR_STRING_LITERAL:
    ast_string_literal_init_copy(&a->u.string_literal,
                                 &c->u.string_literal);
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
  case AST_EXPR_STRINIT:
    ast_strinit_init_copy(&a->u.strinit, &c->u.strinit);
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
  case AST_EXPR_BOOL_LITERAL:
    ast_bool_literal_destroy(&a->u.bool_literal);
    break;
  case AST_EXPR_NULL_LITERAL:
    ast_null_literal_destroy(&a->u.null_literal);
    break;
  case AST_EXPR_VOID_LITERAL:
    ast_void_literal_destroy(&a->u.void_literal);
    break;
  case AST_EXPR_CHAR_LITERAL:
    ast_char_literal_destroy(&a->u.char_literal);
    break;
  case AST_EXPR_STRING_LITERAL:
    ast_string_literal_destroy(&a->u.string_literal);
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
  case AST_EXPR_STRINIT:
    ast_strinit_destroy(&a->u.strinit);
    break;
  default:
    UNREACHABLE();
  }
  ast_expr_info_destroy(&a->info);
  a->tag = (enum ast_expr_tag)-1;
}

void ast_expr_update(struct ast_expr *a,
                     struct ast_expr_info expr_info) {
  CHECK(a->info.typechecked != AST_TYPECHECKED_YES);
  ast_expr_info_destroy(&a->info);
  a->info = expr_info;
}

struct ast_typeexpr *ast_expr_type(struct ast_expr *a) {
  CHECK(a->info.typechecked == AST_TYPECHECKED_YES);
  return &a->info.type;
}

struct ast_typeexpr *ast_expr_partial_type(struct ast_expr *a) {
  CHECK(a->info.typechecked != AST_TYPECHECKED_NO);
  return &a->info.type;
}


int ast_expr_checked_and_complete(struct ast_expr *a) {
  return a->info.typechecked == AST_TYPECHECKED_YES;
}

int ast_expr_not_incomplete(struct ast_expr *a) {
  CHECK(a->info.typechecked != AST_TYPECHECKED_NO);
  return a->info.typechecked == AST_TYPECHECKED_YES;
}

int ast_expr_incomplete(struct ast_expr *a) {
  CHECK(a->info.typechecked != AST_TYPECHECKED_NO);
  return a->info.typechecked == AST_TYPECHECKED_INCOMPLETE;
}

int ast_expr_is_lvalue(struct ast_expr *a) {
  CHECK(a->info.typechecked == AST_TYPECHECKED_YES);
  return a->info.is_lvalue;
}

void ast_expr_alloc_move(struct ast_expr movee, struct ast_expr **out) {
  struct ast_expr *p = malloc(sizeof(*p));
  CHECK(p);
  *p = movee;
  *out = p;
}

void ast_expr_alloc_init_copy(struct ast_expr *c, struct ast_expr **out) {
  struct ast_expr *a = malloc(sizeof(*a));
  CHECK(a);
  ast_expr_init_copy(a, c);
  *out = a;
}

void ast_exprcatch_init(struct ast_exprcatch *a) {
  a->info_valid = 0;
  a->behavior = (enum ast_exprcatch_behavior)-1;
}

void ast_exprcatch_init_annotated(struct ast_exprcatch *a,
                                  enum ast_exprcatch_behavior behavior) {
  a->info_valid = 1;
  a->behavior = behavior;
}

void ast_exprcatch_init_copy(struct ast_exprcatch *a, struct ast_exprcatch *c) {
  a->info_valid = c->info_valid;
  if (c->info_valid) {
    a->behavior = c->behavior;
  } else {
    a->behavior = (enum ast_exprcatch_behavior)-1;
  }
}

void ast_exprcatch_destroy(struct ast_exprcatch *a) {
  a->info_valid = 0;
  a->behavior = (enum ast_exprcatch_behavior)-1;
}

void ast_exprcall_init(struct ast_exprcall *a, struct ast_expr expr) {
  ast_exprcatch_init(&a->catch);
  a->expr = expr;
}

void ast_exprcall_annotate(struct ast_exprcall *a,
                           struct ast_exprcatch catch) {
  CHECK(ast_expr_checked_and_complete(&a->expr));
  CHECK(!a->catch.info_valid);
  CHECK(catch.info_valid);
  ast_exprcatch_destroy(&a->catch);
  a->catch = catch;
}

void ast_exprcall_init_copy(struct ast_exprcall *a, struct ast_exprcall *c) {
  ast_exprcatch_init_copy(&a->catch, &c->catch);
  ast_expr_init_copy(&a->expr, &c->expr);
}

void ast_exprcall_destroy(struct ast_exprcall *a) {
  ast_exprcatch_destroy(&a->catch);
  ast_expr_destroy(&a->expr);
}

struct ast_exprcall ast_exprcall_make(struct ast_expr expr) {
  struct ast_exprcall ret;
  ast_exprcall_init(&ret, expr);
  return ret;
}

void ast_exprcall_alloc_move(struct ast_exprcall a,
                             struct ast_exprcall **out) {
  struct ast_exprcall *p = malloc(sizeof(*p));
  CHECK(p);
  *p = a;
  *out = p;
}

void ast_exprcall_alloc_init_copy(struct ast_exprcall *c,
                                  struct ast_exprcall **out) {
  struct ast_exprcall *a = malloc(sizeof(*a));
  CHECK(a);
  ast_exprcall_init_copy(a, c);
  *out = a;
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
  SLICE_INIT_COPY(a->params, a->params_count, c->params, c->params_count,
                  ast_typeexpr_init_copy);
}

void ast_typeapp_destroy(struct ast_typeapp *a) {
  ast_meta_destroy(&a->meta);
  ast_ident_destroy(&a->name);
  SLICE_FREE(a->params, a->params_count, ast_typeexpr_destroy);
}

void ast_structe_init(struct ast_structe *a, struct ast_meta meta,
                      struct ast_vardecl *fields, size_t fields_count) {
  a->meta = meta;
  a->fields = fields;
  a->fields_count = fields_count;
}

void ast_structe_init_copy(struct ast_structe *a, struct ast_structe *c) {
  a->meta = ast_meta_make_copy(&c->meta);
  SLICE_INIT_COPY(a->fields, a->fields_count, c->fields, c->fields_count,
                  ast_vardecl_init_copy);
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
  SLICE_INIT_COPY(a->fields, a->fields_count, c->fields, c->fields_count,
                  ast_vardecl_init_copy);
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

void ast_unknown_init(struct ast_unknown *a, struct ast_meta meta) {
  a->meta = meta;
}

void ast_unknown_init_copy(struct ast_unknown *a, struct ast_unknown *c) {
  a->meta = ast_meta_make_copy(&c->meta);
}

void ast_unknown_destroy(struct ast_unknown *a) {
  ast_meta_destroy(&a->meta);
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
    ast_unknown_init_copy(&a->u.unknown, &c->u.unknown);
    break;
  case AST_TYPEEXPR_NUMERIC:
    ast_unknown_init_copy(&a->u.numeric, &c->u.numeric);
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
    ast_unknown_destroy(&a->u.unknown);
    break;
  case AST_TYPEEXPR_NUMERIC:
    ast_unknown_destroy(&a->u.numeric);
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
  case AST_TYPEEXPR_UNKNOWN: return &a->u.unknown.meta;
  case AST_TYPEEXPR_NUMERIC: return &a->u.numeric.meta;
  default:
    UNREACHABLE();
  }
}

struct ast_typeexpr ast_unknown_garbage(void) {
  struct ast_typeexpr ret;
  ret.tag = AST_TYPEEXPR_UNKNOWN;
  ast_unknown_init(&ret.u.unknown, ast_meta_make_garbage());
  return ret;
}

struct ast_typeexpr ast_numeric_garbage(void) {
  struct ast_typeexpr ret;
  ret.tag = AST_TYPEEXPR_NUMERIC;
  ast_unknown_init(&ret.u.numeric, ast_meta_make_garbage());
  return ret;
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
    SLICE_INIT_COPY(a->params, a->params_count, c->params, c->params_count,
                    ast_ident_init_copy);
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
                  int is_export, struct ast_generics generics,
                  struct ast_ident name, struct ast_typeexpr typeexpr,
                  struct ast_expr rhs) {
  if (is_export) {
    CHECK(!generics.has_type_params);
  }
  a->meta = meta;
  a->is_export = is_export;
  a->generics = generics;
  a->name = name;
  a->has_typeexpr = 1;
  a->typeexpr = typeexpr;
  a->rhs = rhs;
}

void ast_def_init_no_type(struct ast_def *a, struct ast_meta meta,
                          int is_export, struct ast_generics generics,
                          struct ast_ident name, struct ast_expr rhs) {
  if (is_export) {
    CHECK(!generics.has_type_params);
  }
  a->meta = meta;
  a->is_export = is_export;
  a->generics = generics;
  a->name = name;
  a->has_typeexpr = 0;
  a->rhs = rhs;
}

struct ast_typeexpr *ast_def_typeexpr(struct ast_def *a) {
  CHECK(a->has_typeexpr);
  return &a->typeexpr;
}

void ast_def_destroy(struct ast_def *a) {
  ast_meta_destroy(&a->meta);
  a->is_export = 0;
  ast_generics_destroy(&a->generics);
  ast_ident_destroy(&a->name);
  if (a->has_typeexpr) {
    ast_typeexpr_destroy(&a->typeexpr);
    a->has_typeexpr = 0;
  }
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

void ast_enumspec_init(struct ast_enumspec *a,
                       struct ast_vardecl *enumfields,
                       size_t enumfields_count) {
  a->enumfields = enumfields;
  a->enumfields_count = enumfields_count;
}

void ast_enumspec_init_copy(struct ast_enumspec *a, struct ast_enumspec *c) {
  SLICE_INIT_COPY(a->enumfields, a->enumfields_count,
                  c->enumfields, c->enumfields_count,
                  ast_vardecl_init_copy);
}

void ast_enumspec_destroy(struct ast_enumspec *a) {
  SLICE_FREE(a->enumfields, a->enumfields_count, ast_vardecl_destroy);
}

void ast_deftype_rhs_init_type(struct ast_deftype_rhs *a, struct ast_typeexpr type) {
  a->tag = AST_DEFTYPE_RHS_TYPE;
  a->u.type = type;
}

void ast_deftype_rhs_init_enumspec(struct ast_deftype_rhs *a, struct ast_enumspec enumspec) {
  a->tag = AST_DEFTYPE_RHS_ENUMSPEC;
  a->u.enumspec = enumspec;
}

void ast_deftype_rhs_init_copy(struct ast_deftype_rhs *a, struct ast_deftype_rhs *c) {
  a->tag = c->tag;
  switch (c->tag) {
  case AST_DEFTYPE_RHS_TYPE:
    ast_typeexpr_init_copy(&a->u.type, &c->u.type);
    break;
  case AST_DEFTYPE_RHS_ENUMSPEC:
    ast_enumspec_init_copy(&a->u.enumspec, &c->u.enumspec);
    break;
  default:
    UNREACHABLE();
  }
}

void ast_deftype_rhs_destroy(struct ast_deftype_rhs *a) {
  switch (a->tag) {
  case AST_DEFTYPE_RHS_TYPE:
    ast_typeexpr_destroy(&a->u.type);
    break;
  case AST_DEFTYPE_RHS_ENUMSPEC:
    ast_enumspec_destroy(&a->u.enumspec);
    break;
  default:
    UNREACHABLE();
  }
  a->tag = (enum ast_deftype_rhs_tag)-1;
}

void ast_deftype_init(struct ast_deftype *a, struct ast_meta meta,
                      enum ast_deftype_disposition disposition,
                      struct ast_generics generics,
                      struct ast_ident name, struct ast_typeexpr type) {
  a->meta = meta;
  a->disposition = disposition;
  a->generics = generics;
  a->name = name;
  ast_deftype_rhs_init_type(&a->rhs, type);
}

void ast_deftype_init_enum(struct ast_deftype *a, struct ast_meta meta,
                           struct ast_generics generics,
                           struct ast_ident name,
                           struct ast_vardecl *enumfields,
                           size_t enumfields_count) {
  a->meta = meta;
  a->disposition = AST_DEFTYPE_NOT_CLASS;
  a->generics = generics;
  a->name = name;
  struct ast_enumspec enumspec;
  ast_enumspec_init(&enumspec, enumfields, enumfields_count);
  ast_deftype_rhs_init_enumspec(&a->rhs, enumspec);
}

void ast_deftype_destroy(struct ast_deftype *a) {
  ast_meta_destroy(&a->meta);
  a->disposition = (enum ast_deftype_disposition)-1;
  ast_generics_destroy(&a->generics);
  ast_ident_destroy(&a->name);
  ast_deftype_rhs_destroy(&a->rhs);
}


void ast_access_init(struct ast_access *a, struct ast_meta meta,
                     struct ast_ident name, struct generics_arity arity,
                     struct ast_toplevel *toplevels, size_t toplevels_count) {
  a->meta = meta;
  a->name = name;
  a->arity = arity;
  a->toplevels = toplevels;
  a->toplevels_count = toplevels_count;
}

void ast_access_destroy(struct ast_access *a) {
  ast_meta_destroy(&a->meta);
  ast_ident_destroy(&a->name);
  SLICE_FREE(a->toplevels, a->toplevels_count, ast_toplevel_destroy);
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
  case AST_TOPLEVEL_ACCESS:
    ast_access_destroy(&a->u.access);
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

