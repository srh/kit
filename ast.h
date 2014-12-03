#ifndef KIRA_AST_H_
#define KIRA_AST_H_

#include <stddef.h>
#include <stdint.h>

#include "identmap.h"

struct ast_ident {
  ident_value value;
};

struct ast_numeric_literal {
  /* TODO: Support numeric literals other than decimal integers. */
  int8_t *digits;
  size_t digits_count;
};

struct ast_funcall {
  struct ast_expr *func;
  struct ast_expr *args;
  size_t args_count;
};

enum ast_expr_tag {
  AST_EXPR_NAME,
  AST_EXPR_NUMERIC_LITERAL,
  AST_EXPR_FUNCALL,
};

struct ast_expr {
  enum ast_expr_tag tag;
  union {
    struct ast_ident name;
    struct ast_numeric_literal numeric_literal;
    struct ast_funcall funcall;
    /* TODO: Support other expression types. */
  } u;
};

void ast_expr_destroy(struct ast_expr *a);

struct ast_typeexpr;

struct ast_typeapp {
  struct ast_ident name;
  struct ast_typeexpr *params;
  size_t params_count;
};

enum ast_typeexpr_tag {
  AST_TYPEEXPR_NAME,
  AST_TYPEEXPR_APP,
};

struct ast_typeexpr {
  enum ast_typeexpr_tag tag;
  union {
    struct ast_ident name;
    struct ast_typeapp app;
  } u;
};

void ast_typeexpr_destroy(struct ast_typeexpr *a);

struct ast_def {
  struct ast_ident name;
  struct ast_typeexpr type;
  struct ast_expr rhs;
};

struct ast_toplevel;

struct ast_module {
  struct ast_ident name;
  struct ast_toplevel *toplevels;
  size_t toplevels_count;
};

struct ast_import {
  struct ast_ident ident;
};

enum ast_toplevel_tag {
  AST_TOPLEVEL_IMPORT,
  AST_TOPLEVEL_MODULE,
  AST_TOPLEVEL_DEF,
};

struct ast_toplevel {
  enum ast_toplevel_tag tag;
  union {
    struct ast_import import;
    struct ast_module module;
    struct ast_def def;
  } u;
};

void ast_toplevel_destroy(struct ast_toplevel *a);

struct ast_file {
  struct ast_toplevel *toplevels;
  size_t toplevels_count;
};

void ast_file_init(struct ast_file *a,
		   struct ast_toplevel *toplevels,
		   size_t toplevels_count);

#endif /* KIRA_AST_H_ */
