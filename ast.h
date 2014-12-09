#ifndef KIRA_AST_H_
#define KIRA_AST_H_

#include <stddef.h>
#include <stdint.h>

#include "identmap.h"

struct ast_ident {
  ident_value value;
};

void ast_ident_destroy(struct ast_ident *a);

struct ast_numeric_literal {
  int8_t *digits;
  size_t digits_count;
};

struct ast_funcall {
  struct ast_expr *func;
  struct ast_expr *args;
  size_t args_count;
};

struct ast_typeexpr;

struct ast_typeapp {
  struct ast_ident name;
  struct ast_typeexpr *params;
  size_t params_count;
};

struct ast_structe {
  struct ast_vardecl *fields;
  size_t fields_count;
};

struct ast_unione {
  struct ast_vardecl *fields;
  size_t fields_count;
};

enum ast_typeexpr_tag {
  AST_TYPEEXPR_NAME,
  AST_TYPEEXPR_APP,
  AST_TYPEEXPR_STRUCTE,
  AST_TYPEEXPR_UNIONE,
};

struct ast_typeexpr {
  enum ast_typeexpr_tag tag;
  union {
    struct ast_ident name;
    struct ast_typeapp app;
    struct ast_structe structe;
    struct ast_unione unione;
  } u;
};

void ast_typeexpr_destroy(struct ast_typeexpr *a);

struct ast_vardecl {
  struct ast_ident name;
  struct ast_typeexpr type;
};

void ast_vardecl_destroy(struct ast_vardecl *a);

struct ast_expr;

struct ast_statement;

struct ast_bracebody {
  struct ast_statement *statements;
  size_t statements_count;
};

void ast_bracebody_destroy(struct ast_bracebody *a);

struct ast_var_statement {
  struct ast_ident name;
  struct ast_typeexpr type;
  struct ast_expr *rhs;
};

struct ast_goto_statement {
  struct ast_ident target;
};

struct ast_label_statement {
  struct ast_ident label;
};

struct ast_ifthen_statement {
  struct ast_expr *condition;
  struct ast_bracebody thenbody;
};

struct ast_ifthenelse_statement {
  struct ast_expr *condition;
  struct ast_bracebody thenbody;
  struct ast_bracebody elsebody;
};

enum ast_statement_tag {
  AST_STATEMENT_EXPR,
  AST_STATEMENT_RETURN_EXPR,
  AST_STATEMENT_VAR,
  AST_STATEMENT_GOTO,
  AST_STATEMENT_LABEL,
  AST_STATEMENT_IFTHEN,
  AST_STATEMENT_IFTHENELSE,
};

struct ast_statement {
  enum ast_statement_tag tag;
  union {
    struct ast_expr *expr;
    struct ast_expr *return_expr;
    struct ast_var_statement var_statement;
    struct ast_goto_statement goto_statement;
    struct ast_label_statement label_statement;
    struct ast_ifthen_statement ifthen_statement;
    struct ast_ifthenelse_statement ifthenelse_statement;
  } u;
};

void ast_statement_destroy(struct ast_statement *a);

struct ast_lambda {
  struct ast_vardecl *params;
  size_t params_count;
  struct ast_typeexpr return_type;
  struct ast_bracebody bracebody;
};

enum ast_unop {
  AST_UNOP_DEREFERENCE,
};

struct ast_unop_expr {
  enum ast_unop operator;
  struct ast_expr *rhs;
};

/* See also: binop_precedence in parse.c. */
enum ast_binop {
  AST_BINOP_ASSIGN,
  AST_BINOP_ADD,
  AST_BINOP_SUB,
  AST_BINOP_MUL,
  AST_BINOP_DIV,
  AST_BINOP_MOD,
  AST_BINOP_LT,
  AST_BINOP_LE,
  AST_BINOP_GT,
  AST_BINOP_GE,
  AST_BINOP_EQ,
  AST_BINOP_NE,
  AST_BINOP_BIT_XOR,
  AST_BINOP_BIT_OR,
  AST_BINOP_BIT_AND,
  AST_BINOP_BIT_LEFTSHIFT,
  AST_BINOP_BIT_RIGHTSHIFT,
  AST_BINOP_LOGICAL_OR,
  AST_BINOP_LOGICAL_AND,
};

struct ast_binop_expr {
  enum ast_binop operator;
  struct ast_expr *lhs;
  struct ast_expr *rhs;
};

enum ast_expr_tag {
  AST_EXPR_NAME,
  AST_EXPR_NUMERIC_LITERAL,
  AST_EXPR_FUNCALL,
  AST_EXPR_UNOP,
  AST_EXPR_BINOP,
  AST_EXPR_LAMBDA,
  AST_EXPR_LOCAL_FIELD_ACCESS,
  AST_EXPR_DEREFERENCING_FIELD_ACCESS,
};

struct ast_expr {
  enum ast_expr_tag tag;
  union {
    struct ast_ident name;
    struct ast_numeric_literal numeric_literal;
    struct ast_funcall funcall;
    struct ast_unop_expr unop_expr;
    struct ast_binop_expr binop_expr;
    struct ast_lambda lambda;
    struct ast_ident local_field_access;
    struct ast_ident dereferencing_field_access;
  } u;
};

void ast_expr_destroy(struct ast_expr *a);

struct ast_optional_type_params {
  int has_type_params;  /* 0 or 1 -- params is uninitialized if 0. */
  struct ast_ident *params;
  size_t params_count;
};

void ast_optional_type_params_destroy(struct ast_optional_type_params *a);

struct ast_def {
  struct ast_optional_type_params generics;
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
  struct ast_ident name;
};

struct ast_deftype {
  struct ast_optional_type_params generics;
  struct ast_ident name;
  struct ast_typeexpr type;
};

enum ast_toplevel_tag {
  AST_TOPLEVEL_IMPORT,
  AST_TOPLEVEL_MODULE,
  AST_TOPLEVEL_DEF,
  AST_TOPLEVEL_DEFTYPE,
};

struct ast_toplevel {
  enum ast_toplevel_tag tag;
  union {
    struct ast_import import;
    struct ast_module module;
    struct ast_def def;
    struct ast_deftype deftype;
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

void ast_file_destroy(struct ast_file *a);

#endif /* KIRA_AST_H_ */
