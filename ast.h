#ifndef KIRA_AST_H_
#define KIRA_AST_H_

#include <stddef.h>
#include <stdint.h>

#include "identmap.h"

struct ast_meta {
  size_t pos_start;
  size_t pos_end;
};

struct ast_meta ast_meta_make(size_t pos_start, size_t pos_end);

struct ast_ident {
  struct ast_meta meta;
  ident_value value;
};

void ast_ident_init(struct ast_ident *a, struct ast_meta meta,
		    ident_value value);
void ast_ident_destroy(struct ast_ident *a);

struct ast_numeric_literal {
  struct ast_meta meta;
  int8_t *digits;
  size_t digits_count;
};

void ast_numeric_literal_init(struct ast_numeric_literal *a,
			      struct ast_meta meta, int8_t *digits,
			      size_t digits_count);

struct ast_funcall {
  struct ast_meta meta;
  struct ast_expr *func;
  struct ast_expr *args;
  size_t args_count;
};

void ast_funcall_init(struct ast_funcall *a, struct ast_meta meta,
		      struct ast_expr *func, struct ast_expr *args,
		      size_t args_count);

struct ast_typeexpr;

struct ast_typeapp {
  struct ast_meta meta;
  struct ast_ident name;
  struct ast_typeexpr *params;
  size_t params_count;
};

void ast_typeapp_init(struct ast_typeapp *a, struct ast_meta meta,
		      struct ast_ident name, struct ast_typeexpr *params,
		      size_t params_count);

struct ast_structe {
  struct ast_meta meta;
  struct ast_vardecl *fields;
  size_t fields_count;
};

void ast_structe_init(struct ast_structe *a, struct ast_meta meta,
		      struct ast_vardecl *fields, size_t fields_count);

struct ast_unione {
  struct ast_meta meta;
  struct ast_vardecl *fields;
  size_t fields_count;
};

void ast_unione_init(struct ast_unione *a, struct ast_meta meta,
		     struct ast_vardecl *fields, size_t fields_count);

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
  struct ast_meta meta;
  struct ast_ident name;
  struct ast_typeexpr type;
};

void ast_vardecl_init(struct ast_vardecl *a, struct ast_meta meta,
		      struct ast_ident name, struct ast_typeexpr type);
void ast_vardecl_destroy(struct ast_vardecl *a);

struct ast_expr;

struct ast_statement;

struct ast_bracebody {
  struct ast_meta meta;
  struct ast_statement *statements;
  size_t statements_count;
};

void ast_bracebody_init(struct ast_bracebody *a,
			struct ast_meta meta,
			struct ast_statement *statements,
			size_t statements_count);
void ast_bracebody_destroy(struct ast_bracebody *a);

struct ast_var_statement {
  struct ast_meta meta;
  struct ast_ident name;
  struct ast_typeexpr type;
  struct ast_expr *rhs;
};

void ast_var_statement_init(struct ast_var_statement *a, struct ast_meta meta,
			    struct ast_ident name, struct ast_typeexpr type,
			    struct ast_expr *rhs);

struct ast_goto_statement {
  struct ast_meta meta;
  struct ast_ident target;
};

void ast_goto_statement_init(struct ast_goto_statement *a,
			     struct ast_meta meta, struct ast_ident target);

struct ast_label_statement {
  struct ast_meta meta;
  struct ast_ident label;
};

void ast_label_statement_init(struct ast_label_statement *a,
			      struct ast_meta meta, struct ast_ident label);

struct ast_ifthen_statement {
  struct ast_meta meta;
  struct ast_expr *condition;
  struct ast_bracebody thenbody;
};

void ast_ifthen_statement_init(struct ast_ifthen_statement *a,
			       struct ast_meta meta,
			       struct ast_expr *condition,
			       struct ast_bracebody thenbody);

struct ast_ifthenelse_statement {
  struct ast_meta meta;
  struct ast_expr *condition;
  struct ast_bracebody thenbody;
  struct ast_bracebody elsebody;
};

void ast_ifthenelse_statement_init(struct ast_ifthenelse_statement *a,
				   struct ast_meta meta,
				   struct ast_expr *condition,
				   struct ast_bracebody thenbody,
				   struct ast_bracebody elsebody);

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
  struct ast_meta meta;
  struct ast_vardecl *params;
  size_t params_count;
  struct ast_typeexpr return_type;
  struct ast_bracebody bracebody;
};

void ast_lambda_init(struct ast_lambda *a, struct ast_meta meta,
		     struct ast_vardecl *params, size_t params_count,
		     struct ast_typeexpr return_type,
		     struct ast_bracebody bracebody);

enum ast_unop {
  AST_UNOP_DEREFERENCE,
  AST_UNOP_ADDRESSOF,
  AST_UNOP_NEGATE,
};

struct ast_unop_expr {
  struct ast_meta meta;
  enum ast_unop operator;
  struct ast_expr *rhs;
};

void ast_unop_expr_init(struct ast_unop_expr *a, struct ast_meta meta,
			enum ast_unop operator, struct ast_expr *rhs);

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
  struct ast_meta meta;
  enum ast_binop operator;
  struct ast_expr *lhs;
  struct ast_expr *rhs;
};

void ast_binop_expr_init(struct ast_binop_expr *a, struct ast_meta meta,
			 enum ast_binop operator, struct ast_expr *lhs,
			 struct ast_expr *rhs);

struct ast_local_field_access {
  struct ast_meta meta;
  struct ast_ident fieldname;
};

void ast_local_field_access_init(struct ast_local_field_access *a,
				 struct ast_meta meta,
				 struct ast_ident fieldname);

struct ast_deref_field_access {
  struct ast_meta meta;
  struct ast_ident fieldname;
};

void ast_deref_field_access_init(struct ast_deref_field_access *a,
				 struct ast_meta meta,
				 struct ast_ident fieldname);

enum ast_expr_tag {
  AST_EXPR_NAME,
  AST_EXPR_NUMERIC_LITERAL,
  AST_EXPR_FUNCALL,
  AST_EXPR_UNOP,
  AST_EXPR_BINOP,
  AST_EXPR_LAMBDA,
  AST_EXPR_LOCAL_FIELD_ACCESS,
  AST_EXPR_DEREF_FIELD_ACCESS,
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
    struct ast_local_field_access local_field_access;
    struct ast_deref_field_access deref_field_access;
  } u;
};

size_t ast_expr_pos_end(struct ast_expr *a);
void ast_expr_destroy(struct ast_expr *a);

struct ast_optional_type_params {
  int has_type_params;  /* 0 or 1 -- meta & params is uninitialized if 0. */
  struct ast_meta meta;
  struct ast_ident *params;
  size_t params_count;
};

void ast_optional_type_params_init_no_params(
    struct ast_optional_type_params *a);
void ast_optional_type_params_init_has_params(
    struct ast_optional_type_params *a,
    struct ast_meta meta,
    struct ast_ident *params,
    size_t params_count);
void ast_optional_type_params_destroy(struct ast_optional_type_params *a);

struct ast_def {
  struct ast_meta meta;
  struct ast_optional_type_params generics;
  struct ast_ident name;
  struct ast_typeexpr type;
  struct ast_expr rhs;
};

void ast_def_init(struct ast_def *a, struct ast_meta meta,
		  struct ast_optional_type_params generics,
		  struct ast_ident name, struct ast_typeexpr type,
		  struct ast_expr rhs);

struct ast_toplevel;

struct ast_module {
  struct ast_meta meta;
  struct ast_ident name;
  struct ast_toplevel *toplevels;
  size_t toplevels_count;
};

void ast_module_init(struct ast_module *a, struct ast_meta meta,
		     struct ast_ident name, struct ast_toplevel *toplevels,
		     size_t toplevels_count);

struct ast_import {
  struct ast_meta meta;
  struct ast_ident name;
};

void ast_import_init(struct ast_import *a, struct ast_meta meta,
		     struct ast_ident name);

struct ast_deftype {
  struct ast_meta meta;
  struct ast_optional_type_params generics;
  struct ast_ident name;
  struct ast_typeexpr type;
};

void ast_deftype_init(struct ast_deftype *a, struct ast_meta meta,
		      struct ast_optional_type_params generics,
		      struct ast_ident name, struct ast_typeexpr type);

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
