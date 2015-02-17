#ifndef KIRA_AST_H_
#define KIRA_AST_H_

#include <stddef.h>
#include <stdint.h>

#include "arity.h"
#include "identmap.h"
#include "pos.h"

struct def_instantiation;

struct ast_meta {
  struct pos pos_start;
  struct pos pos_end;
};

struct ast_meta ast_meta_make(struct pos start, struct pos end);
struct ast_meta ast_meta_make_copy(struct ast_meta *c);
struct ast_meta ast_meta_make_garbage(void);
void ast_meta_destroy(struct ast_meta *a);

struct ast_ident {
  struct ast_meta meta;
  ident_value value;
};

void ast_ident_init(struct ast_ident *a, struct ast_meta meta,
                    ident_value value);
void ast_ident_init_copy(struct ast_ident *a, struct ast_ident *c);
void ast_ident_destroy(struct ast_ident *a);

enum ast_numeric_type {
  AST_NUMERIC_TYPE_SIGNED,
  AST_NUMERIC_TYPE_UNSIGNED,
};

struct ast_numeric_literal {
  struct ast_meta meta;
  int8_t *digits;
  size_t digits_count;
  enum ast_numeric_type numeric_type;
};

void ast_numeric_literal_init(struct ast_numeric_literal *a,
                              struct ast_meta meta, int8_t *digits,
                              size_t digits_count,
                              enum ast_numeric_type numeric_type);
void ast_numeric_literal_init_copy(struct ast_numeric_literal *a,
                                   struct ast_numeric_literal *c);

struct ast_char_literal {
  struct ast_meta meta;
  uint8_t value;
  uint8_t padding_ignore[3];  /* Get cl to shut up. */
};

void ast_char_literal_init(struct ast_char_literal *a,
                           struct ast_meta meta, uint8_t value);
void ast_char_literal_init_copy(struct ast_char_literal *a,
                                struct ast_char_literal *c);

struct ast_string_literal {
  struct ast_meta meta;
  uint8_t *values;
  size_t values_count;
};

void ast_string_literal_init(struct ast_string_literal *a,
                             struct ast_meta meta,
                             uint8_t *values, size_t values_count);
void ast_string_literal_init_copy(struct ast_string_literal *a,
                                  struct ast_string_literal *c);

struct ast_funcall {
  struct ast_meta meta;
  struct ast_expr *func;
  struct ast_exprcall *args;
  size_t args_count;
};

void ast_funcall_init(struct ast_funcall *a, struct ast_meta meta,
                      struct ast_expr func,
                      struct ast_exprcall *args, size_t args_count);

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

struct ast_arraytype {
  struct ast_meta meta;
  /* TODO: This should be a (statically evaluable) expr.  But such an
  expr would need to be analyzed for sizeof and alignof expressions
  when seen in another type. */
  uint32_t count;
  struct ast_typeexpr *param;
};

void ast_arraytype_init(struct ast_arraytype *a, struct ast_meta meta,
                        uint32_t count, struct ast_typeexpr param);

struct ast_unknown {
  struct ast_meta meta;
};

void ast_unknown_init(struct ast_unknown *a, struct ast_meta meta);

enum ast_typeexpr_tag {
  AST_TYPEEXPR_NAME,
  AST_TYPEEXPR_APP,
  AST_TYPEEXPR_STRUCTE,
  AST_TYPEEXPR_UNIONE,
  AST_TYPEEXPR_ARRAY,
  /* "Concrete" types don't have this field.  (Maybe they don't have
  generic names either, idk.) */
  AST_TYPEEXPR_UNKNOWN,
};

struct ast_typeexpr {
  enum ast_typeexpr_tag tag;
  union {
    struct ast_ident name;
    struct ast_typeapp app;
    struct ast_structe structe;
    struct ast_unione unione;
    struct ast_arraytype arraytype;
    struct ast_unknown unknown;
  } u;
};

void ast_typeexpr_init_copy(struct ast_typeexpr *a,
                            struct ast_typeexpr *c);
void ast_typeexpr_destroy(struct ast_typeexpr *a);
struct ast_meta *ast_typeexpr_meta(struct ast_typeexpr *a);

struct ast_typeexpr ast_unknown_garbage(void);

struct varnum {
  size_t value;
};

/* TODO: This varnum info is unused (but should be used soon). */
struct ast_var_info {
  int info_valid;
  struct varnum varnum;
  struct ast_typeexpr concrete_type;
};

void ast_var_info_specify(struct ast_var_info *a, struct varnum varnum,
                          struct ast_typeexpr concrete_type);
struct varnum ast_var_info_varnum(struct ast_var_info *a);
struct ast_typeexpr *ast_var_info_type(struct ast_var_info *a);

struct ast_vardecl {
  struct ast_meta meta;
  struct ast_var_info var_info;
  struct ast_ident name;
  struct ast_typeexpr type;
};

void ast_vardecl_init(struct ast_vardecl *a, struct ast_meta meta,
                      struct ast_ident name, struct ast_typeexpr type);
void ast_vardecl_init_copy(struct ast_vardecl *a, struct ast_vardecl *c);
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
  struct ast_vardecl decl_;
  int has_rhs;
  struct ast_expr *rhs;
};

void ast_var_statement_init_with_rhs(struct ast_var_statement *a, struct ast_meta meta,
                                     struct ast_vardecl decl, struct ast_expr rhs);
void ast_var_statement_init_without_rhs(struct ast_var_statement *a, struct ast_meta meta,
                                        struct ast_vardecl decl);

struct ast_typeexpr *ast_var_statement_type(struct ast_var_statement *a);

struct ast_ifthen_statement {
  struct ast_meta meta;
  struct ast_expr *condition;
  struct ast_bracebody body;
};

void ast_ifthen_statement_init(struct ast_ifthen_statement *a,
                               struct ast_meta meta,
                               struct ast_expr condition,
                               struct ast_bracebody body);

struct ast_ifthenelse_statement {
  struct ast_meta meta;
  struct ast_expr *condition;
  struct ast_bracebody thenbody;
  struct ast_bracebody elsebody;
};

void ast_ifthenelse_statement_init(struct ast_ifthenelse_statement *a,
                                   struct ast_meta meta,
                                   struct ast_expr condition,
                                   struct ast_bracebody thenbody,
                                   struct ast_bracebody elsebody);

struct ast_while_statement {
  struct ast_meta meta;
  struct ast_expr *condition;
  struct ast_bracebody body;
};

void ast_while_statement_init(struct ast_while_statement *a,
                              struct ast_meta meta,
                              struct ast_expr condition,
                              struct ast_bracebody body);

struct ast_for_statement {
  struct ast_meta meta;
  /* Can only be an AST_STATEMENT_EXPR or AST_STATEMENT_VAR. */
  int has_initializer;
  struct ast_statement *initializer;
  int has_condition;
  struct ast_expr *condition;
  int has_increment;
  struct ast_expr *increment;
  struct ast_bracebody body;
};

void ast_for_statement_init(struct ast_for_statement *a,
                            struct ast_meta meta,
                            int has_initializer,
                            struct ast_statement *initializer,
                            int has_condition,
                            struct ast_expr *condition,
                            int has_expr,
                            struct ast_expr *expr,
                            struct ast_bracebody body);

struct ast_cased_statement;

struct ast_switch_statement {
  struct ast_meta meta;
  struct ast_expr *swartch;
  struct ast_cased_statement *cased_statements;
  size_t cased_statements_count;
};

void ast_switch_statement_init(struct ast_switch_statement *a,
                               struct ast_meta meta,
                               struct ast_expr swartch,
                               struct ast_cased_statement *cased_statements,
                               size_t cased_statements_count);

struct ast_return_statement {
  struct ast_meta meta;
  /* If no expr, it returns void. */
  int has_expr;
  struct ast_expr *expr;
};

void ast_return_statement_init(struct ast_return_statement *a,
                               struct ast_meta meta,
                               struct ast_expr expr);
void ast_return_statement_init_no_expr(struct ast_return_statement *a,
                                       struct ast_meta meta);

enum ast_statement_tag {
  AST_STATEMENT_EXPR,
  AST_STATEMENT_RETURN,
  AST_STATEMENT_VAR,
  AST_STATEMENT_IFTHEN,
  AST_STATEMENT_IFTHENELSE,
  AST_STATEMENT_WHILE,
  AST_STATEMENT_FOR,
  AST_STATEMENT_SWITCH,
};

struct ast_statement {
  enum ast_statement_tag tag;
  union {
    struct ast_expr *expr;
    struct ast_return_statement return_statement;
    struct ast_var_statement var_statement;
    struct ast_ifthen_statement ifthen_statement;
    struct ast_ifthenelse_statement ifthenelse_statement;
    struct ast_while_statement while_statement;
    struct ast_for_statement for_statement;
    struct ast_switch_statement switch_statement;
  } u;
};

void ast_statement_init_copy(struct ast_statement *a,
                             struct ast_statement *c);
void ast_statement_destroy(struct ast_statement *a);

void ast_statement_alloc_move(struct ast_statement movee,
                              struct ast_statement **out);

struct ast_case_pattern_info {
  int info_valid;
  size_t constructor_number;
};

size_t ast_case_pattern_info_constructor_number(struct ast_case_pattern_info *a);

void ast_case_pattern_info_specify(struct ast_case_pattern_info *a,
                                   size_t constructor_number);

struct ast_case_pattern {
  struct ast_meta meta;
  struct ast_case_pattern_info info;
  struct ast_ident constructor_name;
  struct ast_vardecl decl_;
};

void ast_case_pattern_init(struct ast_case_pattern *a,
                           struct ast_meta meta,
                           struct ast_ident constructor_name,
                           struct ast_vardecl decl);
void ast_case_pattern_init_copy(struct ast_case_pattern *a,
                                struct ast_case_pattern *c);
void ast_case_pattern_destroy(struct ast_case_pattern *a);

/* TODO: Rename. */
struct ast_cased_statement {
  struct ast_meta meta;
  struct ast_case_pattern pattern;
  struct ast_bracebody body;
};

void ast_cased_statement_init(struct ast_cased_statement *a,
                              struct ast_meta meta,
                              struct ast_case_pattern pattern,
                              struct ast_bracebody body);

void ast_cased_statement_init_copy(struct ast_cased_statement *a,
                                   struct ast_cased_statement *c);
void ast_cased_statement_destroy(struct ast_cased_statement *a);

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
  /* Ops below are seen while parsing, but don't show up in
     ast_unop_expr. */
  AST_UNOP_NEGATE,
  AST_UNOP_CONVERT,
  AST_UNOP_LOGICAL_NOT,
  AST_UNOP_BITWISE_NOT,
};

int is_magic_unop(enum ast_unop unop);

struct ast_unop_expr {
  struct ast_meta meta;
  enum ast_unop operator;
  struct ast_expr *rhs;
};

void ast_unop_expr_init(struct ast_unop_expr *a, struct ast_meta meta,
                        enum ast_unop operator, struct ast_expr rhs);

/* See also: binop_precedence in parse.c. */
/* See also: Every use of these values in typecheck.c, including
dependencies on the order of these values. */
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

int is_magic_binop(enum ast_binop binop);

struct ast_binop_expr {
  struct ast_meta meta;
  enum ast_binop operator;
  struct ast_expr *lhs;
  struct ast_expr *rhs;
};

void ast_binop_expr_init(struct ast_binop_expr *a, struct ast_meta meta,
                         enum ast_binop operator, struct ast_expr lhs,
                         struct ast_expr rhs);

struct ast_fieldname {
  struct ast_meta meta;
  int whole_field;
  struct ast_ident ident;
};

void ast_fieldname_init(struct ast_fieldname *a,
                        struct ast_meta meta,
                        struct ast_ident ident);

void ast_fieldname_init_whole(struct ast_fieldname *a,
                              struct ast_meta meta);

void ast_fieldname_init_copy(struct ast_fieldname *a,
                             struct ast_fieldname *c);

struct ast_local_field_access {
  struct ast_meta meta;
  struct ast_expr *lhs;
  struct ast_fieldname fieldname;
};

void ast_local_field_access_init(struct ast_local_field_access *a,
                                 struct ast_meta meta,
                                 struct ast_expr lhs,
                                 struct ast_fieldname fieldname);

struct ast_deref_field_access {
  struct ast_meta meta;
  struct ast_expr *lhs;
  struct ast_fieldname fieldname;
};

void ast_deref_field_access_init(struct ast_deref_field_access *a,
                                 struct ast_meta meta,
                                 struct ast_expr lhs,
                                 struct ast_fieldname fieldname);

struct ast_name_expr_info {
  /* True if typechecking happened and the info means something. */
  int info_valid;
  /* The instantiation this name refers to -- if it refers to a
  global.  NULL if the name refers to a local variable. */
  struct def_instantiation *inst_or_null;
};

void ast_name_expr_info_init(struct ast_name_expr_info *a);
void ast_name_expr_info_destroy(struct ast_name_expr_info *a);
void ast_name_expr_info_mark_inst(struct ast_name_expr_info *a,
                                  struct def_instantiation *inst_or_null);
int ast_name_expr_info_get_inst(struct ast_name_expr_info *a,
                                struct def_instantiation **inst_or_null_out);

struct ast_name_expr {
  struct ast_meta meta;
  struct ast_name_expr_info info;
  struct ast_ident ident;

  int has_params;
  struct ast_typeexpr *params;
  size_t params_count;
};

void ast_name_expr_init(struct ast_name_expr *a,
                        struct ast_ident ident);
void ast_name_expr_init_with_params(struct ast_name_expr *a,
                                    struct ast_meta meta,
                                    struct ast_ident ident,
                                    struct ast_typeexpr *params,
                                    size_t params_count);
void ast_name_expr_init_copy(struct ast_name_expr *a,
                             struct ast_name_expr *c);
void ast_name_expr_destroy(struct ast_name_expr *a);

struct ast_index_expr {
  struct ast_meta meta;
  struct ast_expr *lhs;
  struct ast_expr *rhs;
};

void ast_index_expr_init(struct ast_index_expr *a,
                         struct ast_meta meta,
                         struct ast_expr lhs,
                         struct ast_expr rhs);
void ast_index_expr_init_copy(struct ast_index_expr *a,
                              struct ast_index_expr *c);

void ast_index_expr_destroy(struct ast_index_expr *a);

struct ast_typed_expr {
  struct ast_meta meta;
  struct ast_typeexpr type;
  struct ast_expr *expr;
};

void ast_typed_expr_init(struct ast_typed_expr *a,
                         struct ast_meta meta,
                         struct ast_typeexpr type,
                         struct ast_expr expr);

void ast_typed_expr_init_copy(struct ast_typed_expr *a,
                              struct ast_typed_expr *c);


enum ast_expr_tag {
  AST_EXPR_NAME,
  AST_EXPR_NUMERIC_LITERAL,
  AST_EXPR_CHAR_LITERAL,
  AST_EXPR_STRING_LITERAL,
  AST_EXPR_FUNCALL,
  AST_EXPR_INDEX,
  AST_EXPR_UNOP,
  AST_EXPR_BINOP,
  AST_EXPR_LAMBDA,
  AST_EXPR_LOCAL_FIELD_ACCESS,
  AST_EXPR_DEREF_FIELD_ACCESS,
  AST_EXPR_TYPED,
};

enum ast_typechecked {
  AST_TYPECHECKED_NO,
  AST_TYPECHECKED_YES,
  AST_TYPECHECKED_INCOMPLETE,
};

struct ast_expr_info {
  enum typechecked typechecked;
  int is_lvalue;
  struct ast_typeexpr concrete_type;

  /* Does a temporary exist?  Subsequent fields are ignored if not.
  (All this is only valid if is_typechecked is true.)

  When evaluating, an expr can produce exactly 0 or 1 temporary
  objects.  The expr's return value could be a subfield or array
  element of the temporary object. */
  int temporary_exists;
  /* The type of the temporary. */
  struct ast_typeexpr temporary_type;
  /* Says that the temporary is the whole value this expr returns --
  field access exprs will have this be false, for example. */
  int whole_thing;
  /* The temptag.  Exprs that have the same temporary object have the
  same temptag. */
  size_t temptag;
};

struct ast_expr_info ast_expr_info_default(void);
struct ast_expr_info ast_expr_info_incomplete(void);
struct ast_expr_info ast_expr_info_typechecked_no_temporary(
    int is_lvalue,
    struct ast_typeexpr concrete_type);
struct ast_expr_info ast_expr_info_typechecked_trivial_temporary(
    int is_lvalue,
    struct ast_typeexpr concrete_type);
struct ast_expr_info ast_expr_info_typechecked_no_or_trivial_temporary(
    int is_lvalue,
    struct ast_typeexpr concrete_type);
struct ast_expr_info ast_expr_info_typechecked_temporary(
    int is_lvalue,
    struct ast_typeexpr concrete_type,
    struct ast_typeexpr temporary_type,
    int whole_thing,
    size_t temptag);
struct ast_expr_info ast_expr_info_typechecked_identical(
    struct ast_expr_info *info);

struct ast_expr {
  enum ast_expr_tag tag;
  struct ast_expr_info info;
  union {
    struct ast_name_expr name;
    struct ast_numeric_literal numeric_literal;
    struct ast_char_literal char_literal;
    struct ast_string_literal string_literal;
    struct ast_funcall funcall;
    struct ast_index_expr index_expr;
    struct ast_unop_expr unop_expr;
    struct ast_binop_expr binop_expr;
    struct ast_lambda lambda;
    struct ast_local_field_access local_field_access;
    struct ast_deref_field_access deref_field_access;
    struct ast_typed_expr typed_expr;
  } u;
};

void ast_expr_partial_init(struct ast_expr *a,
                           enum ast_expr_tag tag,
                           struct ast_expr_info expr_info);

void ast_expr_init_copy(struct ast_expr *a, struct ast_expr *c);
void ast_expr_destroy(struct ast_expr *a);

void ast_expr_update(struct ast_expr *a,
                     struct ast_expr_info expr_info);

struct ast_typeexpr *ast_expr_type(struct ast_expr *a);
int ast_expr_incomplete(struct ast_expr *a);
struct ast_meta *ast_expr_ast_meta(struct ast_expr *a);
struct pos ast_expr_pos_end(struct ast_expr *a);
void ast_expr_alloc_move(struct ast_expr movee, struct ast_expr **out);

enum ast_exprcatch_behavior {
  /* The value an ast_expr returns, which is a whole_thing temporary,
  should be constructed in-place. */
  AST_EXPRCATCH_IN_PLACE,
  /* The value an ast_expr returns, which is a non-whole_thing
  temporary, should be copied into its place, and the temporary shall
  be destroyed. */
  AST_EXPRCATCH_COPY_AND_DESTROY,
  /* The value an ast_expr returns, which is not a temporary, should
  be copied into its place. */
  AST_EXPRCATCH_COPY,
};

struct ast_exprcatch {
  int info_valid;
  enum ast_exprcatch_behavior behavior;
};

void ast_exprcatch_init_annotated(struct ast_exprcatch *a,
                                  enum ast_exprcatch_behavior behavior);

struct ast_exprcall {
  struct ast_exprcatch catch;
  struct ast_expr expr;
};

void ast_exprcall_init(struct ast_exprcall *a, struct ast_expr expr);
void ast_exprcall_annotate(struct ast_exprcall *a,
                           struct ast_exprcatch catch);
void ast_exprcall_init_copy(struct ast_exprcall *a, struct ast_exprcall *c);
void ast_exprcall_destroy(struct ast_exprcall *a);

struct ast_generics {
  int has_type_params;  /* 0 or 1 -- meta & params is uninitialized if 0. */
  struct ast_meta meta;
  struct ast_ident *params;
  size_t params_count;
};

void ast_generics_init_no_params(struct ast_generics *a);
void ast_generics_init_has_params(struct ast_generics *a,
                                  struct ast_meta meta,
                                  struct ast_ident *params,
                                  size_t params_count);
void ast_generics_init_copy(struct ast_generics *a,
                            struct ast_generics *c);

void ast_generics_destroy(struct ast_generics *a);

struct ast_def {
  struct ast_meta meta;
  int is_export;
  struct ast_generics generics;
  struct ast_ident name;
  int has_typeexpr;
  struct ast_typeexpr typeexpr;
  struct ast_expr rhs;
};

void ast_def_init(struct ast_def *a, struct ast_meta meta,
                  int is_export, struct ast_generics generics,
                  struct ast_ident name, struct ast_typeexpr typeexpr,
                  struct ast_expr rhs);

void ast_def_init_no_type(struct ast_def *a, struct ast_meta meta,
                          int is_export, struct ast_generics generics,
                          struct ast_ident name, struct ast_expr rhs);

struct ast_typeexpr *ast_def_typeexpr(struct ast_def *a);

struct ast_extern_def {
  struct ast_meta meta;
  struct ast_ident name;
  struct ast_typeexpr type;
};

void ast_extern_def_init(struct ast_extern_def *a, struct ast_meta meta,
                         struct ast_ident name, struct ast_typeexpr type);

struct ast_toplevel;

struct ast_import {
  struct ast_meta meta;
  struct ast_ident name;
};

void ast_import_init(struct ast_import *a, struct ast_meta meta,
                     struct ast_ident name);

struct ast_enumspec {
  struct ast_vardecl *enumfields;
  size_t enumfields_count;
};

void ast_enumspec_init(struct ast_enumspec *a,
                       struct ast_vardecl *enumfields,
                       size_t enumfields_count);
void ast_enumspec_init_copy(struct ast_enumspec *a, struct ast_enumspec *c);
void ast_enumspec_destroy(struct ast_enumspec *a);

enum ast_deftype_rhs_tag {
  AST_DEFTYPE_RHS_TYPE,
  AST_DEFTYPE_RHS_ENUMSPEC,
};

struct ast_deftype_rhs {
  enum ast_deftype_rhs_tag tag;
  union {
    struct ast_typeexpr type;
    struct ast_enumspec enumspec;
  } u;
};

void ast_deftype_rhs_init_copy(struct ast_deftype_rhs *a, struct ast_deftype_rhs *c);
void ast_deftype_rhs_init_type(struct ast_deftype_rhs *a, struct ast_typeexpr type);
void ast_deftype_rhs_init_enumspec(struct ast_deftype_rhs *a, struct ast_enumspec enumspec);
void ast_deftype_rhs_destroy(struct ast_deftype_rhs *a);

enum ast_deftype_disposition {
  AST_DEFTYPE_NOT_CLASS,
  AST_DEFTYPE_CLASS_DEFAULT_COPY_MOVE_DESTROY,
  AST_DEFTYPE_CLASS_DEFAULT_MOVE,
  AST_DEFTYPE_CLASS_NO_DEFAULTS,
};

struct ast_deftype {
  struct ast_meta meta;
  enum ast_deftype_disposition disposition;
  struct ast_generics generics;
  struct ast_ident name;
  struct ast_deftype_rhs rhs;
};

void ast_deftype_init(struct ast_deftype *a, struct ast_meta meta,
                      enum ast_deftype_disposition disposition,
                      struct ast_generics generics,
                      struct ast_ident name, struct ast_typeexpr type);
void ast_deftype_init_enum(struct ast_deftype *a, struct ast_meta meta,
                           struct ast_generics generics,
                           struct ast_ident name,
                           struct ast_vardecl *enumfields,
                           size_t enumfields_count);

struct ast_access {
  struct ast_meta meta;
  struct ast_ident name;
  struct generics_arity arity;
  struct ast_toplevel *toplevels;
  size_t toplevels_count;
};

void ast_access_init(struct ast_access *a, struct ast_meta meta,
                     struct ast_ident name, struct generics_arity arity,
                     struct ast_toplevel *toplevels, size_t toplevels_count);

enum ast_toplevel_tag {
  AST_TOPLEVEL_IMPORT,
  AST_TOPLEVEL_DEF,
  AST_TOPLEVEL_EXTERN_DEF,
  AST_TOPLEVEL_DEFTYPE,
  AST_TOPLEVEL_ACCESS,
};

struct ast_toplevel {
  enum ast_toplevel_tag tag;
  union {
    struct ast_import import;
    struct ast_def def;
    struct ast_extern_def extern_def;
    struct ast_deftype deftype;
    struct ast_access access;
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
