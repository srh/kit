#include "typecheck.h"

#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "ast.h"
#include "checkstate.h"
#include "identmap.h"
#include "io.h"
#include "parse.h"
#include "print.h"
#include "sizeattr.h"
#include "slice.h"
#include "table.h"
#include "util.h"

#define CHECK_DBG(...) do { } while (0)

struct exprscope;
int lookup_global_maybe_typecheck(
    struct checkstate *cs,
    struct exprscope *also_maybe_typecheck,
    struct ast_name_expr *name,
    struct ast_typeexpr *partial_type,
    enum report_mode report_mode,
    enum match_result *match_result_out,
    struct ast_typeexpr *out,
    int *is_lvalue_out,
    struct def_instantiation **inst_out);
void wrap_in_ptr(struct common_idents *cm,
                 struct ast_typeexpr *target,
                 struct ast_typeexpr *ptr_out);

const int MAX_TEMPLATE_INSTANTIATION_RECURSION_DEPTH = 50;

struct ast_ident make_ast_ident(ident_value ident) {
  struct ast_ident ret;
  ast_ident_init(&ret, ast_meta_make_garbage(), ident);
  return ret;
}

void intern_primitive_type(struct checkstate *cs,
                           const char *name,
                           int *flatly_held,
                           size_t flatly_held_count,
                           uint32_t primitive_sizeof,
                           uint32_t primitive_alignof) {
  ident_value ident = identmap_intern_c_str(cs->im, name);
  int res = name_table_add_primitive_type(cs->im, &cs->nt, ident,
                                          flatly_held, flatly_held_count,
                                          primitive_sizeof,
                                          primitive_alignof);
  CHECK(res);
}

const char *binop_name(enum ast_binop binop) {
  CHECK(!is_magic_binop(binop));

  static const char *const binop_names[] = {
    [AST_BINOP_ASSIGN] = NULL,
    [AST_BINOP_ADD] = "+",
    [AST_BINOP_SUB] = "-",
    [AST_BINOP_MUL] = "*",
    [AST_BINOP_DIV] = "/",
    [AST_BINOP_MOD] = "%",
    [AST_BINOP_LT] = "<",
    [AST_BINOP_LE] = "<=",
    [AST_BINOP_GT] = ">",
    [AST_BINOP_GE] = ">=",
    [AST_BINOP_EQ] = "==",
    [AST_BINOP_NE] = "!=",
    [AST_BINOP_BIT_XOR] = "^",
    [AST_BINOP_BIT_OR] = "|",
    [AST_BINOP_BIT_AND] = "&",
    [AST_BINOP_BIT_LEFTSHIFT] = "<<",
    [AST_BINOP_BIT_RIGHTSHIFT] = ">>",
    [AST_BINOP_LOGICAL_OR] = NULL,
    [AST_BINOP_LOGICAL_AND] = NULL,
  };

  return binop_names[binop];
}

static const enum primitive_op_tag binop_bool_primitive_ops[] = {
  [AST_BINOP_ASSIGN] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_ADD] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_SUB] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_MUL] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_DIV] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_MOD] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_LT] = PRIMITIVE_OP_LT_BOOL,
  [AST_BINOP_LE] = PRIMITIVE_OP_LE_BOOL,
  [AST_BINOP_GT] = PRIMITIVE_OP_GT_BOOL,
  [AST_BINOP_GE] = PRIMITIVE_OP_GE_BOOL,
  [AST_BINOP_EQ] = PRIMITIVE_OP_EQ_BOOL,
  [AST_BINOP_NE] = PRIMITIVE_OP_NE_BOOL,
  [AST_BINOP_BIT_XOR] = PRIMITIVE_OP_BIT_XOR_BOOL,
  [AST_BINOP_BIT_OR] = PRIMITIVE_OP_BIT_OR_BOOL,
  [AST_BINOP_BIT_AND] = PRIMITIVE_OP_BIT_AND_BOOL,
  [AST_BINOP_BIT_LEFTSHIFT] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_BIT_RIGHTSHIFT] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_LOGICAL_OR] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_LOGICAL_AND] = PRIMITIVE_OP_INVALID,
};

static const enum primitive_op_tag binop_u8_primitive_ops[] = {
  [AST_BINOP_ASSIGN] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_ADD] = PRIMITIVE_OP_ADD_U8,
  [AST_BINOP_SUB] = PRIMITIVE_OP_SUB_U8,
  [AST_BINOP_MUL] = PRIMITIVE_OP_MUL_U8,
  [AST_BINOP_DIV] = PRIMITIVE_OP_DIV_U8,
  [AST_BINOP_MOD] = PRIMITIVE_OP_MOD_U8,
  [AST_BINOP_LT] = PRIMITIVE_OP_LT_U8,
  [AST_BINOP_LE] = PRIMITIVE_OP_LE_U8,
  [AST_BINOP_GT] = PRIMITIVE_OP_GT_U8,
  [AST_BINOP_GE] = PRIMITIVE_OP_GE_U8,
  [AST_BINOP_EQ] = PRIMITIVE_OP_EQ_U8,
  [AST_BINOP_NE] = PRIMITIVE_OP_NE_U8,
  [AST_BINOP_BIT_XOR] = PRIMITIVE_OP_BIT_XOR_U8,
  [AST_BINOP_BIT_OR] = PRIMITIVE_OP_BIT_OR_U8,
  [AST_BINOP_BIT_AND] = PRIMITIVE_OP_BIT_AND_U8,
  [AST_BINOP_BIT_LEFTSHIFT] = PRIMITIVE_OP_BIT_LEFTSHIFT_U8,
  [AST_BINOP_BIT_RIGHTSHIFT] = PRIMITIVE_OP_BIT_RIGHTSHIFT_U8,
  [AST_BINOP_LOGICAL_OR] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_LOGICAL_AND] = PRIMITIVE_OP_INVALID,
};

static const enum primitive_op_tag binop_i8_primitive_ops[] = {
  [AST_BINOP_ASSIGN] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_ADD] = PRIMITIVE_OP_ADD_I8,
  [AST_BINOP_SUB] = PRIMITIVE_OP_SUB_I8,
  [AST_BINOP_MUL] = PRIMITIVE_OP_MUL_I8,
  [AST_BINOP_DIV] = PRIMITIVE_OP_DIV_I8,
  [AST_BINOP_MOD] = PRIMITIVE_OP_MOD_I8,
  [AST_BINOP_LT] = PRIMITIVE_OP_LT_I8,
  [AST_BINOP_LE] = PRIMITIVE_OP_LE_I8,
  [AST_BINOP_GT] = PRIMITIVE_OP_GT_I8,
  [AST_BINOP_GE] = PRIMITIVE_OP_GE_I8,
  [AST_BINOP_EQ] = PRIMITIVE_OP_EQ_I8,
  [AST_BINOP_NE] = PRIMITIVE_OP_NE_I8,
  [AST_BINOP_BIT_XOR] = PRIMITIVE_OP_BIT_XOR_I8,
  [AST_BINOP_BIT_OR] = PRIMITIVE_OP_BIT_OR_I8,
  [AST_BINOP_BIT_AND] = PRIMITIVE_OP_BIT_AND_I8,
  [AST_BINOP_BIT_LEFTSHIFT] = PRIMITIVE_OP_BIT_LEFTSHIFT_I8,
  [AST_BINOP_BIT_RIGHTSHIFT] = PRIMITIVE_OP_BIT_RIGHTSHIFT_I8,
  [AST_BINOP_LOGICAL_OR] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_LOGICAL_AND] = PRIMITIVE_OP_INVALID,
};

static const enum primitive_op_tag binop_u16_primitive_ops[] = {
  [AST_BINOP_ASSIGN] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_ADD] = PRIMITIVE_OP_ADD_U16,
  [AST_BINOP_SUB] = PRIMITIVE_OP_SUB_U16,
  [AST_BINOP_MUL] = PRIMITIVE_OP_MUL_U16,
  [AST_BINOP_DIV] = PRIMITIVE_OP_DIV_U16,
  [AST_BINOP_MOD] = PRIMITIVE_OP_MOD_U16,
  [AST_BINOP_LT] = PRIMITIVE_OP_LT_U16,
  [AST_BINOP_LE] = PRIMITIVE_OP_LE_U16,
  [AST_BINOP_GT] = PRIMITIVE_OP_GT_U16,
  [AST_BINOP_GE] = PRIMITIVE_OP_GE_U16,
  [AST_BINOP_EQ] = PRIMITIVE_OP_EQ_U16,
  [AST_BINOP_NE] = PRIMITIVE_OP_NE_U16,
  [AST_BINOP_BIT_XOR] = PRIMITIVE_OP_BIT_XOR_U16,
  [AST_BINOP_BIT_OR] = PRIMITIVE_OP_BIT_OR_U16,
  [AST_BINOP_BIT_AND] = PRIMITIVE_OP_BIT_AND_U16,
  [AST_BINOP_BIT_LEFTSHIFT] = PRIMITIVE_OP_BIT_LEFTSHIFT_U16,
  [AST_BINOP_BIT_RIGHTSHIFT] = PRIMITIVE_OP_BIT_RIGHTSHIFT_U16,
  [AST_BINOP_LOGICAL_OR] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_LOGICAL_AND] = PRIMITIVE_OP_INVALID,
};

static const enum primitive_op_tag binop_i16_primitive_ops[] = {
  [AST_BINOP_ASSIGN] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_ADD] = PRIMITIVE_OP_ADD_I16,
  [AST_BINOP_SUB] = PRIMITIVE_OP_SUB_I16,
  [AST_BINOP_MUL] = PRIMITIVE_OP_MUL_I16,
  [AST_BINOP_DIV] = PRIMITIVE_OP_DIV_I16,
  [AST_BINOP_MOD] = PRIMITIVE_OP_MOD_I16,
  [AST_BINOP_LT] = PRIMITIVE_OP_LT_I16,
  [AST_BINOP_LE] = PRIMITIVE_OP_LE_I16,
  [AST_BINOP_GT] = PRIMITIVE_OP_GT_I16,
  [AST_BINOP_GE] = PRIMITIVE_OP_GE_I16,
  [AST_BINOP_EQ] = PRIMITIVE_OP_EQ_I16,
  [AST_BINOP_NE] = PRIMITIVE_OP_NE_I16,
  [AST_BINOP_BIT_XOR] = PRIMITIVE_OP_BIT_XOR_I16,
  [AST_BINOP_BIT_OR] = PRIMITIVE_OP_BIT_OR_I16,
  [AST_BINOP_BIT_AND] = PRIMITIVE_OP_BIT_AND_I16,
  [AST_BINOP_BIT_LEFTSHIFT] = PRIMITIVE_OP_BIT_LEFTSHIFT_I16,
  [AST_BINOP_BIT_RIGHTSHIFT] = PRIMITIVE_OP_BIT_RIGHTSHIFT_I16,
  [AST_BINOP_LOGICAL_OR] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_LOGICAL_AND] = PRIMITIVE_OP_INVALID,
};

static const enum primitive_op_tag binop_u32_primitive_ops[] = {
  [AST_BINOP_ASSIGN] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_ADD] = PRIMITIVE_OP_ADD_U32,
  [AST_BINOP_SUB] = PRIMITIVE_OP_SUB_U32,
  [AST_BINOP_MUL] = PRIMITIVE_OP_MUL_U32,
  [AST_BINOP_DIV] = PRIMITIVE_OP_DIV_U32,
  [AST_BINOP_MOD] = PRIMITIVE_OP_MOD_U32,
  [AST_BINOP_LT] = PRIMITIVE_OP_LT_U32,
  [AST_BINOP_LE] = PRIMITIVE_OP_LE_U32,
  [AST_BINOP_GT] = PRIMITIVE_OP_GT_U32,
  [AST_BINOP_GE] = PRIMITIVE_OP_GE_U32,
  [AST_BINOP_EQ] = PRIMITIVE_OP_EQ_U32,
  [AST_BINOP_NE] = PRIMITIVE_OP_NE_U32,
  [AST_BINOP_BIT_XOR] = PRIMITIVE_OP_BIT_XOR_U32,
  [AST_BINOP_BIT_OR] = PRIMITIVE_OP_BIT_OR_U32,
  [AST_BINOP_BIT_AND] = PRIMITIVE_OP_BIT_AND_U32,
  [AST_BINOP_BIT_LEFTSHIFT] = PRIMITIVE_OP_BIT_LEFTSHIFT_U32,
  [AST_BINOP_BIT_RIGHTSHIFT] = PRIMITIVE_OP_BIT_RIGHTSHIFT_U32,
  [AST_BINOP_LOGICAL_OR] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_LOGICAL_AND] = PRIMITIVE_OP_INVALID,
};

static const enum primitive_op_tag binop_i32_primitive_ops[] = {
  [AST_BINOP_ASSIGN] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_ADD] = PRIMITIVE_OP_ADD_I32,
  [AST_BINOP_SUB] = PRIMITIVE_OP_SUB_I32,
  [AST_BINOP_MUL] = PRIMITIVE_OP_MUL_I32,
  [AST_BINOP_DIV] = PRIMITIVE_OP_DIV_I32,
  [AST_BINOP_MOD] = PRIMITIVE_OP_MOD_I32,
  [AST_BINOP_LT] = PRIMITIVE_OP_LT_I32,
  [AST_BINOP_LE] = PRIMITIVE_OP_LE_I32,
  [AST_BINOP_GT] = PRIMITIVE_OP_GT_I32,
  [AST_BINOP_GE] = PRIMITIVE_OP_GE_I32,
  [AST_BINOP_EQ] = PRIMITIVE_OP_EQ_I32,
  [AST_BINOP_NE] = PRIMITIVE_OP_NE_I32,
  [AST_BINOP_BIT_XOR] = PRIMITIVE_OP_BIT_XOR_I32,
  [AST_BINOP_BIT_OR] = PRIMITIVE_OP_BIT_OR_I32,
  [AST_BINOP_BIT_AND] = PRIMITIVE_OP_BIT_AND_I32,
  [AST_BINOP_BIT_LEFTSHIFT] = PRIMITIVE_OP_BIT_LEFTSHIFT_I32,
  [AST_BINOP_BIT_RIGHTSHIFT] = PRIMITIVE_OP_BIT_RIGHTSHIFT_I32,
  [AST_BINOP_LOGICAL_OR] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_LOGICAL_AND] = PRIMITIVE_OP_INVALID,
};

static const enum primitive_op_tag binop_size_primitive_ops[] = {
  [AST_BINOP_ASSIGN] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_ADD] = PRIMITIVE_OP_ADD_SIZE,
  [AST_BINOP_SUB] = PRIMITIVE_OP_SUB_SIZE,
  [AST_BINOP_MUL] = PRIMITIVE_OP_MUL_SIZE,
  [AST_BINOP_DIV] = PRIMITIVE_OP_DIV_SIZE,
  [AST_BINOP_MOD] = PRIMITIVE_OP_MOD_SIZE,
  [AST_BINOP_LT] = PRIMITIVE_OP_LT_SIZE,
  [AST_BINOP_LE] = PRIMITIVE_OP_LE_SIZE,
  [AST_BINOP_GT] = PRIMITIVE_OP_GT_SIZE,
  [AST_BINOP_GE] = PRIMITIVE_OP_GE_SIZE,
  [AST_BINOP_EQ] = PRIMITIVE_OP_EQ_SIZE,
  [AST_BINOP_NE] = PRIMITIVE_OP_NE_SIZE,
  [AST_BINOP_BIT_XOR] = PRIMITIVE_OP_BIT_XOR_SIZE,
  [AST_BINOP_BIT_OR] = PRIMITIVE_OP_BIT_OR_SIZE,
  [AST_BINOP_BIT_AND] = PRIMITIVE_OP_BIT_AND_SIZE,
  [AST_BINOP_BIT_LEFTSHIFT] = PRIMITIVE_OP_BIT_LEFTSHIFT_SIZE,
  [AST_BINOP_BIT_RIGHTSHIFT] = PRIMITIVE_OP_BIT_RIGHTSHIFT_SIZE,
  [AST_BINOP_LOGICAL_OR] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_LOGICAL_AND] = PRIMITIVE_OP_INVALID,
};

static const enum primitive_op_tag binop_osize_primitive_ops[] = {
  [AST_BINOP_ASSIGN] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_ADD] = PRIMITIVE_OP_ADD_OSIZE,
  [AST_BINOP_SUB] = PRIMITIVE_OP_SUB_OSIZE,
  [AST_BINOP_MUL] = PRIMITIVE_OP_MUL_OSIZE,
  [AST_BINOP_DIV] = PRIMITIVE_OP_DIV_OSIZE,
  [AST_BINOP_MOD] = PRIMITIVE_OP_MOD_OSIZE,
  [AST_BINOP_LT] = PRIMITIVE_OP_LT_OSIZE,
  [AST_BINOP_LE] = PRIMITIVE_OP_LE_OSIZE,
  [AST_BINOP_GT] = PRIMITIVE_OP_GT_OSIZE,
  [AST_BINOP_GE] = PRIMITIVE_OP_GE_OSIZE,
  [AST_BINOP_EQ] = PRIMITIVE_OP_EQ_OSIZE,
  [AST_BINOP_NE] = PRIMITIVE_OP_NE_OSIZE,
  [AST_BINOP_BIT_XOR] = PRIMITIVE_OP_BIT_XOR_OSIZE,
  [AST_BINOP_BIT_OR] = PRIMITIVE_OP_BIT_OR_OSIZE,
  [AST_BINOP_BIT_AND] = PRIMITIVE_OP_BIT_AND_OSIZE,
  [AST_BINOP_BIT_LEFTSHIFT] = PRIMITIVE_OP_BIT_LEFTSHIFT_OSIZE,
  [AST_BINOP_BIT_RIGHTSHIFT] = PRIMITIVE_OP_BIT_RIGHTSHIFT_OSIZE,
  [AST_BINOP_LOGICAL_OR] = PRIMITIVE_OP_INVALID,
  [AST_BINOP_LOGICAL_AND] = PRIMITIVE_OP_INVALID,
};




void intern_binop(struct checkstate *cs,
                  enum ast_binop binop,
                  const enum primitive_op_tag *primop_array,
                  struct ast_generics *generics,
                  struct ast_typeexpr *type) {
  name_table_add_primitive_def(
      cs->im,
      &cs->nt,
      identmap_intern_c_str(cs->im, binop_name(binop)),
      make_primop(primop_array[binop]),
      generics,
      type);
}

#define CONVERT_FUNCTION_NAME "~"
#define UPCONVERT_FUNCTION_NAME "+"

int typeexpr_is_func_type(struct identmap *im, struct ast_typeexpr *x) {
  return x->tag == AST_TYPEEXPR_APP
    && x->u.app.name.value == identmap_intern(im, FUNC_TYPE_NAME,
                                              strlen(FUNC_TYPE_NAME));
}

void checkstate_import_primitive_types(struct checkstate *cs) {
  intern_primitive_type(cs, VOID_TYPE_NAME, NULL, 0, 0, 1);
  intern_primitive_type(cs, BOOL_TYPE_NAME, NULL, 0, 1, 1);
  intern_primitive_type(cs, U8_TYPE_NAME, NULL, 0, 1, 1);
  intern_primitive_type(cs, I8_TYPE_NAME, NULL, 0, 1, 1);
  intern_primitive_type(cs, U16_TYPE_NAME, NULL, 0, 2, 2);
  intern_primitive_type(cs, I16_TYPE_NAME, NULL, 0, 2, 2);
  intern_primitive_type(cs, U32_TYPE_NAME, NULL, 0, 4, 4);
  intern_primitive_type(cs, I32_TYPE_NAME, NULL, 0, 4, 4);
  uint32_t psize = ptr_size(cs->arch);
  intern_primitive_type(cs, SIZE_TYPE_NAME, NULL, 0, psize, psize);
  intern_primitive_type(cs, OSIZE_TYPE_NAME, NULL, 0, psize, psize);

  int not_flatly_held[20] = { 0 };
  intern_primitive_type(cs, PTR_TYPE_NAME, not_flatly_held, 1, psize, psize);
  for (size_t i = 1; i < 21; i++) {
    intern_primitive_type(cs, FUNC_TYPE_NAME, not_flatly_held, i, psize, psize);
  }
}

void init_func_type(struct ast_typeexpr *a, struct common_idents *cm,
                    ident_value *args, size_t args_count) {
  a->tag = AST_TYPEEXPR_APP;
  struct ast_typeexpr_array params = ast_typeexpr_array_malloc(args_count);
  for (size_t i = 0; i < args_count; i++) {
    params.ptr[i].tag = AST_TYPEEXPR_NAME;
    params.ptr[i].u.name = make_ast_ident(args[i]);
  }
  ast_typeapp_init(&a->u.app, ast_meta_make_garbage(),
                   make_ast_ident(cm->func), params);
}

void init_name_type(struct ast_typeexpr *a, ident_value name) {
  a->tag = AST_TYPEEXPR_NAME;
  a->u.name = make_ast_ident(name);
}

void expose_func_type_parts(struct common_idents *cm,
                            struct ast_typeexpr *func,
                            struct ast_typeexpr **args_out,
                            size_t *args_count_out,
                            struct ast_typeexpr **return_type_out) {
  CHECK(func->tag == AST_TYPEEXPR_APP);
  CHECK(func->u.app.name.value == cm->func);
  *args_out = func->u.app.params.ptr;
  size_t args_count = size_sub(func->u.app.params.count, 1);
  *args_count_out = args_count;
  *return_type_out = &func->u.app.params.ptr[args_count];
}

struct ast_typeexpr *expose_func_return_type(struct common_idents *cm,
                                             struct ast_typeexpr *func,
                                             size_t expected_params_count) {
  struct ast_typeexpr *args;
  size_t args_count;
  struct ast_typeexpr *return_type;
  expose_func_type_parts(cm, func, &args, &args_count, &return_type);
  CHECK(size_add(args_count, 1) == expected_params_count);
  return return_type;
}

void copy_func_return_type(struct common_idents *cm,
                           struct ast_typeexpr *func,
                           size_t expected_params_count,
                           struct ast_typeexpr *out) {
  ast_typeexpr_init_copy(out, expose_func_return_type(cm, func, expected_params_count));
}

void init_binop_func_type(struct ast_typeexpr *a, struct checkstate *cs,
                          const char *type_name) {
  ident_value name = identmap_intern_c_str(cs->im, type_name);
  ident_value names[3];
  names[0] = names[1] = names[2] = name;
  init_func_type(a, &cs->cm, names, 3);
}

void init_binop_compare_type(struct ast_typeexpr *a, struct checkstate *cs,
                             const char *type_name) {
  ident_value name = identmap_intern_c_str(cs->im, type_name);
  ident_value names[3];
  names[0] = names[1] = name;
  names[2] = cs->cm.boole;
  init_func_type(a, &cs->cm, names, 3);
}

void import_integer_binops(struct checkstate *cs,
                           const enum primitive_op_tag *primop_array,
                           const char *type_name) {
  struct ast_generics generics;
  ast_generics_init_no_params(&generics);
  struct ast_typeexpr binop_type;
  init_binop_func_type(&binop_type, cs, type_name);
  for (enum ast_binop op = AST_BINOP_ADD; op < AST_BINOP_LT; op++) {
    intern_binop(cs, op, primop_array, &generics, &binop_type);
  }
  for (enum ast_binop op = AST_BINOP_BIT_XOR;
       op < AST_BINOP_LOGICAL_OR;
       op++) {
    intern_binop(cs, op, primop_array, &generics, &binop_type);
  }
  ast_typeexpr_destroy(&binop_type);
  init_binop_compare_type(&binop_type, cs, type_name);
  for (enum ast_binop op = AST_BINOP_LT; op < AST_BINOP_BIT_XOR; op++) {
    intern_binop(cs, op, primop_array, &generics, &binop_type);
  }
  ast_typeexpr_destroy(&binop_type);
  ast_generics_destroy(&generics);
}

void import_bool_binops(struct checkstate *cs) {
  struct ast_generics generics;
  ast_generics_init_no_params(&generics);
  struct ast_typeexpr binop_type;
  init_binop_func_type(&binop_type, cs, BOOL_TYPE_NAME);
  for (enum ast_binop op = AST_BINOP_BIT_XOR;
       op < AST_BINOP_BIT_LEFTSHIFT;
       op++) {
    intern_binop(cs, op, binop_bool_primitive_ops, &generics, &binop_type);
  }
  ast_typeexpr_destroy(&binop_type);
  init_binop_compare_type(&binop_type, cs, BOOL_TYPE_NAME);
  for (enum ast_binop op = AST_BINOP_LT; op < AST_BINOP_BIT_XOR; op++) {
    intern_binop(cs, op, binop_bool_primitive_ops, &generics, &binop_type);
  }
  ast_typeexpr_destroy(&binop_type);
  ast_generics_destroy(&generics);
}

struct fake_limit { int low, high; };

void import_integer_conversions(struct checkstate *cs) {
  const size_t num_types = 8;
  ident_value types[8];
  types[0] = cs->cm.u8_type_name;
  types[1] = cs->cm.i8_type_name;
  types[2] = cs->cm.u16_type_name;
  types[3] = cs->cm.i16_type_name;
  types[4] = cs->cm.u32_type_name;
  types[5] = cs->cm.i32_type_name;
  types[6] = cs->cm.size_type_name;
  types[7] = cs->cm.osize_type_name;

  struct fake_limit fake_limits[8] = {
    {0, 4},
    {-3, 3},
    {0, 8},
    {-7, 7},
    {0, 12},
    {-11, 11},
    {0, 12},
    {0, 12},
  };

  ident_value convert = identmap_intern_c_str(cs->im, CONVERT_FUNCTION_NAME);
  ident_value upconvert = identmap_intern_c_str(cs->im, UPCONVERT_FUNCTION_NAME);

  struct ast_generics generics;
  ast_generics_init_no_params(&generics);

  enum primitive_op_tag conversions[8][8] = {
    {
      PRIMITIVE_OP_CONVERT_U8_TO_U8,
      PRIMITIVE_OP_CONVERT_U8_TO_I8,
      PRIMITIVE_OP_CONVERT_U8_TO_U16,
      PRIMITIVE_OP_CONVERT_U8_TO_I16,
      PRIMITIVE_OP_CONVERT_U8_TO_U32,
      PRIMITIVE_OP_CONVERT_U8_TO_I32,
      PRIMITIVE_OP_CONVERT_U8_TO_SIZE,
      PRIMITIVE_OP_CONVERT_U8_TO_OSIZE,
    }, {
      PRIMITIVE_OP_CONVERT_I8_TO_U8,
      PRIMITIVE_OP_CONVERT_I8_TO_I8,
      PRIMITIVE_OP_CONVERT_I8_TO_U16,
      PRIMITIVE_OP_CONVERT_I8_TO_I16,
      PRIMITIVE_OP_CONVERT_I8_TO_U32,
      PRIMITIVE_OP_CONVERT_I8_TO_I32,
      PRIMITIVE_OP_CONVERT_I8_TO_SIZE,
      PRIMITIVE_OP_CONVERT_I8_TO_OSIZE,
    }, {
      PRIMITIVE_OP_CONVERT_U16_TO_U8,
      PRIMITIVE_OP_CONVERT_U16_TO_I8,
      PRIMITIVE_OP_CONVERT_U16_TO_U16,
      PRIMITIVE_OP_CONVERT_U16_TO_I16,
      PRIMITIVE_OP_CONVERT_U16_TO_U32,
      PRIMITIVE_OP_CONVERT_U16_TO_I32,
      PRIMITIVE_OP_CONVERT_U16_TO_SIZE,
      PRIMITIVE_OP_CONVERT_U16_TO_OSIZE,
    }, {
      PRIMITIVE_OP_CONVERT_I16_TO_U8,
      PRIMITIVE_OP_CONVERT_I16_TO_I8,
      PRIMITIVE_OP_CONVERT_I16_TO_U16,
      PRIMITIVE_OP_CONVERT_I16_TO_I16,
      PRIMITIVE_OP_CONVERT_I16_TO_U32,
      PRIMITIVE_OP_CONVERT_I16_TO_I32,
      PRIMITIVE_OP_CONVERT_I16_TO_SIZE,
      PRIMITIVE_OP_CONVERT_I16_TO_OSIZE,
    }, {
      PRIMITIVE_OP_CONVERT_U32_TO_U8,
      PRIMITIVE_OP_CONVERT_U32_TO_I8,
      PRIMITIVE_OP_CONVERT_U32_TO_U16,
      PRIMITIVE_OP_CONVERT_U32_TO_I16,
      PRIMITIVE_OP_CONVERT_U32_TO_U32,
      PRIMITIVE_OP_CONVERT_U32_TO_I32,
      PRIMITIVE_OP_CONVERT_U32_TO_SIZE,
      PRIMITIVE_OP_CONVERT_U32_TO_OSIZE,
    }, {
      PRIMITIVE_OP_CONVERT_I32_TO_U8,
      PRIMITIVE_OP_CONVERT_I32_TO_I8,
      PRIMITIVE_OP_CONVERT_I32_TO_U16,
      PRIMITIVE_OP_CONVERT_I32_TO_I16,
      PRIMITIVE_OP_CONVERT_I32_TO_U32,
      PRIMITIVE_OP_CONVERT_I32_TO_I32,
      PRIMITIVE_OP_CONVERT_I32_TO_SIZE,
      PRIMITIVE_OP_CONVERT_I32_TO_OSIZE,
    }, {
      PRIMITIVE_OP_CONVERT_SIZE_TO_U8,
      PRIMITIVE_OP_CONVERT_SIZE_TO_I8,
      PRIMITIVE_OP_CONVERT_SIZE_TO_U16,
      PRIMITIVE_OP_CONVERT_SIZE_TO_I16,
      PRIMITIVE_OP_CONVERT_SIZE_TO_U32,
      PRIMITIVE_OP_CONVERT_SIZE_TO_I32,
      PRIMITIVE_OP_CONVERT_SIZE_TO_SIZE,
      PRIMITIVE_OP_CONVERT_SIZE_TO_OSIZE,
    }, {
      PRIMITIVE_OP_CONVERT_OSIZE_TO_U8,
      PRIMITIVE_OP_CONVERT_OSIZE_TO_I8,
      PRIMITIVE_OP_CONVERT_OSIZE_TO_U16,
      PRIMITIVE_OP_CONVERT_OSIZE_TO_I16,
      PRIMITIVE_OP_CONVERT_OSIZE_TO_U32,
      PRIMITIVE_OP_CONVERT_OSIZE_TO_I32,
      PRIMITIVE_OP_CONVERT_OSIZE_TO_SIZE,
      PRIMITIVE_OP_CONVERT_OSIZE_TO_OSIZE,
    },
  };

  for (size_t i = 0; i < num_types; i++) {
    for (size_t j = 0; j < num_types; j++) {
      struct ast_typeexpr func_type;
      ident_value names[2];
      names[0] = types[i];
      names[1] = types[j];
      init_func_type(&func_type, &cs->cm, names, 2);
      name_table_add_primitive_def(cs->im,
                                   &cs->nt,
                                   convert,
                                   make_primop(conversions[i][j]),
                                   &generics,
                                   &func_type);
      if (fake_limits[i].low >= fake_limits[j].low && fake_limits[i].high <= fake_limits[j].high) {
        name_table_add_primitive_def(cs->im,
                                     &cs->nt,
                                     upconvert,
                                     make_primop(conversions[i][j]),
                                     &generics,
                                     &func_type);
      }
      ast_typeexpr_destroy(&func_type);
    }
  }
}

void import_unop(struct checkstate *cs,
                 enum primitive_op_tag primitive_op,
                 const char *op_name,
                 const char *type_name) {
  struct ast_generics generics;
  ast_generics_init_no_params(&generics);

  struct ast_typeexpr type;
  ident_value args[2];
  args[0] = args[1] = identmap_intern_c_str(cs->im, type_name);
  init_func_type(&type, &cs->cm, args, 2);
  name_table_add_primitive_def(
      cs->im,
      &cs->nt,
      identmap_intern_c_str(cs->im, op_name),
      make_primop(primitive_op),
      &generics,
      &type);
  ast_typeexpr_destroy(&type);

  ast_generics_destroy(&generics);
}

struct ast_generics one_param_generics(struct checkstate *cs, const char *name) {
  struct ast_ident_array param = ast_ident_array_malloc(1);
  param.ptr[0] = make_ast_ident(identmap_intern_c_str(cs->im, name));

  struct ast_generics generics;
  ast_generics_init_has_params(&generics, ast_meta_make_garbage(), param);
  return generics;
}

void import_sizeof_alignof(struct checkstate *cs) {
  struct ast_generics generics = one_param_generics(cs, "T");
  struct ast_typeexpr type;
  init_name_type(&type, cs->cm.size_type_name);

  name_table_add_primitive_def(
      cs->im,
      &cs->nt,
      identmap_intern_c_str(cs->im, "sizeof"),
      make_primop(PRIMITIVE_OP_SIZEOF),
      &generics,
      &type);

  name_table_add_primitive_def(
      cs->im,
      &cs->nt,
      identmap_intern_c_str(cs->im, "alignof"),
      make_primop(PRIMITIVE_OP_ALIGNOF),
      &generics,
      &type);

  ast_typeexpr_destroy(&type);
  ast_generics_destroy(&generics);
}

void make_ptr_func_type(struct checkstate *cs, struct ast_typeexpr *target, size_t count,
                        const char *return_type_name, struct ast_typeexpr *out) {
  size_t params_count = size_add(count, 1);
  struct ast_typeexpr_array params = ast_typeexpr_array_malloc(params_count);
  for (size_t i = 0; i < count; i++) {
    wrap_in_ptr(&cs->cm, target, &params.ptr[i]);
  }

  init_name_type(&params.ptr[count], identmap_intern_c_str(cs->im, return_type_name));

  out->tag = AST_TYPEEXPR_APP;
  ast_typeapp_init(&out->u.app, ast_meta_make_garbage(),
                   make_ast_ident(cs->cm.func), params);
}

void import_ptr_binops(struct checkstate *cs) {
  struct ast_generics generics = one_param_generics(cs, "T");
  struct ast_typeexpr target;
  init_name_type(&target, identmap_intern_c_str(cs->im, "T"));
  struct ast_typeexpr type;
  make_ptr_func_type(cs, &target, 2, BOOL_TYPE_NAME, &type);
  name_table_add_primitive_def(cs->im, &cs->nt,
                               identmap_intern_c_str(cs->im, binop_name(AST_BINOP_EQ)),
                               make_primop(PRIMITIVE_OP_EQ_PTR),
                               &generics,
                               &type);
  name_table_add_primitive_def(cs->im, &cs->nt,
                               identmap_intern_c_str(cs->im, binop_name(AST_BINOP_NE)),
                               make_primop(PRIMITIVE_OP_NE_PTR),
                               &generics,
                               &type);
  ast_typeexpr_destroy(&type);
  ast_typeexpr_destroy(&target);
  ast_generics_destroy(&generics);
}

void import_constructors(struct checkstate *cs) {
  ident_value t_ident = identmap_intern_c_str(cs->im, "T");
  struct ast_generics generics;
  {
    struct ast_ident_array param = ast_ident_array_malloc(1);
    param.ptr[0] = make_ast_ident(t_ident);
    ast_generics_init_has_params(&generics, ast_meta_make_garbage(), param);
  }

  struct ast_typeexpr target;
  init_name_type(&target, t_ident);

  {
    struct ast_typeexpr func1;
    make_ptr_func_type(cs, &target, 1, VOID_TYPE_NAME, &func1);

    name_table_add_primitive_def(cs->im,
                                 &cs->nt,
                                 cs->cm.init,
                                 make_primop(PRIMITIVE_OP_INIT),
                                 &generics,
                                 &func1);
    name_table_add_primitive_def(cs->im,
                                 &cs->nt,
                                 cs->cm.destroy,
                                 make_primop(PRIMITIVE_OP_DESTROY),
                                 &generics,
                                 &func1);

    ast_typeexpr_destroy(&func1);
  }

  {
    struct ast_typeexpr func2;
    make_ptr_func_type(cs, &target, 2, VOID_TYPE_NAME, &func2);

    name_table_add_primitive_def(cs->im,
                                 &cs->nt,
                                 cs->cm.move,
                                 make_primop(PRIMITIVE_OP_MOVE),
                                 &generics,
                                 &func2);
    name_table_add_primitive_def(cs->im,
                                 &cs->nt,
                                 cs->cm.copy,
                                 make_primop(PRIMITIVE_OP_COPY),
                                 &generics,
                                 &func2);

    ast_typeexpr_destroy(&func2);
  }

  ast_typeexpr_destroy(&target);
  ast_generics_destroy(&generics);
}

void checkstate_import_primitive_defs(struct checkstate *cs) {
  import_ptr_binops(cs);
  import_integer_binops(cs, binop_u8_primitive_ops, U8_TYPE_NAME);
  import_integer_binops(cs, binop_i8_primitive_ops, I8_TYPE_NAME);
  import_integer_binops(cs, binop_u16_primitive_ops, U16_TYPE_NAME);
  import_integer_binops(cs, binop_i16_primitive_ops, I16_TYPE_NAME);
  import_integer_binops(cs, binop_u32_primitive_ops, U32_TYPE_NAME);
  import_integer_binops(cs, binop_i32_primitive_ops, I32_TYPE_NAME);
  import_integer_binops(cs, binop_size_primitive_ops, SIZE_TYPE_NAME);
  import_integer_binops(cs, binop_osize_primitive_ops, OSIZE_TYPE_NAME);
  import_bool_binops(cs);

  import_integer_conversions(cs);

  {
    /* TODO: String values duplicated with parse code, I guess. */
    import_unop(cs, PRIMITIVE_OP_NEGATE_I8, "-", I8_TYPE_NAME);
    import_unop(cs, PRIMITIVE_OP_NEGATE_I16, "-", I16_TYPE_NAME);
    import_unop(cs, PRIMITIVE_OP_NEGATE_I32, "-", I32_TYPE_NAME);

    import_unop(cs, PRIMITIVE_OP_LOGICAL_NOT, "!", BOOL_TYPE_NAME);

    import_unop(cs, PRIMITIVE_OP_BIT_NOT_I8, "^", I8_TYPE_NAME);
    import_unop(cs, PRIMITIVE_OP_BIT_NOT_U8, "^", U8_TYPE_NAME);
    import_unop(cs, PRIMITIVE_OP_BIT_NOT_I16, "^", I16_TYPE_NAME);
    import_unop(cs, PRIMITIVE_OP_BIT_NOT_U16, "^", U16_TYPE_NAME);
    import_unop(cs, PRIMITIVE_OP_BIT_NOT_I32, "^", I32_TYPE_NAME);
    import_unop(cs, PRIMITIVE_OP_BIT_NOT_U32, "^", U32_TYPE_NAME);
    import_unop(cs, PRIMITIVE_OP_BIT_NOT_SIZE, "^", SIZE_TYPE_NAME);
    import_unop(cs, PRIMITIVE_OP_BIT_NOT_OSIZE, "^", OSIZE_TYPE_NAME);
  }

  import_sizeof_alignof(cs);

  import_constructors(cs);
}

void checkstate_import_primitives(struct checkstate *cs) {
  checkstate_import_primitive_types(cs);
  checkstate_import_primitive_defs(cs);
}

void init_boolean_typeexpr(struct checkstate *cs, struct ast_typeexpr *a) {
  init_name_type(a, cs->cm.boole);
}

void stderr_errmsg(struct error_dump *ctx, struct identmap *im,
                   size_t line, size_t column, const char *msg, size_t msglen) {
  ERR("%.*s:%"PRIz":%"PRIz": Parse error: %.*s\n",
      IM_P(im, ctx->filepath), line, size_add(column, 1), size_to_int(msglen), msg);
}

int resolve_import_filename_and_parse(struct checkstate *cs,
                                      ident_value name,
                                      size_t *global_offset_base_out,
                                      struct ast_file *file_out,
                                      ident_value *filepath_out,
                                      uint8_t **buf_out,
                                      size_t *buf_count_out) {
  const void *module_name;
  size_t module_name_count;
  identmap_lookup(cs->im, name, &module_name, &module_name_count);

  char *filepath = NULL;
  size_t filepath_size = 0;
  uint8_t *data;
  size_t data_size;
  if (!(*cs->loader)(cs->loader_ctx, module_name, module_name_count,
                     &filepath, &filepath_size, &data, &data_size)) {
    ERR("Could not read file '%.*s' for module %.*s.\n",
        size_to_int(filepath_size), filepath,
        size_to_int(module_name_count), (const char *)module_name);
    goto fail;
  }

  ident_value filepath_ident;
  if (filepath) {
    filepath_ident = identmap_intern(cs->im, filepath, filepath_size);
  } else {
    filepath_ident = identmap_intern_c_str(cs->im, "(no filepath)");
  }
  free(filepath);

  size_t global_offset_base = cs->total_filesize;
  cs->total_filesize = size_add(1, size_add(cs->total_filesize, data_size));

  struct error_dump error_dump;
  error_dump.filepath = filepath_ident;
  error_dump.dumper = &stderr_errmsg;
  if (!parse_buf_file(cs->im, data, data_size, global_offset_base,
                      file_out, &error_dump)) {
    goto fail_data;
  }

  *global_offset_base_out = global_offset_base;
  *filepath_out = filepath_ident;
  *buf_out = data;
  *buf_count_out = data_size;
  return 1;
 fail_data:
  free(data);
 fail:
  return 0;
}

struct import_chase_state {
  struct ident_value_slice names;

  struct defclass_ident_slice accessible;
};

void defclass_ident_destroy(struct defclass_ident *dci) {
  (void)dci;
  /* Do nothing. */
}

void copy_make_unary_func_type(struct checkstate *cs,
                               struct ast_typeexpr *arg_type,
                               struct ast_typeexpr *return_type,
                               struct ast_typeexpr *out) {
  struct ast_typeexpr_array params = ast_typeexpr_array_malloc(2);
  ast_typeexpr_init_copy(&params.ptr[0], arg_type);
  ast_typeexpr_init_copy(&params.ptr[1], return_type);

  out->tag = AST_TYPEEXPR_APP;
  ast_typeapp_init(&out->u.app, ast_meta_make_garbage(), make_ast_ident(cs->cm.func),
                   params);
}

int add_enum_constructors(struct checkstate *cs,
                          struct ast_generics *generics,
                          struct ast_ident *name,
                          struct ast_enumspec *enumspec) {
  for (size_t i = 0, e = enumspec->enumfields_count; i < e; i++) {
    struct ast_vardecl *f = &enumspec->enumfields[i];

    if (f->name.value == name->value) {
      METERR(cs, f->name.meta, "enum constructor %.*s matches enum name\n",
             IM_P(cs->im, f->name.value));
      return 0;
    }

    for (size_t j = 0; j < i; j++) {
      if (f->name.value == enumspec->enumfields[j].name.value) {
        METERR(cs, f->name.meta, "enum constructor %.*s with duplicate name.\n",
               IM_P(cs->im, f->name.value));
        return 0;
      }
    }

    struct ast_typeexpr_array params = ast_typeexpr_array_malloc(2);
    ast_typeexpr_init_copy(&params.ptr[0], &f->type);

    if (!generics->has_type_params) {
      params.ptr[1].tag = AST_TYPEEXPR_NAME;
      ast_ident_init_copy(&params.ptr[1].u.name, name);
    } else {
      struct ast_typeexpr_array app_params = ast_typeexpr_array_malloc(generics->params.count);
      for (size_t j = 0, je = generics->params.count; j < je; j++) {
        app_params.ptr[j].tag = AST_TYPEEXPR_NAME;
        ast_ident_init_copy(&app_params.ptr[j].u.name, &generics->params.ptr[j]);
      }
      struct ast_ident name_copy;
      ast_ident_init_copy(&name_copy, name);
      params.ptr[1].tag = AST_TYPEEXPR_APP;
      ast_typeapp_init(&params.ptr[1].u.app, ast_meta_make_garbage(),
                       name_copy, app_params);
    }

    struct ast_typeexpr func_type;
    func_type.tag = AST_TYPEEXPR_APP;
    ast_typeapp_init(&func_type.u.app, ast_meta_make_garbage(),
                     make_ast_ident(cs->cm.func), params);

    if (!name_table_add_primitive_def(
            cs->im,
            &cs->nt,
            f->name.value,
            make_enumconstruct_op(i),
            generics,
            &func_type)) {
      ast_typeexpr_destroy(&func_type);
      return 0;
    }

    if (f->type.tag == AST_TYPEEXPR_NAME && f->type.u.name.value == cs->cm.voide) {
      if (!name_table_add_primitive_def(
              cs->im,
              &cs->nt,
              f->name.value,
              make_enumvoid_op(i),
              generics,
              &func_type.u.app.params.ptr[1])) {
        ast_typeexpr_destroy(&func_type);
        return 0;
      }
    }

    ast_typeexpr_destroy(&func_type);
  }

  return 1;
}

int make_complete_lambda_typeexpr(struct common_idents *cm, struct ast_expr *a, struct ast_typeexpr *out) {
  switch (a->tag) {
  case AST_EXPR_LAMBDA: {
    struct ast_lambda *lam = &a->u.lambda;

    size_t args_count = size_add(lam->params_count, 1);
    struct ast_typeexpr_array args = ast_typeexpr_array_malloc(args_count);
    for (size_t i = 0, e = lam->params_count; i < e; i++) {
      ast_typeexpr_init_copy(&args.ptr[i], &lam->params[i].type);
    }
    ast_typeexpr_init_copy(&args.ptr[lam->params_count], &lam->return_type);

    out->tag = AST_TYPEEXPR_APP;
    ast_typeapp_init(&out->u.app, ast_meta_make_garbage(),
                     make_ast_ident(cm->func), args);
    return 1;
  } break;
  default:
    return 0;
  }
}

int chase_through_toplevels(struct checkstate *cs,
                            struct import_chase_state *ics,
                            struct ast_toplevel *toplevels,
                            size_t toplevels_count) {
  for (size_t i = 0; i < toplevels_count; i++) {
    struct ast_toplevel *toplevel = &toplevels[i];
    switch (toplevel->tag) {
    case AST_TOPLEVEL_IMPORT: {
      ident_value_slice_push(&ics->names, toplevel->u.import.name.value);
    } break;
    case AST_TOPLEVEL_DEF: {
      if (!toplevel->u.def.has_typeexpr) {
        if (!make_complete_lambda_typeexpr(&cs->cm, &toplevel->u.def.rhs, &toplevel->u.def.typeexpr)) {
          METERR(cs, toplevel->u.def.meta, "Incomplete type for def %.*s\n",
                 IM_P(cs->im, toplevel->u.def.name.value));
          return 0;
        }
        toplevel->u.def.has_typeexpr = 1;
      }

      if (!name_table_add_def(cs->im,
                              &cs->nt,
                              toplevel->u.def.name.value,
                              &toplevel->u.def.generics,
                              ast_def_typeexpr(&toplevel->u.def),
                              ics->accessible.ptr,
                              ics->accessible.count,
                              toplevel->u.def.is_export,
                              &toplevel->u.def)) {
        return 0;
      }
    } break;
    case AST_TOPLEVEL_EXTERN_DEF: {
      if (!name_table_add_extern_def(cs->im,
                                     &cs->nt,
                                     toplevel->u.extern_def.name.value,
                                     &toplevel->u.extern_def.type)) {
        return 0;
      }
    } break;
    case AST_TOPLEVEL_DEFTYPE: {
      struct ast_deftype *dt = &toplevel->u.deftype;
      struct generics_arity arity = params_arity(&dt->generics);
      if (!name_table_add_deftype(cs->im, &cs->nt, dt->name.value, arity, dt)) {
        return 0;
      }
      switch (dt->rhs.tag) {
      case AST_DEFTYPE_RHS_TYPE:
        break;
      case AST_DEFTYPE_RHS_ENUMSPEC: {
        if (!add_enum_constructors(cs, &dt->generics, &dt->name, &dt->rhs.u.enumspec)) {
          return 0;
        }
      } break;
      default:
        UNREACHABLE();
      }
    } break;
    case AST_TOPLEVEL_ACCESS: {
      struct defclass_ident dci;
      dci.name = toplevel->u.access.name.value;
      dci.arity = toplevel->u.access.arity;
      defclass_ident_slice_push(&ics->accessible, dci);
      int success = chase_through_toplevels(cs, ics,
                                            toplevel->u.access.toplevels,
                                            toplevel->u.access.toplevels_count);
      defclass_ident_slice_pop(&ics->accessible, defclass_ident_destroy);
      if (!success) {
        return 0;
      }
    } break;
    default:
      UNREACHABLE();
    }
  }

  return 1;
}

int chase_imports(struct checkstate *cs, ident_value name) {
  int ret = 0;
  struct import_chase_state ics = { SLICE_INITIALIZER, SLICE_INITIALIZER };

  ident_value_slice_push(&ics.names, name);

  while (ics.names.count) {
    name = ics.names.ptr[--ics.names.count];

    for (size_t i = 0, e = cs->imports.count; i < e; i++) {
      if (cs->imports.ptr[i].import_name == name) {
        goto continue_outer;
      }
    }

    size_t global_offset_base;
    struct ast_file file;
    ident_value filepath;
    uint8_t *buf;
    size_t buf_count;
    if (!resolve_import_filename_and_parse(cs, name, &global_offset_base,
                                           &file, &filepath, &buf, &buf_count)) {
      goto cleanup;
    }

    struct ast_file *heap_file = malloc(sizeof(*heap_file));
    CHECK(heap_file);
    *heap_file = file;
    struct import imp;
    imp.import_name = name;
    imp.import_filepath = filepath;
    imp.global_offset_base = global_offset_base;
    imp.file = heap_file;
    imp.buf = buf;
    imp.buf_count = buf_count;
    import_slice_push(&cs->imports, imp);

    if (!chase_through_toplevels(cs, &ics, heap_file->toplevels, heap_file->toplevels_count)) {
      goto cleanup;
    }

  continue_outer:
    continue;
  }

  ret = 1;
 cleanup:
  ident_value_slice_destroy_prim(&ics.names);
  defclass_ident_slice_destroy_prim(&ics.accessible);
  return ret;
}

int lookup_import(struct checkstate *cs, ident_value name,
                  struct ast_file **file_out) {
  for (size_t i = 0, e = cs->imports.count; i < e; i++) {
    if (cs->imports.ptr[i].import_name == name) {
      *file_out = cs->imports.ptr[i].file;
      return 1;
    }
  }
  return 0;
}

int generics_lookup_name(struct ast_generics *a,
                         ident_value name,
                         size_t *index_out) {
  if (!a->has_type_params) {
    return 0;
  }
  for (size_t i = 0, e = a->params.count; i < e; i++) {
    if (a->params.ptr[i].value == name) {
      *index_out = i;
      return 1;
    }
  }
  return 0;
}

int check_deftype(struct checkstate *cs, struct deftype_entry *ent);
int check_typeexpr(struct checkstate *cs,
                   struct ast_generics *generics,
                   struct ast_typeexpr *a,
                   struct deftype_entry *flat_typeexpr);

int check_typeexpr_name(struct checkstate *cs,
                        struct ast_generics *generics,
                        struct ast_ident *a,
                        struct deftype_entry *flat_typeexpr) {
  CHECK_DBG("check_typeexpr_name\n");
  ident_value name = a->value;
  size_t which_generic;
  if (generics_lookup_name(generics, name, &which_generic)) {
    if (flat_typeexpr) {
      deftype_entry_mark_generic_flatly_held(flat_typeexpr,
                                             which_generic);
    }
  } else {
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(&cs->nt, name, no_param_list_arity(),
                                   &ent)) {
      METERR(cs, a->meta, "Unrecognized type name '%.*s'.\n", IM_P(cs->im, name));
      return 0;
    }

    if (flat_typeexpr) {
      if (!check_deftype(cs, ent)) {
        return 0;
      }
    }
  }

  return 1;
}

int check_typeexpr_app(struct checkstate *cs,
                       struct ast_generics *generics,
                       struct ast_typeapp *a,
                       struct deftype_entry *flat_typeexpr) {
  CHECK_DBG("check_typeexpr_app\n");
  struct deftype_entry *ent;
  if (!name_table_lookup_deftype(&cs->nt, a->name.value,
                                 param_list_arity(a->params.count),
                                 &ent)) {
    METERR(cs, a->meta, "Type lookup fail for %.*s, arity %"PRIz"\n",
           IM_P(cs->im, a->name.value), a->params.count);
    return 0;
  }

  if (flat_typeexpr) {
    if (!check_deftype(cs, ent)) {
      return 0;
    }
  }

  for (size_t i = 0, e = a->params.count; i < e; i++) {
    int f = deftype_entry_param_is_flatly_held(ent, i);
    if (!check_typeexpr(cs, generics, &a->params.ptr[i],
                        f ? flat_typeexpr : NULL)) {
      return 0;
    }
  }

  return 1;
}

int check_typeexpr_fields(struct checkstate *cs,
                          struct ast_generics *generics,
                          struct ast_vardecl *fields,
                          size_t fields_count,
                          struct deftype_entry *flat_typeexpr) {
  for (size_t i = 0; i < fields_count; i++) {
    struct ast_vardecl *field = &fields[i];
    for (size_t j = 0; j < i; j++) {
      if (field->name.value == fields[j].name.value) {
        METERR(cs, field->meta, "struct/union fields have duplicate name %.*s\n",
               IM_P(cs->im, field->name.value));
        return 0;
      }
    }

    {
      size_t which_generic;
      if (generics_lookup_name(generics, field->name.value, &which_generic)) {
        METERR(cs, field->meta,
               "struct/union field shadows template parameter %.*s, "
               "which is gauche.\n",
               IM_P(cs->im, field->name.value));
        return 0;
      }
    }

    if (!check_typeexpr(cs, generics, &field->type, flat_typeexpr)) {
      return 0;
    }
  }

  return 1;
}

int check_typeexpr(struct checkstate *cs,
                   struct ast_generics *generics,
                   struct ast_typeexpr *a,
                   struct deftype_entry *flat_typeexpr) {
  CHECK_DBG("check_typeexpr\n");
  /* null means we have to worry about flatness, non-null means we don't. */
  switch (a->tag) {
  case AST_TYPEEXPR_NAME:
    return check_typeexpr_name(cs, generics, &a->u.name, flat_typeexpr);
  case AST_TYPEEXPR_APP:
    return check_typeexpr_app(cs, generics, &a->u.app, flat_typeexpr);
  case AST_TYPEEXPR_STRUCTE:
    return check_typeexpr_fields(cs, generics,
                                 a->u.structe.fields,
                                 a->u.structe.fields_count,
                                 flat_typeexpr);
  case AST_TYPEEXPR_UNIONE:
    return check_typeexpr_fields(cs, generics,
                                 a->u.structe.fields,
                                 a->u.structe.fields_count,
                                 flat_typeexpr);
  case AST_TYPEEXPR_ARRAY:
    return check_typeexpr(cs, generics, a->u.arraytype.param, flat_typeexpr);
  default:
    UNREACHABLE();
  }
}

int check_enumspec(struct checkstate *cs,
                   struct ast_generics *generics,
                   struct ast_enumspec *a,
                   struct deftype_entry *flat_typeexpr) {
  CHECK_DBG("check_enumspec\n");
  return check_typeexpr_fields(cs, generics,
                               a->enumfields,
                               a->enumfields_count,
                               flat_typeexpr);
}

enum typeexpr_trait min_typeexpr_trait(enum typeexpr_trait a, enum typeexpr_trait b) {
  return a < b ? a : b;
}

/* TODO: This should really return a 3-way enum.  It's icky that the
caller assumes it only needs to check the destructor on
lookup_copy. */
void deftype_trait_strategies(enum ast_deftype_disposition disposition,
                              int *lookup_move_out,
                              int *lookup_copy_out,
                              int *lookup_init_out) {
  switch (disposition) {
  case AST_DEFTYPE_NOT_CLASS:
    *lookup_move_out = 0;
    *lookup_copy_out = 0;
    *lookup_init_out = 0;
    break;
  case AST_DEFTYPE_CLASS_DEFAULT_COPY_MOVE_DESTROY:
    *lookup_move_out = 0;
    *lookup_copy_out = 0;
    *lookup_init_out = 1;
    break;
  case AST_DEFTYPE_CLASS_DEFAULT_MOVE:
    *lookup_move_out = 0;
    *lookup_copy_out = 1;
    *lookup_init_out = 1;
    break;
  case AST_DEFTYPE_CLASS_NO_DEFAULTS:
    *lookup_move_out = 1;
    *lookup_copy_out = 1;
    *lookup_init_out = 1;
    break;
  default:
    UNREACHABLE();
  }
}

void make_pointee_func_lookup_type(struct checkstate *cs,
                                   struct ast_typeexpr *a,
                                   size_t count,
                                   struct ast_typeexpr *out) {
  size_t params_count = size_add(count, 1);
  struct ast_typeexpr_array params = ast_typeexpr_array_malloc(params_count);
  for (size_t i = 0; i < count; i++) {
    wrap_in_ptr(&cs->cm, a, &params.ptr[i]);
  }
  /* Unknown return type -- we'll match anything named
  "copy"/"move"/"destroy" with any return type. */
  params.ptr[count] = ast_unknown_garbage();

  struct ast_ident func_name = make_ast_ident(cs->cm.func);

  out->tag = AST_TYPEEXPR_APP;
  ast_typeapp_init(&out->u.app, ast_meta_make_garbage(), func_name, params);
}

/* Returns false if multiple matching definitions. */
int has_explicit_movecopydestroy(struct checkstate *cs,
                                 struct ast_meta *meta,
                                 struct ast_typeexpr *a,
                                 /* copy/move takes 2, destroy takes 1. */
                                 size_t argdupes,
                                 ident_value name,
                                 struct exprscope *also_typecheck,
                                 int *result_out,
                                 struct def_instantiation **inst_out) {
  /* TODO: Pass meta to lookup_global_maybe_typecheck or make_ast_ident or something. */
  (void)meta;

  struct ast_typeexpr func_type;
  make_pointee_func_lookup_type(cs, a, argdupes, &func_type);

  struct ast_name_expr func_name;
  ast_name_expr_init(&func_name, make_ast_ident(name));

  enum match_result match_result;
  struct ast_typeexpr unified;
  int lvalue_discard;
  struct def_instantiation *inst;
  if (!lookup_global_maybe_typecheck(
          cs,
          also_typecheck,
          &func_name,
          &func_type,
          REPORT_MODE_MULTI_MATCH,
          &match_result,
          &unified,
          &lvalue_discard,
          &inst)) {
    if (match_result == MATCH_NONE) {
      inst = NULL;
      goto success;
    }
    goto fail;
  }
  ast_typeexpr_destroy(&unified);

 success:
  ast_name_expr_destroy(&func_name);
  ast_typeexpr_destroy(&func_type);
  *result_out = (match_result == MATCH_SUCCESS);
  *inst_out = inst;
  return 1;

 fail:
  ast_name_expr_destroy(&func_name);
  ast_typeexpr_destroy(&func_type);
  return 0;
}

int has_explicit_copy(struct checkstate *cs, struct ast_meta *meta, struct ast_typeexpr *a, struct exprscope *also_typecheck, int *result_out,
                      struct def_instantiation **inst_out) {
  return has_explicit_movecopydestroy(cs, meta, a, 2, cs->cm.do_copy, also_typecheck, result_out, inst_out);
}

int has_explicit_destroy(struct checkstate *cs, struct ast_meta *meta, struct ast_typeexpr *a, struct exprscope *also_typecheck, int *result_out,
                         struct def_instantiation **inst_out) {
  return has_explicit_movecopydestroy(cs, meta, a, 1, cs->cm.do_destroy, also_typecheck, result_out, inst_out);
}

int has_explicit_move(struct checkstate *cs, struct ast_meta *meta, struct ast_typeexpr *a, struct exprscope *also_typecheck, int *result_out,
                      struct def_instantiation **inst_out) {
  return has_explicit_movecopydestroy(cs, meta, a, 2, cs->cm.do_move, also_typecheck, result_out, inst_out);
}

int has_explicit_init(struct checkstate *cs, struct ast_meta *meta, struct ast_typeexpr *a, struct exprscope *also_typecheck, int *result_out,
                      struct def_instantiation **inst_out) {
  return has_explicit_movecopydestroy(cs, meta, a, 1, cs->cm.do_init, also_typecheck, result_out, inst_out);
}

int check_typeexpr_traits(struct checkstate *cs,
                          /* a is a concrete type. */
                          struct ast_typeexpr *a,
                          struct exprscope *also_typecheck,
                          struct typeexpr_traits *out);
int check_enumspec_traits(struct checkstate *cs,
                          /* a is a concrete enumspec. */
                          struct ast_enumspec *a,
                          struct exprscope *also_typecheck,
                          struct typeexpr_traits *out);

int finish_checking_name_traits(struct checkstate *cs,
                                /* The original concrete name/app type we are checking traits for. */
                                struct ast_typeexpr *a,
                                struct ast_meta *deftype_meta,
                                enum ast_deftype_disposition deftype_disposition,
                                struct ast_deftype_rhs *concrete_deftype_rhs,
                                struct exprscope *also_typecheck,
                                struct typeexpr_traits *out,
                                struct typeexpr_trait_instantiations *insts_out) {
  int lookup_move;
  int lookup_copy;
  int lookup_init;
  deftype_trait_strategies(deftype_disposition, &lookup_move, &lookup_copy, &lookup_init);

  struct typeexpr_traits rhs_traits;
  if (!(lookup_move && lookup_copy && lookup_init)) {
    switch (concrete_deftype_rhs->tag) {
    case AST_DEFTYPE_RHS_TYPE:
      if (!check_typeexpr_traits(cs, &concrete_deftype_rhs->u.type,
                                 also_typecheck, &rhs_traits)) {
        return 0;
      }
      break;
    case AST_DEFTYPE_RHS_ENUMSPEC:
      if (!check_enumspec_traits(cs, &concrete_deftype_rhs->u.enumspec,
                                 also_typecheck, &rhs_traits)) {
        return 0;
      }
      break;
    default:
      UNREACHABLE();
    }
  }

  int explicit_move;
  struct def_instantiation *move_inst;
  if (!has_explicit_move(cs, deftype_meta, a, also_typecheck, &explicit_move, &move_inst)) {
    return 0;
  }

  int explicit_copy;
  struct def_instantiation *copy_inst;
  if (!has_explicit_copy(cs, deftype_meta, a, also_typecheck, &explicit_copy, &copy_inst)) {
    return 0;
  }

  int explicit_destroy;
  struct def_instantiation *destroy_inst;
  if (!has_explicit_destroy(cs, deftype_meta, a, also_typecheck, &explicit_destroy, &destroy_inst)) {
    return 0;
  }

  int explicit_init;
  struct def_instantiation *init_inst;
  if (!has_explicit_init(cs, deftype_meta, a, also_typecheck, &explicit_init, &init_inst)) {
    return 0;
  }

  if (lookup_move) {
    rhs_traits.movable = explicit_move ? TYPEEXPR_TRAIT_HAD : TYPEEXPR_TRAIT_LACKED;
  } else {
    if (explicit_move) {
      METERR(cs, *deftype_meta, "Type has both implicit and explicit move.%s", "\n");
      return 0;
    }
  }

  if (lookup_copy) {
    rhs_traits.copyable = explicit_copy ? TYPEEXPR_TRAIT_HAD : TYPEEXPR_TRAIT_LACKED;
    if (!explicit_destroy) {
      METERR(cs, *deftype_meta, "Type lacks explicit destructor.%s", "\n");
      return 0;
    }
  } else {
    if (explicit_copy) {
      METERR(cs, *deftype_meta, "Type has both implicit and explicit copy.%s", "\n");
      return 0;
    }
    if (explicit_destroy) {
      METERR(cs, *deftype_meta, "Type has both implicit and explicit destroy.%s", "\n");
      return 0;
    }
  }

  if (lookup_init) {
    rhs_traits.inittible = explicit_init ? TYPEEXPR_TRAIT_HAD : TYPEEXPR_TRAIT_LACKED;
  } else {
    if (explicit_init) {
      METERR(cs, *deftype_meta, "Type has both implicit and explicit init.%s", "\n");
      return 0;
    }
  }

  *out = rhs_traits;
  insts_out->move_inst = move_inst;
  insts_out->copy_inst = copy_inst;
  insts_out->destroy_inst = destroy_inst;
  insts_out->init_inst = init_inst;
  return 1;
}

int check_typeexpr_name_traits(struct checkstate *cs,
                               struct ast_typeexpr *a,
                               struct exprscope *also_typecheck,
                               struct typeexpr_traits *out,
                               struct typeexpr_trait_instantiations *insts_out,
                               int *has_concrete_deftype_rhs_out_or_null,
                               struct ast_deftype_rhs *concrete_deftype_rhs_out_or_null) {
  CHECK((has_concrete_deftype_rhs_out_or_null == NULL)
        == (concrete_deftype_rhs_out_or_null == NULL));
  CHECK(a->tag == AST_TYPEEXPR_NAME);
  struct deftype_entry *ent;
  struct deftype_instantiation *inst;
  if (!name_table_lookup_deftype_inst(cs->im, &cs->nt, a->u.name.value,
                                      NULL, 0,
                                      &ent, &inst)) {
    METERR(cs, *ast_typeexpr_meta(a), "Invalid type name %.*s found.\n", IM_P(cs->im, a->u.name.value));
    return 0;
  }

  if (ent->is_primitive) {
    out->movable = TYPEEXPR_TRAIT_TRIVIALLY_HAD;
    out->copyable = TYPEEXPR_TRAIT_TRIVIALLY_HAD;
    out->inittible = TYPEEXPR_TRAIT_TRIVIALLY_HAD;
    insts_out->move_inst = NULL;
    insts_out->copy_inst = NULL;
    insts_out->destroy_inst = NULL;
    insts_out->init_inst = NULL;
    if (concrete_deftype_rhs_out_or_null) {
      *has_concrete_deftype_rhs_out_or_null = 0;
      concrete_deftype_rhs_out_or_null->tag = (enum ast_deftype_rhs_tag)-1;
    }
    return 1;
  }

  int ret;
  if (inst->has_typeexpr_traits) {
    *out = inst->typeexpr_traits;
    *insts_out = inst->explicit_trait_instantiations;
    ret = 1;
  } else {
    /* We should have already typechecked the insts at typechecking time. */
    if (!also_typecheck) {
      METERR(cs, *ast_typeexpr_meta(a), "ICE: also_typecheck null for name %.*s\n", IM_P(cs->im, a->u.name.value));
      CRASH("in check_typeexpr_name_traits");
    }
    struct typeexpr_traits traits;
    struct typeexpr_trait_instantiations trait_insts;
    ret = finish_checking_name_traits(cs,
                                      a,
                                      &ent->deftype->meta,
                                      ent->deftype->disposition,
                                      &ent->deftype->rhs,
                                      also_typecheck,
                                      &traits,
                                      &trait_insts);
    if (ret) {
      /* Maybe recursive typechecking already cached the traits... */
      if (!inst->has_typeexpr_traits) {
        inst->has_typeexpr_traits = 1;
        inst->typeexpr_traits = traits;
        inst->explicit_trait_instantiations = trait_insts;
        ast_deftype_rhs_init_copy(&inst->concrete_rhs, &ent->deftype->rhs);
      }
      *out = traits;
      *insts_out = trait_insts;
    }
  }
  if (ret && concrete_deftype_rhs_out_or_null) {
    *has_concrete_deftype_rhs_out_or_null = 1;
    /* (Yes, this _is_ a concrete RHS -- because there's no generic
       param list when looking up a name.) */
    ast_deftype_rhs_init_copy(concrete_deftype_rhs_out_or_null, &inst->concrete_rhs);
  }
  return ret;
}

int check_typeexpr_app_traits(struct checkstate *cs,
                              struct ast_typeexpr *a,
                              struct exprscope *also_typecheck,
                              struct typeexpr_traits *out,
                              struct typeexpr_trait_instantiations *insts_out,
                              int *has_concrete_deftype_rhs_out_or_null,
                              struct ast_deftype_rhs *concrete_deftype_rhs_out_or_null) {
  CHECK((has_concrete_deftype_rhs_out_or_null == NULL)
        == (concrete_deftype_rhs_out_or_null == NULL));
  CHECK(a->tag == AST_TYPEEXPR_APP);
  struct deftype_entry *ent;
  struct deftype_instantiation *inst;
  if (!name_table_lookup_deftype_inst(cs->im, &cs->nt, a->u.app.name.value,
                                      a->u.app.params.ptr,
                                      a->u.app.params.count,
                                      &ent, &inst)) {
    METERR(cs, *ast_typeexpr_meta(a), "An invalid generic type '%.*s'\n",
           IM_P(cs->im, a->u.app.name.value));
    return 0;
  }

  if (ent->is_primitive) {
    out->movable = TYPEEXPR_TRAIT_TRIVIALLY_HAD;
    out->copyable = TYPEEXPR_TRAIT_TRIVIALLY_HAD;
    out->inittible = TYPEEXPR_TRAIT_TRIVIALLY_HAD;
    insts_out->move_inst = NULL;
    insts_out->copy_inst = NULL;
    insts_out->destroy_inst = NULL;
    insts_out->init_inst = NULL;
    if (concrete_deftype_rhs_out_or_null) {
      *has_concrete_deftype_rhs_out_or_null = 0;
      concrete_deftype_rhs_out_or_null->tag = (enum ast_deftype_rhs_tag)-1;
    }
    return 1;
  }

  int ret;
  if (inst->has_typeexpr_traits) {
    *out = inst->typeexpr_traits;
    *insts_out = inst->explicit_trait_instantiations;
    ret = 1;
  } else {
    CHECK(also_typecheck);
    struct ast_deftype *deftype = ent->deftype;

    CHECK(deftype->generics.has_type_params
          && deftype->generics.params.count == a->u.app.params.count);

    struct ast_deftype_rhs concrete_deftype_rhs;
    do_replace_rhs_generics(&deftype->generics,
                            a->u.app.params.ptr,
                            a->u.app.params.count,
                            &deftype->rhs,
                            &concrete_deftype_rhs);

    struct typeexpr_traits traits;
    struct typeexpr_trait_instantiations trait_insts;
    ret = finish_checking_name_traits(cs,
                                      a,
                                      &deftype->meta,
                                      deftype->disposition,
                                      &concrete_deftype_rhs,
                                      also_typecheck,
                                      &traits,
                                      &trait_insts);
    if (ret) {
      if (inst->has_typeexpr_traits) {
        /* Recursive typechecking already applied the traits... */
        ast_deftype_rhs_destroy(&concrete_deftype_rhs);
      } else {
        inst->has_typeexpr_traits = 1;
        inst->typeexpr_traits = traits;
        inst->explicit_trait_instantiations = trait_insts;
        inst->concrete_rhs = concrete_deftype_rhs;
      }
      *out = traits;
      *insts_out = trait_insts;
    } else {
      ast_deftype_rhs_destroy(&concrete_deftype_rhs);
    }
  }

  if (ret && concrete_deftype_rhs_out_or_null) {
    *has_concrete_deftype_rhs_out_or_null = 1;
    ast_deftype_rhs_init_copy(concrete_deftype_rhs_out_or_null, &inst->concrete_rhs);
  }
  return ret;
}

/* The composability of traits of struct and enumspec fields are the
same -- the thing's trivially copyable if all the fields are trivially
copyable, and the thing's copyable if all the fields are copyable.
Likewise for move.  But not so for init: enum types are not
default-inittable. */
int check_typeexpr_structe_enumspec_fields_traits(
    struct checkstate *cs,
    struct ast_vardecl *fields,
    size_t fields_count,
    struct exprscope *also_typecheck,
    struct typeexpr_traits *out) {
  struct typeexpr_traits combined;
  combined.movable = TYPEEXPR_TRAIT_TRIVIALLY_HAD;
  combined.copyable = TYPEEXPR_TRAIT_TRIVIALLY_HAD;
  combined.inittible = TYPEEXPR_TRAIT_TRIVIALLY_HAD;
  for (size_t i = 0; i < fields_count; i++) {
    struct typeexpr_traits traits;
    if (!check_typeexpr_traits(cs, &fields[i].type, also_typecheck, &traits)) {
      return 0;
    }

    combined.movable = min_typeexpr_trait(combined.movable, traits.movable);
    combined.copyable = min_typeexpr_trait(combined.copyable, traits.copyable);
    combined.inittible = min_typeexpr_trait(combined.inittible, traits.inittible);
  }

  *out = combined;
  return 1;
}

int check_typeexpr_structe_traits(struct checkstate *cs,
                                  struct ast_typeexpr *a,
                                  struct exprscope *also_typecheck,
                                  struct typeexpr_traits *out) {
  return check_typeexpr_structe_enumspec_fields_traits(
      cs, a->u.structe.fields, a->u.structe.fields_count, also_typecheck, out);
}

int check_typeexpr_unione_traits(struct checkstate *cs,
                                 struct ast_typeexpr *a,
                                 struct exprscope *also_typecheck,
                                 struct typeexpr_traits *out) {
  for (size_t i = 0, e = a->u.unione.fields_count; i < e; i++) {
    struct typeexpr_traits traits;
    if (!check_typeexpr_traits(cs, &a->u.unione.fields[i].type, also_typecheck, &traits)) {
      return 0;
    }

    if (traits.movable != TYPEEXPR_TRAIT_TRIVIALLY_HAD) {
      METERR(cs, a->u.unione.meta, "Union field %.*s is not trivially movable.\n",
             IM_P(cs->im, a->u.unione.fields[i].name.value));
      return 0;
    }

    if (traits.copyable != TYPEEXPR_TRAIT_TRIVIALLY_HAD) {
      METERR(cs, a->u.unione.meta, "Union field %.*s is not trivially copyable.\n",
             IM_P(cs->im, a->u.unione.fields[i].name.value));
      return 0;
    }

    if (traits.inittible != TYPEEXPR_TRAIT_TRIVIALLY_HAD) {
      METERR(cs, a->u.unione.meta, "Union field %.*s is not trivially initializable.\n",
             IM_P(cs->im, a->u.unione.fields[i].name.value));
      return 0;
    }
  }

  out->movable = TYPEEXPR_TRAIT_TRIVIALLY_HAD;
  out->copyable = TYPEEXPR_TRAIT_TRIVIALLY_HAD;
  out->inittible = TYPEEXPR_TRAIT_TRIVIALLY_HAD;
  return 1;
}


/* Returns false if the type is invalid, e.g. a union with fields that
are not trivially copyable and movable, or a class type that needs a
defined destructor, or a type that has multiply defined
move/copy/destroy operations. */
int check_typeexpr_traits(struct checkstate *cs,
                          /* a is a concrete type. */
                          struct ast_typeexpr *a,
                          struct exprscope *also_typecheck,
                          struct typeexpr_traits *out) {
  switch (a->tag) {
  case AST_TYPEEXPR_NAME: {
    struct typeexpr_trait_instantiations insts_discard;
    return check_typeexpr_name_traits(cs, a, also_typecheck, out, &insts_discard,
                                      NULL, NULL);
  } break;
  case AST_TYPEEXPR_APP: {
    struct typeexpr_trait_instantiations insts_discard;
    return check_typeexpr_app_traits(cs, a, also_typecheck, out, &insts_discard,
                                     NULL, NULL);
  } break;
  case AST_TYPEEXPR_STRUCTE:
    return check_typeexpr_structe_traits(cs, a, also_typecheck, out);
  case AST_TYPEEXPR_UNIONE:
    return check_typeexpr_unione_traits(cs, a, also_typecheck, out);
  case AST_TYPEEXPR_ARRAY:
    return check_typeexpr_traits(cs, a->u.arraytype.param, also_typecheck, out);
  case AST_TYPEEXPR_UNKNOWN: {
    METERR(cs, *ast_typeexpr_meta(a), "Incomplete type in bad place.%s", "\n");
    return 0;
  } break;
  case AST_TYPEEXPR_NUMERIC: {
    METERR(cs, *ast_typeexpr_meta(a), "Incomplete numeric type in bad place.%s", "\n");
    return 0;
  }
  default:
    UNREACHABLE();
  }
}

int check_enumspec_traits(struct checkstate *cs,
                          /* a is a concrete enumspec. */
                          struct ast_enumspec *a,
                          struct exprscope *also_typecheck,
                          struct typeexpr_traits *out) {
  int ret = check_typeexpr_structe_enumspec_fields_traits(
      cs, a->enumfields, a->enumfields_count, also_typecheck, out);
  if (ret) {
    out->inittible = TYPEEXPR_TRAIT_TRIVIALLY_HAD;
  }
  return ret;
}



int check_generics_shadowing(struct checkstate *cs,
                             struct ast_generics *a) {
  if (!a->has_type_params) {
    return 1;
  }

  for (size_t i = 0, e = a->params.count; i < e; i++) {
    ident_value name = a->params.ptr[i].value;
    for (size_t j = 0; j < i; j++) {
      if (name == a->params.ptr[j].value) {
        METERR(cs, a->meta, "duplicate param names %.*s within same generics list.\n",
               IM_P(cs->im, name));
        return 0;
      }
    }

    if (name_table_shadowed(&cs->nt, name)) {
      METERR(cs, a->meta, "generics list shadows global name %.*s.\n",
             IM_P(cs->im, name));
      return 0;
    }
  }

  return 1;
}

int check_deftype(struct checkstate *cs, struct deftype_entry *ent) {
  CHECK_DBG("check_deftype\n");
  if (ent->has_been_checked) {
    return 1;
  }

  /* Can't be primitive, because they all have has_been_checked be true. */
  CHECK(!ent->is_primitive);

  struct ast_deftype *a = ent->deftype;
  CHECK(a);  /* Must be non-null, because ent->is_primitive is false. */

  if (ent->is_being_checked) {
    METERR(cs, a->meta, "deftype %.*s recursively held.\n",
           IM_P(cs->im, ent->name));
    return 0;
  }

  deftype_entry_mark_is_being_checked(ent);

  /* We know there's no clashes with a->name and the _arity_ of a->generics. */
  if (!check_generics_shadowing(cs, &a->generics)) {
    return 0;
  }

  switch (a->rhs.tag) {
  case AST_DEFTYPE_RHS_TYPE:
    if (!check_typeexpr(cs, &a->generics, &a->rhs.u.type, ent)) {
      return 0;
    }
    break;
  case AST_DEFTYPE_RHS_ENUMSPEC:
    if (!check_enumspec(cs, &a->generics, &a->rhs.u.enumspec, ent)) {
      return 0;
    }
    break;
  default:
    UNREACHABLE();
  }

  deftype_entry_mark_has_been_checked(ent);
  return 1;
}

enum static_computation {
  STATIC_COMPUTATION_NO,
  STATIC_COMPUTATION_YES,
};

struct varpair {
  struct ast_vardecl *decl;
};

void varpair_destroy(struct varpair *v) {
  /* Do nothing substantial.  Wipe for safety? */
  v->decl = NULL;
}

GEN_SLICE_HDR(varpair, struct varpair);
GEN_SLICE_IMPL(varpair, struct varpair);

struct exprscope {
  struct checkstate *cs;
  struct ast_generics *generics;
  struct ast_typeexpr *generics_substitutions;
  /* 0 if !generics->has_type_params; otherwise, equal to
  generics->params_count. */
  size_t generics_substitutions_count;

  struct defclass_ident *accessible;
  size_t accessible_count;

  /* "YES" if the expr must be statically computable. */
  enum static_computation computation;
  /* The def_entry for this expr, maybe.  We record static_referents
  that we see, if this is a statically-computed expression.  It would
  be null for expressions that aren't statically computed for a
  top-level def. */
  struct def_entry *entry_or_null;

  /* A stack of variables that are in scope. */
  struct varpair_slice vars;

  /* A counter for uniquely numbering temporaries. */
  size_t temp_counter;
};

void exprscope_init(struct exprscope *es,
                    struct checkstate *cs,
                    struct ast_generics *generics,
                    struct ast_typeexpr *generics_substitutions,
                    size_t generics_substitutions_count,
                    struct defclass_ident *accessible,
                    size_t accessible_count,
                    enum static_computation computation,
                    struct def_entry *entry_or_null) {
  CHECK(generics->params.count == (generics->has_type_params ?
                                   generics_substitutions_count : 0));
  es->cs = cs;
  es->generics = generics;
  es->generics_substitutions = generics_substitutions;
  es->generics_substitutions_count = generics_substitutions_count;
  es->accessible = accessible;
  es->accessible_count = accessible_count;
  es->computation = computation;
  es->entry_or_null = entry_or_null;
  es->vars = varpair_slice_initializer();
  es->temp_counter = 0;
}

void exprscope_destroy(struct exprscope *es) {
  es->cs = NULL;
  es->generics = NULL;
  es->generics_substitutions = NULL;
  es->generics_substitutions_count = 0;
  es->accessible = NULL;
  es->accessible_count = 0;
  es->computation = STATIC_COMPUTATION_YES;
  varpair_slice_destroy_prim(&es->vars);
  es->temp_counter = SIZE_MAX;
}

size_t exprscope_temptag(struct exprscope *es) {
  /* Notably, never returns zero. */
  es->temp_counter = size_add(es->temp_counter, 1);
  return es->temp_counter;
}

void exprscope_note_static_reference(struct exprscope *es,
                                     struct def_entry *ent) {
  if (es->computation == STATIC_COMPUTATION_YES && es->entry_or_null) {
    def_entry_note_static_reference(es->entry_or_null, ent);
  }
}

int check_var_shadowing(struct exprscope *es, struct ast_ident *name) {
  for (size_t i = 0, e = es->vars.count; i < e; i++) {
    if (es->vars.ptr[i].decl->name.value == name->value) {
      METERR(es->cs, name->meta, "Variable name %.*s shadows local.\n",
             IM_P(es->cs->im, name->value));
      return 0;
    }
  }

  {
    size_t which_generic;
    if (generics_lookup_name(es->generics, name->value, &which_generic)) {
      METERR(es->cs, name->meta, "Variable name %.*s shadows template parameter, which is gauche.\n",
             IM_P(es->cs->im, name->value));
      return 0;
    }
  }

  return 1;
}

int exprscope_push_var(struct exprscope *es, struct ast_vardecl *var) {
  if (!check_var_shadowing(es, &var->name)) {
    return 0;
  }

  struct varpair pair;
  pair.decl = var;
  varpair_slice_push(&es->vars, pair);

  return 1;
}

void exprscope_pop_var(struct exprscope *es) {
  varpair_slice_pop(&es->vars, varpair_destroy);
}

int help_unify_directionally(struct identmap *im,
                             int exact,
                             struct ast_typeexpr *partial_type,
                             struct ast_typeexpr *complete_type);

int help_unify_fields_directionally(struct identmap *im,
                                    int exact,
                                    struct ast_vardecl *partial_fields,
                                    size_t partial_fields_count,
                                    struct ast_vardecl *complete_fields,
                                    size_t complete_fields_count) {
  if (partial_fields_count != complete_fields_count) {
    return 0;
  }

  for (size_t i = 0; i < partial_fields_count; i++) {
    if (partial_fields[i].name.value != complete_fields[i].name.value
        || !help_unify_directionally(im, exact,
                                     &partial_fields[i].type,
                                     &complete_fields[i].type)) {
      return 0;
    }
  }

  return 1;
}

int help_unify_directionally(struct identmap *im,
                             int exact,
                             struct ast_typeexpr *partial_type,
                             struct ast_typeexpr *complete_type) {
  CHECK(complete_type->tag != AST_TYPEEXPR_UNKNOWN);
  if (partial_type->tag == AST_TYPEEXPR_UNKNOWN) {
    CHECK(!exact);
    return 1;
  }

  if (partial_type->tag == AST_TYPEEXPR_NUMERIC) {
    CHECK(!exact);
    return is_numeric_type(im, complete_type);
  }

  if (partial_type->tag != complete_type->tag) {
    return 0;
  }

  switch (partial_type->tag) {
  case AST_TYPEEXPR_NAME: {
    return partial_type->u.name.value == complete_type->u.name.value;
  } break;
  case AST_TYPEEXPR_APP: {
    struct ast_typeapp *p_app = &partial_type->u.app;
    struct ast_typeapp *c_app = &complete_type->u.app;
    if (p_app->name.value != c_app->name.value
        || p_app->params.count != c_app->params.count) {
      return 0;
    }
    for (size_t i = 0, e = p_app->params.count; i < e; i++) {
      if (!help_unify_directionally(im, exact, &p_app->params.ptr[i], &c_app->params.ptr[i])) {
        return 0;
      }
    }
    return 1;
  } break;
  case AST_TYPEEXPR_STRUCTE:
    return help_unify_fields_directionally(im, exact, partial_type->u.structe.fields,
                                           partial_type->u.structe.fields_count,
                                           complete_type->u.structe.fields,
                                           complete_type->u.structe.fields_count);
  case AST_TYPEEXPR_UNIONE:
    return help_unify_fields_directionally(im, exact, partial_type->u.unione.fields,
                                           partial_type->u.unione.fields_count,
                                           complete_type->u.unione.fields,
                                           complete_type->u.unione.fields_count);
  case AST_TYPEEXPR_ARRAY: {
    if (unsafe_numeric_literal_u32(&partial_type->u.arraytype.number)
        != unsafe_numeric_literal_u32(&complete_type->u.arraytype.number)) {
      return 0;
    }
    return help_unify_directionally(im, exact, partial_type->u.arraytype.param,
                                    complete_type->u.arraytype.param);
  } break;
  default:
    UNREACHABLE();
  }
}

int unify_directionally(struct identmap *im, struct ast_typeexpr *a,
                        struct ast_typeexpr *b) {
  return help_unify_directionally(im, 0, a, b);
}

int exact_typeexprs_equal(struct identmap *im, struct ast_typeexpr *a,
                          struct ast_typeexpr *b) {
  return help_unify_directionally(im, 1, a, b);
}

int is_accessible(struct exprscope *es, struct defclass_ident accessee_privacy_scope) {
  for (size_t i = 0, e = es->accessible_count; i < e; i++) {
    if (es->accessible[i].name == accessee_privacy_scope.name
        && es->accessible[i].arity.value == accessee_privacy_scope.arity.value) {
      return 1;
    }
  }
  return 0;
}

int deftype_is_accessible(struct exprscope *es, struct ast_deftype *deftype) {
  if (deftype->disposition == AST_DEFTYPE_NOT_CLASS) {
    return 1;
  }

  struct defclass_ident privacy_scope;
  privacy_scope.name = deftype->name.value;
  privacy_scope.arity = params_arity(&deftype->generics);
  return is_accessible(es, privacy_scope);
}

enum allow_incomplete {
  ALLOW_INCOMPLETE_NO,
  ALLOW_INCOMPLETE_YES,
};

int check_expr(struct exprscope *es, struct ast_expr *x,
               struct ast_typeexpr *partial_type);

int check_expr_ai(struct exprscope *es,
                  enum allow_incomplete ai,
                  struct ast_expr *x,
                  struct ast_typeexpr *partial_type);

int lookup_global_maybe_typecheck(
    struct checkstate *cs,
    struct exprscope *also_maybe_typecheck,
    struct ast_name_expr *name,
    struct ast_typeexpr *partial_type,
    enum report_mode report_mode,
    enum match_result *match_result_out,
    struct ast_typeexpr *out,
    int *is_lvalue_out,
    struct def_instantiation **inst_out) {
  struct ast_typeexpr unified;
  struct def_entry *ent;
  struct def_instantiation *inst;
  enum match_result res = name_table_match_def(
      cs,
      &cs->nt,
      &name->ident,
      name->has_params ? name->params.ptr : NULL,
      name->has_params ? name->params.count : 0,
      partial_type,
      report_mode,
      &unified,
      &ent,
      &inst);
  *match_result_out = res;
  if (res != MATCH_SUCCESS) {
    return 0;
  }

  if (!also_maybe_typecheck) {
    *out = unified;
    /* Right now, there are no global variables. */
    *is_lvalue_out = 0;
    *inst_out = inst;
    return 1;
  }

  for (size_t i = 0, e = ent->private_to_count; i < e; i++) {
    if (!is_accessible(also_maybe_typecheck, ent->private_to[i])) {
      METERR(cs, name->meta, "Access denied.%s", "\n");
      goto fail_unified;
    }
  }

  exprscope_note_static_reference(also_maybe_typecheck, ent);

  if (ent->is_primitive) {
    /* We want to force typechecking of do_copy, do_init, do_move,
    do_destroy instantiations to happen, if real ones are going to get
    called here.  (This is real ghetto shit, yeah, because we don't
    then generate info for build.c to use, it re-figures out all this
    shit all over again.) */

    /* TODO: avoid this string duping. */
    if (name->ident.value == cs->cm.copy
        || name->ident.value == cs->cm.move
        || name->ident.value == cs->cm.destroy
        || name->ident.value == cs->cm.init) {
      CHECK(unified.tag == AST_TYPEEXPR_APP
            && (unified.u.app.params.count == 2
                || unified.u.app.params.count == 3));
      struct ast_typeexpr *optype;
      if (!view_ptr_target(&cs->cm, &unified.u.app.params.ptr[0], &optype)) {
        CRASH("Expected copy/move/init/destroy lookup to match pointer type.\n");
      }

      struct typeexpr_traits traits;
      if (!check_typeexpr_traits(cs, optype, also_maybe_typecheck, &traits)) {
        goto fail_unified;
      }

      if (name->ident.value == cs->cm.copy) {
        if (traits.copyable == TYPEEXPR_TRAIT_LACKED) {
          METERR(cs, name->meta, "Copy trait lacked.%s", "\n");
          goto fail_unified;
        }
      } else if (name->ident.value == cs->cm.move) {
        if (traits.movable == TYPEEXPR_TRAIT_LACKED) {
          METERR(cs, name->meta, "Copy trait lacked.%s", "\n");
          goto fail_unified;
        }
      } else if (name->ident.value == cs->cm.init) {
        if (traits.inittible == TYPEEXPR_TRAIT_LACKED) {
          METERR(cs, name->meta, "Init trait lacked.%s", "\n");
          goto fail_unified;
        }
      }
      // else: Everything is destroyable, that was already checked by
      // check_typeexpr_traits.
    }
  } else {
    if (!ent->is_extern && ent->generics.has_type_params
        && !inst->typecheck_started) {
      CHECK(ent->def);
      CHECK(!inst->annotated_rhs_computed);
      if (cs->template_instantiation_recursion_depth
          == MAX_TEMPLATE_INSTANTIATION_RECURSION_DEPTH) {
        METERR(cs, ent->def->meta, "Max template instantiation recursion depth exceeded.%s", "\n");
        goto fail_unified;
      }

      cs->template_instantiation_recursion_depth++;

      inst->typecheck_started = 1;
      struct exprscope scope;
      exprscope_init(&scope, cs,
                     &ent->def->generics,
                     inst->substitutions,
                     inst->substitutions_count,
                     ent->accessible,
                     ent->accessible_count,
                     STATIC_COMPUTATION_YES,
                     ent);

      struct ast_expr annotated_rhs;
      ast_expr_init_copy(&annotated_rhs, &ent->def->rhs);
      if (!check_expr(&scope, &annotated_rhs, &unified)) {
        ast_expr_destroy(&annotated_rhs);
        exprscope_destroy(&scope);
        cs->template_instantiation_recursion_depth--;
        METERR(cs, name->meta, "... when instantiating '%.*s'.\n", IM_P(cs->im, ent->name));
        goto fail_unified;
      }

      /* We just assume there's no temporaries, no constructors or
      destructors, because the expression was statically computable. */

      di_set_annotated_rhs(inst, annotated_rhs);

      exprscope_destroy(&scope);
      cs->template_instantiation_recursion_depth--;
    }
  }

  *out = unified;
  /* Right now, there are no global variables. */
  *is_lvalue_out = 0;
  *inst_out = inst;
  return 1;
 fail_unified:
  ast_typeexpr_destroy(&unified);
  return 0;
}

int exprscope_lookup_name(struct exprscope *es,
                          struct ast_name_expr *name,
                          struct ast_typeexpr *partial_type,
                          struct ast_typeexpr *out,
                          int *is_lvalue_out,
                          struct def_instantiation **inst_or_null_out,
                          int report_multi_match,
                          enum match_result *match_result_out) {
  if (!name->has_params) {
    for (size_t i = es->vars.count; i > 0; ) {
      i--;
      struct ast_vardecl *decl = es->vars.ptr[i].decl;
      if (decl->name.value != name->ident.value) {
        continue;
      }

      *match_result_out = MATCH_SUCCESS;

      if (!unify_directionally(es->cs->im, partial_type, &decl->type)) {
        METERR(es->cs, name->meta, "Type mismatch for vardecl %.*s lookup.\n",
               IM_P(es->cs->im, name->ident.value));
        return 0;
      }

      ast_typeexpr_init_copy(out, &decl->type);
      *is_lvalue_out = 1;
      *inst_or_null_out = NULL;  /* NULL because it's a local. */
      return 1;
    }
  }

  /* inst_or_null_out gets initialized to a non-NULL value. */
  return lookup_global_maybe_typecheck(
      es->cs, es, name, partial_type,
      REPORT_MODE_NO_MATCH | (report_multi_match ? REPORT_MODE_MULTI_MATCH : 0),
      match_result_out,
      out, is_lvalue_out, inst_or_null_out);
}

void do_replace_generics_in_fields(struct ast_generics *generics,
                                   struct ast_typeexpr *generics_substitutions,
                                   size_t generics_substitutions_count,
                                   struct ast_vardecl *fields,
                                   size_t fields_count,
                                   struct ast_vardecl **fields_out,
                                   size_t *fields_count_out) {
  struct ast_vardecl *f = malloc_mul(sizeof(*f), fields_count);
  for (size_t i = 0; i < fields_count; i++) {
    struct ast_ident name;
    ast_ident_init_copy(&name, &fields[i].name);
    struct ast_typeexpr type;
    do_replace_generics(generics,
                        generics_substitutions,
                        generics_substitutions_count,
                        &fields[i].type,
                        &type);
    ast_vardecl_init(&f[i], ast_meta_make_copy(&fields[i].meta),
                     name, type);
  }
  *fields_out = f;
  *fields_count_out = fields_count;
}

void do_replace_generics(struct ast_generics *generics,
                         struct ast_typeexpr *generics_substitutions,
                         size_t generics_substitutions_count,
                         struct ast_typeexpr *a,
                         struct ast_typeexpr *out) {
  CHECK(generics->has_type_params);
  CHECK(generics->params.count == generics_substitutions_count);
  switch (a->tag) {
  case AST_TYPEEXPR_NAME: {
    size_t which_generic;
    if (generics_lookup_name(generics, a->u.name.value, &which_generic)) {
      ast_typeexpr_init_copy(out, &generics_substitutions[which_generic]);
    } else {
      ast_typeexpr_init_copy(out, a);
    }
  } break;
  case AST_TYPEEXPR_APP: {
    struct ast_typeapp *app = &a->u.app;
    size_t params_count = app->params.count;
    struct ast_typeexpr_array params = ast_typeexpr_array_malloc(params_count);

    for (size_t i = 0, e = params_count; i < e; i++) {
      do_replace_generics(generics, generics_substitutions,
                          generics_substitutions_count,
                          &app->params.ptr[i], &params.ptr[i]);
    }

    struct ast_ident name;
    ast_ident_init_copy(&name, &app->name);

    out->tag = AST_TYPEEXPR_APP;
    ast_typeapp_init(&out->u.app, ast_meta_make_copy(&a->u.app.meta),
                     name, params);
  } break;
  case AST_TYPEEXPR_STRUCTE: {
    struct ast_vardecl *fields;
    size_t fields_count;
    do_replace_generics_in_fields(generics, generics_substitutions,
                                  generics_substitutions_count,
                                  a->u.structe.fields,
                                  a->u.structe.fields_count,
                                  &fields, &fields_count);
    out->tag = AST_TYPEEXPR_STRUCTE;
    ast_structe_init(&out->u.structe, ast_meta_make_copy(&a->u.structe.meta),
                     fields, fields_count);
  } break;
  case AST_TYPEEXPR_UNIONE: {
    struct ast_vardecl *fields;
    size_t fields_count;
    do_replace_generics_in_fields(generics, generics_substitutions,
                                  generics_substitutions_count,
                                  a->u.unione.fields, a->u.unione.fields_count,
                                  &fields, &fields_count);
    out->tag = AST_TYPEEXPR_UNIONE;
    ast_unione_init(&out->u.unione, ast_meta_make_copy(&a->u.unione.meta),
                    fields, fields_count);
  } break;
  case AST_TYPEEXPR_ARRAY: {
    struct ast_typeexpr param;
    do_replace_generics(generics, generics_substitutions,
                        generics_substitutions_count,
                        a->u.arraytype.param, &param);
    out->tag = AST_TYPEEXPR_ARRAY;
    struct ast_numeric_literal number;
    ast_numeric_literal_init_copy(&number, &a->u.arraytype.number);
    ast_arraytype_init(&out->u.arraytype, ast_meta_make_copy(&a->u.arraytype.meta),
                       number, param);
  } break;
  case AST_TYPEEXPR_UNKNOWN: {
    out->tag = AST_TYPEEXPR_UNKNOWN;
    ast_unknown_init(&out->u.unknown, ast_meta_make_copy(&a->u.unknown.meta));
  } break;
  case AST_TYPEEXPR_NUMERIC: {
    out->tag = AST_TYPEEXPR_NUMERIC;
    ast_unknown_init(&out->u.numeric, ast_meta_make_copy(&a->u.numeric.meta));
  } break;
  default:
    UNREACHABLE();
  }
}

void replace_generics(struct exprscope *es,
                      struct ast_typeexpr *a,
                      struct ast_typeexpr *out) {
  if (!es->generics->has_type_params) {
    ast_typeexpr_init_copy(out, a);
  } else {
    do_replace_generics(es->generics, es->generics_substitutions, es->generics_substitutions_count, a, out);
  }
}

void do_replace_enumspec_generics(struct ast_generics *generics,
                                  struct ast_typeexpr *generics_substitutions,
                                  size_t generics_substitutions_count,
                                  struct ast_enumspec *a,
                                  struct ast_enumspec *out) {
  CHECK(generics->has_type_params);
  CHECK(generics->params.count == generics_substitutions_count);
  struct ast_vardecl *enumfields;
  size_t enumfields_count;
  do_replace_generics_in_fields(generics, generics_substitutions,
                                generics_substitutions_count,
                                a->enumfields,
                                a->enumfields_count,
                                &enumfields, &enumfields_count);
  ast_enumspec_init(out, enumfields, enumfields_count);
}

void do_replace_rhs_generics(struct ast_generics *generics,
                             struct ast_typeexpr *generics_substitutions,
                             size_t generics_substitutions_count,
                             struct ast_deftype_rhs *a,
                             struct ast_deftype_rhs *out) {
  switch (a->tag) {
  case AST_DEFTYPE_RHS_TYPE: {
    struct ast_typeexpr type;
    do_replace_generics(generics, generics_substitutions,
                        generics_substitutions_count, &a->u.type, &type);
    ast_deftype_rhs_init_type(out, type);
  } break;
  case AST_DEFTYPE_RHS_ENUMSPEC: {
    struct ast_enumspec enumspec;
    do_replace_enumspec_generics(generics, generics_substitutions,
                                 generics_substitutions_count,
                                 &a->u.enumspec, &enumspec);
    ast_deftype_rhs_init_enumspec(out, enumspec);
  } break;
  default:
    UNREACHABLE();
  }
}

struct ast_expr_info expr_info_typechecked_subobject(
    struct ast_typeexpr concrete_type,
    struct ast_expr_info *info) {
  CHECK(info->typechecked == AST_TYPECHECKED_YES);
  if (!info->temporary_exists) {
    return ast_expr_info_typechecked_no_temporary(info->is_lvalue, concrete_type);
  } else {
    struct ast_typeexpr temporary_type;
    ast_typeexpr_init_copy(&temporary_type, &info->temporary_type);
    return ast_expr_info_typechecked_temporary(
        info->is_lvalue,
        concrete_type,
        temporary_type,
        0,
        info->temptag);
  }
}

int compute_and_check_exprcatch(struct exprscope *es,
                                struct ast_expr *annotated_expr,
                                struct ast_exprcatch *out) {
  struct ast_expr_info *info = &annotated_expr->info;
  CHECK(info->typechecked == AST_TYPECHECKED_YES);
  if (info->temporary_exists) {
    if (info->whole_thing) {
      ast_exprcatch_init_annotated(out, AST_EXPRCATCH_IN_PLACE);
      return 1;
    } else {
      struct typeexpr_traits traits;
      if (!check_typeexpr_traits(es->cs, &info->temporary_type, es, &traits)) {
        return 0;
      }
      /* The temporary_type is thus destructable. */

      if (!check_typeexpr_traits(es->cs, ast_expr_type(annotated_expr), es, &traits)) {
        return 0;
      }

      /* Oof, there's no copy constructor for the type that we would copy. */
      if (traits.copyable == TYPEEXPR_TRAIT_LACKED) {
        METERR(es->cs, *ast_expr_ast_meta(annotated_expr),
               "Copy demanded of noncopyable type.%s", "\n");
        return 0;
      }
      ast_exprcatch_init_annotated(out, AST_EXPRCATCH_COPY_AND_DESTROY);
      return 1;
    }
  } else {
    struct typeexpr_traits traits;

    if (!check_typeexpr_traits(es->cs, ast_expr_type(annotated_expr), es, &traits)) {
      return 0;
    }

    /* Oof, there's no copy constructor for the type that we would copy. */
    if (traits.copyable == TYPEEXPR_TRAIT_LACKED) {
      METERR(es->cs, *ast_expr_ast_meta(annotated_expr),
             "Copy demanded of noncopyable type.%s", "\n");
      return 0;
    }
    ast_exprcatch_init_annotated(out, AST_EXPRCATCH_COPY);
    return 1;
  }
}

int check_funcall_args_firstcheck(struct exprscope *es,
                                  struct ast_exprcall *args,
                                  size_t args_count,
                                  int *an_arg_incomplete_out) {
  int ret = 0;
  struct ast_typeexpr local_partial = ast_unknown_garbage();
  int an_arg_incomplete = 0;
  for (size_t i = 0; i < args_count; i++) {
    if (!check_expr_ai(es, ALLOW_INCOMPLETE_YES, &args[i].expr, &local_partial)) {
      goto fail;
    }
    an_arg_incomplete |= ast_expr_incomplete(&args[i].expr);
  }

  *an_arg_incomplete_out = an_arg_incomplete;
  ret = 1;
 fail:
  ast_typeexpr_destroy(&local_partial);
  return ret;
}

int check_funcall_args_secondcheck(struct exprscope *es,
                                   struct ast_typeexpr *func_type,
                                   struct ast_exprcall *args,
                                   size_t args_count) {
  CHECK(typeexpr_is_func_type(es->cs->im, func_type));
  CHECK(func_type->u.app.params.count == size_add(args_count, 1));

  for (size_t i = 0; i < args_count; i++) {
    if (!check_expr(es, &args[i].expr, &func_type->u.app.params.ptr[i])) {
      return 0;
    }
  }

  return 1;
}

int check_funcall_funcexpr_ai(struct exprscope *es,
                              enum allow_incomplete ai,
                              struct ast_exprcall *args,
                              size_t args_count,
                              struct ast_typeexpr *partial_type,
                              struct ast_exprcall *func) {
  struct ast_typeexpr funcexpr;
  {
    size_t args_types_count = size_add(args_count, 1);
    struct ast_typeexpr_array args_types = ast_typeexpr_array_malloc(args_types_count);

    for (size_t i = 0; i < args_count; i++) {
      ast_typeexpr_init_copy(&args_types.ptr[i], ast_expr_partial_type(&args[i].expr));
    }
    ast_typeexpr_init_copy(&args_types.ptr[args_count], partial_type);

    funcexpr.tag = AST_TYPEEXPR_APP;
    ast_typeapp_init(&funcexpr.u.app, ast_meta_make_garbage(),
                     make_ast_ident(es->cs->cm.func),
                     args_types);
  }

  int ret = check_expr_ai(es, ai, &func->expr, &funcexpr);
  ast_typeexpr_destroy(&funcexpr);
  return ret;
}

int check_expr_funcall(struct exprscope *es,
                       enum allow_incomplete ai,
                       struct ast_expr *y,
                       struct ast_typeexpr *partial_type) {
  struct ast_funcall *x = &y->u.funcall;
  size_t args_count = x->args.count;
  int an_arg_incomplete;
  if (!check_funcall_args_firstcheck(es, x->args.ptr, args_count, &an_arg_incomplete)) {
    goto fail;
  }

  if (!check_funcall_funcexpr_ai(
          es, an_arg_incomplete ? ALLOW_INCOMPLETE_NO : ai,
          x->args.ptr, args_count, partial_type, x->func)) {
    goto fail;
  }

  if (ast_expr_incomplete(&x->func->expr)) {
    ast_expr_update(y, ast_expr_info_incomplete());
    CHECK(ai == ALLOW_INCOMPLETE_YES);
    return 1;
  }

  if (an_arg_incomplete) {
    struct ast_typeexpr *func_type = ast_expr_type(&x->func->expr);
    if (!check_funcall_args_secondcheck(es, func_type, x->args.ptr, args_count)) {
      goto fail;
    }
  }

  for (size_t i = 0; i < args_count; i++) {
    struct ast_exprcatch arg_exprcatch;
    if (!compute_and_check_exprcatch(es, &x->args.ptr[i].expr, &arg_exprcatch)) {
      goto fail;
    }

    ast_exprcall_annotate(&x->args.ptr[i], arg_exprcatch);
  }

  struct ast_exprcatch func_exprcatch;
  if (!compute_and_check_exprcatch(es, &x->func->expr, &func_exprcatch)) {
    goto fail;
  }
  ast_exprcall_annotate(x->func, func_exprcatch);

  struct ast_typeexpr return_type;
  copy_func_return_type(&es->cs->cm, ast_expr_type(&x->func->expr),
                        size_add(args_count, 1), &return_type);

  struct ast_typeexpr temporary_type;
  ast_typeexpr_init_copy(&temporary_type, &return_type);

  /* TODO: This'll end up being the wrong place to typecheck this
  temporary type. */
  struct typeexpr_traits discard;
  if (!check_typeexpr_traits(es->cs, &temporary_type, es, &discard)) {
    goto fail_cleanup_temporary_type;
  }

  struct ast_expr_info expr_info = ast_expr_info_typechecked_temporary(
      0, return_type, temporary_type, 1, exprscope_temptag(es));

  ast_expr_update(y, expr_info);

  return 1;
 fail_cleanup_temporary_type:
  ast_typeexpr_destroy(&temporary_type);
  ast_typeexpr_destroy(&return_type);
 fail:
  return 0;
}

struct bodystate {
  struct exprscope *es;
  struct ast_typeexpr *partial_type;
  int have_exact_return_type;
  struct ast_typeexpr exact_return_type;
};

void bodystate_init(struct bodystate *bs, struct exprscope *es,
                    struct ast_typeexpr *partial_type) {
  bs->es = es;
  bs->partial_type = partial_type;
  bs->have_exact_return_type = 0;
}

void bodystate_destroy(struct bodystate *bs) {
  bs->es = NULL;
  if (bs->have_exact_return_type) {
    ast_typeexpr_destroy(&bs->exact_return_type);
    bs->have_exact_return_type = 0;
  }
}

void free_ast_vardecl(struct ast_vardecl **p) {
  ast_vardecl_destroy(*p);
  free(*p);
  *p = NULL;
}

/* Tells how a bracebody or statement falls through. */
enum fallthrough {
  /* Says a statement/bracebody never exits "out the bottom" -- it
  exits with a goto or return. */
  FALLTHROUGH_NEVER,
  /* Says a statement/bracebody could exit "out the bottom" only if
  you can enter "from the top." */
  FALLTHROUGH_FROMTHETOP,
};

enum fallthrough max_fallthrough(enum fallthrough x, enum fallthrough y) {
  return x < y ? y : x;
}

enum fallthrough compose_fallthrough(enum fallthrough top_reachability,
                                     enum fallthrough statement_fallthrough) {
  return top_reachability < statement_fallthrough
    ? top_reachability : statement_fallthrough;
}

int is_enum_type(struct checkstate *cs,
                 struct ast_typeexpr *concrete_type,
                 struct ast_enumspec *concrete_enumspec_out) {
  switch (concrete_type->tag) {
  case AST_TYPEEXPR_NAME: {
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(&cs->nt, concrete_type->u.name.value,
                                   no_param_list_arity(),
                                   &ent)) {
      return 0;
    }

    if (ent->is_primitive) {
      return 0;
    }

    struct ast_deftype *deftype = ent->deftype;
    CHECK(deftype);
    switch (deftype->rhs.tag) {
    case AST_DEFTYPE_RHS_TYPE:
      return 0;
    case AST_DEFTYPE_RHS_ENUMSPEC: {
      ast_enumspec_init_copy(concrete_enumspec_out, &deftype->rhs.u.enumspec);
      return 1;
    } break;
    default:
      UNREACHABLE();
    }
  } break;
  case AST_TYPEEXPR_APP: {
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(&cs->nt, concrete_type->u.app.name.value,
                                   param_list_arity(concrete_type->u.app.params.count),
                                   &ent)) {
      return 0;
    }

    if (ent->is_primitive) {
      return 0;
    }

    struct ast_deftype *deftype = ent->deftype;
    CHECK(deftype);
    switch (deftype->rhs.tag) {
    case AST_DEFTYPE_RHS_TYPE:
      return 0;
    case AST_DEFTYPE_RHS_ENUMSPEC: {
      do_replace_enumspec_generics(&deftype->generics,
                                   concrete_type->u.app.params.ptr,
                                   concrete_type->u.app.params.count,
                                   &deftype->rhs.u.enumspec,
                                   concrete_enumspec_out);
      return 1;
    } break;
    default:
      UNREACHABLE();
    }
  } break;
  case AST_TYPEEXPR_STRUCTE:
  case AST_TYPEEXPR_UNIONE:
  case AST_TYPEEXPR_ARRAY:
    return 0;
  default:
    UNREACHABLE();
  }
}


int check_expr_bracebody(struct bodystate *bs,
                         struct ast_bracebody *x,
                         enum fallthrough *fallthrough_out);

/* This type is not wrapped up tight -- check_swartch touches its
fields directly. */
struct swartchspec {
  int is_ptr;
  struct ast_enumspec concrete_enumspec;
};

void swartchspec_destroy(struct swartchspec *spec) {
  spec->is_ptr = 0;
  ast_enumspec_destroy(&spec->concrete_enumspec);
}

int check_swartch(struct exprscope *es, struct ast_expr *swartch,
                  struct swartchspec *out) {
  struct ast_typeexpr partial = ast_unknown_garbage();
  int swartch_result = check_expr(es, swartch, &partial);
  ast_typeexpr_destroy(&partial);
  if (!swartch_result) {
    return 0;
  }

  if (is_enum_type(es->cs, ast_expr_type(swartch), &out->concrete_enumspec)) {
    out->is_ptr = 0;
    return 1;
  } else {
    struct ast_typeexpr *ptr_target;
    if (view_ptr_target(&es->cs->cm, ast_expr_type(swartch), &ptr_target)) {
      if (is_enum_type(es->cs, ptr_target, &out->concrete_enumspec)) {
        out->is_ptr = 1;
        return 1;
      } else {
        METERR(es->cs, *ast_expr_ast_meta(swartch),
               "Pattern matching over pointer to non-enum type.%s", "\n");
        return 0;
      }
    } else {
      METERR(es->cs, *ast_expr_ast_meta(swartch),
             "Pattern matching over non-enum, non-pointer type.%s", "\n");
      return 0;
    }
  }
}

struct varwind {
  int has_decl;
  struct ast_vardecl *replaced_decl_;
};

void varwind_destroy(struct varwind *a) {
  if (a->has_decl) {
    free_ast_vardecl(&a->replaced_decl_);
    a->has_decl = 0;
  }
}

void varwind_unwind_destroy(struct exprscope *es, struct varwind *a) {
  if (a->has_decl) {
    exprscope_pop_var(es);
    varwind_destroy(a);
  }
}

int check_constructor(struct exprscope *es,
                      struct swartchspec *spec,
                      struct ast_constructor_pattern *constructor,
                      struct varwind *out) {
  if (spec->is_ptr != constructor->addressof_constructor) {
    METERR(es->cs, constructor->meta, "Constructor pointeriness mismatches swartch.%s", "\n");
    return 0;
  }

  size_t constructor_num = SIZE_MAX;  /* Initialized to appease cl. */
  int constructor_found = 0;
  for (size_t i = 0, e = spec->concrete_enumspec.enumfields_count; i < e; i++) {
    if (spec->concrete_enumspec.enumfields[i].name.value
        == constructor->constructor_name.value) {
      constructor_found = 1;
      constructor_num = i;
      break;
    }
  }

  if (!constructor_found) {
    METERR(es->cs, constructor->meta, "Unrecognized constructor '%.*s' in switch case.\n", IM_P(es->cs->im, constructor->constructor_name.value));
    METMORE("Valid constructors are: ");
    for (size_t i = 0, e = spec->concrete_enumspec.enumfields_count; i < e; i++) {
      METMORE("%s'%.*s'",
              i ? ", " : "",
              IM_P(es->cs->im, spec->concrete_enumspec.enumfields[i].name.value));
    }
    METMORE("\n");
    return 0;
  }

  struct typeexpr_traits discard;
  if (!check_typeexpr_traits(
          es->cs, &spec->concrete_enumspec.enumfields[constructor_num].type,
          es, &discard)) {
    return 0;
  }

  struct ast_vardecl *replaced_decl = NULL;
  if (constructor->has_decl) {
    struct ast_typeexpr replaced_incomplete_type;
    replace_generics(es, &constructor->decl_.type, &replaced_incomplete_type);

    int unify_res = unify_directionally(
        es->cs->im, &replaced_incomplete_type,
        &spec->concrete_enumspec.enumfields[constructor_num].type);
    ast_typeexpr_destroy(&replaced_incomplete_type);
    if (!unify_res) {
      METERR(es->cs, constructor->meta, "Case decl type mismatch.%s", "\n");
      return 0;
    }

    struct ast_ident replaced_decl_name;
    ast_ident_init_copy(&replaced_decl_name, &constructor->decl_.name);
    struct ast_typeexpr concrete_type_copy;
    ast_typeexpr_init_copy(&concrete_type_copy,
                           &spec->concrete_enumspec.enumfields[constructor_num].type);
    replaced_decl = malloc(sizeof(*replaced_decl));
    CHECK(replaced_decl);
    ast_vardecl_init(replaced_decl, ast_meta_make_copy(&constructor->decl_.meta),
                     replaced_decl_name, concrete_type_copy);
  } else {
    struct ast_typeexpr void_type;
    init_name_type(&void_type, es->cs->cm.voide);
    int unify_res = unify_directionally(es->cs->im, &void_type, &spec->concrete_enumspec.enumfields[constructor_num].type);
    ast_typeexpr_destroy(&void_type);
    if (!unify_res) {
      METERR(es->cs, constructor->meta, "Decl-free case pattern used with non-void case.%s", "\n");
      return 0;
    }
  }

  if (constructor->has_decl) {
    if (!exprscope_push_var(es, replaced_decl)) {
      free_ast_vardecl(&replaced_decl);
      return 0;
    }

    struct ast_typeexpr concrete_type_copy;
    ast_typeexpr_init_copy(&concrete_type_copy,
                           &spec->concrete_enumspec.enumfields[constructor_num].type);
    ast_var_info_specify(&constructor->decl_.var_info, concrete_type_copy);
  }

  struct constructor_num cn;
  cn.value = constructor_num;
  ast_case_pattern_info_specify(&constructor->info, cn);

  out->has_decl = constructor->has_decl;
  out->replaced_decl_ = replaced_decl;
  return 1;
}

struct maybe_varwind {
  int has_varwind;
  struct varwind varwind;
};

void maybe_varwind_destroy(struct maybe_varwind *a) {
  if (a->has_varwind) {
    varwind_destroy(&a->varwind);
  }
}

void maybe_varwind_unwind_destroy(struct exprscope *es, struct maybe_varwind *a) {
  if (a->has_varwind) {
    varwind_unwind_destroy(es, &a->varwind);
  }
}

int check_condition(struct bodystate *bs, struct ast_condition *a,
                    struct maybe_varwind *out) {
  switch (a->tag) {
  case AST_CONDITION_EXPR: {
    struct ast_typeexpr boolean;
    init_name_type(&boolean, bs->es->cs->cm.boole);

    int res = check_expr(bs->es, a->u.expr, &boolean);
    ast_typeexpr_destroy(&boolean);
    if (!res) {
      return 0;
    }
    out->has_varwind = 0;
    return 1;
  } break;
  case AST_CONDITION_PATTERN: {
    struct swartchspec spec;
    if (!check_swartch(bs->es, a->u.pa.rhs, &spec)) {
      return 0;
    }

    int res = check_constructor(bs->es, &spec, &a->u.pa.pattern, &out->varwind);
    swartchspec_destroy(&spec);
    if (!res) {
      return 0;
    }
    out->has_varwind = 1;
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}

int check_statement(struct bodystate *bs,
                    struct ast_statement *s,
                    enum fallthrough *fallthrough_out,
                    struct ast_vardecl **vardecl_to_push_or_null_out) {
  enum fallthrough fallthrough;
  struct ast_vardecl *vardecl_to_push = NULL;

  switch (s->tag) {
  case AST_STATEMENT_EXPR: {
    struct ast_typeexpr anything = ast_unknown_garbage();
    int check_result = check_expr(bs->es, s->u.expr, &anything);
    ast_typeexpr_destroy(&anything);
    if (!check_result) {
      goto fail;
    }
    fallthrough = FALLTHROUGH_FROMTHETOP;
  } break;
  case AST_STATEMENT_RETURN: {
    int has_expr = s->u.return_statement.has_expr;
    struct ast_typeexpr void_type;
    struct ast_typeexpr *expr_type;
    if (has_expr) {
      if (!check_expr(bs->es, s->u.return_statement.expr, bs->partial_type)) {
        goto fail;
      }
      expr_type = ast_expr_type(s->u.return_statement.expr);
    } else {
      init_name_type(&void_type, bs->es->cs->cm.voide);
      expr_type = &void_type;
    }

    if (!bs->have_exact_return_type) {
      bs->have_exact_return_type = 1;
      if (has_expr) {
        ast_typeexpr_init_copy(&bs->exact_return_type, expr_type);
      } else {
        bs->exact_return_type = void_type;
      }
    } else {
      if (!exact_typeexprs_equal(bs->es->cs->im, &bs->exact_return_type, expr_type)) {
        METERR(bs->es->cs, s->u.return_statement.meta,
               "Return statements with conflicting return types.%s", "\n");
        if (!has_expr) {
          ast_typeexpr_destroy(&void_type);
        }
        goto fail;
      }
      if (!has_expr) {
        ast_typeexpr_destroy(&void_type);
      }
    }
    fallthrough = FALLTHROUGH_NEVER;
  } break;
  case AST_STATEMENT_VAR: {
    struct ast_typeexpr replaced_incomplete_type;
    replace_generics(bs->es, &s->u.var_statement.decl.type, &replaced_incomplete_type);

    struct ast_typeexpr concrete_type;

    int has_rhs = s->u.var_statement.has_rhs;
    if (has_rhs) {
      int check_result = check_expr(bs->es, &s->u.var_statement.rhs->expr,
                                    &replaced_incomplete_type);
      ast_typeexpr_destroy(&replaced_incomplete_type);
      if (!check_result) {
        goto fail;
      }
      struct ast_exprcatch rhs_exprcatch;
      if (!compute_and_check_exprcatch(bs->es, &s->u.var_statement.rhs->expr,
                                       &rhs_exprcatch)) {
        goto fail;
      }
      ast_exprcall_annotate(s->u.var_statement.rhs, rhs_exprcatch);
      ast_typeexpr_init_copy(&concrete_type,
                             ast_expr_type(&s->u.var_statement.rhs->expr));
    } else {
      if (!is_complete(&replaced_incomplete_type)) {
        METERR(bs->es->cs, s->u.var_statement.decl.meta, "Var statement without initializer has incomplete type.%s", "\n");
        goto fail;
      }

      /* TODO: Initialization info must be computed. */
      concrete_type = replaced_incomplete_type;
    }

    struct typeexpr_traits traits;
    if (!check_typeexpr_traits(bs->es->cs, &concrete_type, bs->es, &traits)) {
      ast_typeexpr_destroy(&concrete_type);
      goto fail;
    }

    if (!has_rhs && traits.inittible == TYPEEXPR_TRAIT_LACKED) {
        METERR(bs->es->cs, s->u.var_statement.meta, "Variable %.*s not default-initializable.\n",
               IM_P(bs->es->cs->im, s->u.var_statement.decl.name.value));
        ast_typeexpr_destroy(&concrete_type);
        goto fail;
    }

    struct ast_vardecl *replaced_decl = malloc(sizeof(*replaced_decl));
    {
      struct ast_ident name;
      ast_ident_init_copy(&name, &s->u.var_statement.decl.name);

      struct ast_typeexpr concrete_type_copy;
      ast_typeexpr_init_copy(&concrete_type_copy, &concrete_type);

      CHECK(replaced_decl);
      ast_vardecl_init(replaced_decl, ast_meta_make_copy(&s->u.var_statement.decl.meta),
                       name, concrete_type_copy);
    }

    if (!exprscope_push_var(bs->es, replaced_decl)) {
      free_ast_vardecl(&replaced_decl);
      ast_typeexpr_destroy(&concrete_type);
      goto fail;
    }

    vardecl_to_push = replaced_decl;
    ast_var_info_specify(&s->u.var_statement.decl.var_info, concrete_type);
    fallthrough = FALLTHROUGH_FROMTHETOP;
  } break;
  case AST_STATEMENT_IFTHEN: {
    struct maybe_varwind varwind;
    if (!check_condition(bs, &s->u.ifthen_statement.condition, &varwind)) {
      goto fail;
    }

    enum fallthrough body_fallthrough;
    if (!check_expr_bracebody(bs, &s->u.ifthen_statement.body,
                              &body_fallthrough)) {
      maybe_varwind_destroy(&varwind);
      goto fail;
    }

    maybe_varwind_unwind_destroy(bs->es, &varwind);

    fallthrough = max_fallthrough(FALLTHROUGH_FROMTHETOP, body_fallthrough);
  } break;
  case AST_STATEMENT_IFTHENELSE: {
    struct maybe_varwind varwind;
    if (!check_condition(bs, &s->u.ifthenelse_statement.condition, &varwind)) {
      goto fail;
    }

    enum fallthrough thenbody_fallthrough;
    if (!check_expr_bracebody(bs, &s->u.ifthenelse_statement.thenbody,
                              &thenbody_fallthrough)) {
      maybe_varwind_destroy(&varwind);
      goto fail;
    }

    maybe_varwind_unwind_destroy(bs->es, &varwind);

    enum fallthrough elsebody_fallthrough;
    if (!check_expr_bracebody(bs, &s->u.ifthenelse_statement.elsebody,
                              &elsebody_fallthrough)) {
      goto fail;
    }

    fallthrough = max_fallthrough(thenbody_fallthrough, elsebody_fallthrough);
  } break;
  case AST_STATEMENT_WHILE: {
    struct maybe_varwind varwind;
    if (!check_condition(bs, &s->u.while_statement.condition, &varwind)) {
      goto fail;
    }

    enum fallthrough body_fallthrough;
    if (!check_expr_bracebody(bs, &s->u.while_statement.body,
                              &body_fallthrough)) {
      maybe_varwind_destroy(&varwind);
      goto fail;
    }

    maybe_varwind_unwind_destroy(bs->es, &varwind);

    fallthrough = max_fallthrough(FALLTHROUGH_FROMTHETOP, body_fallthrough);
  } break;
  case AST_STATEMENT_FOR: {
    struct ast_for_statement *fs = &s->u.for_statement;

    struct ast_vardecl *vardecl_to_push_or_null = NULL;
    int var_pushed = 0;
    if (fs->has_initializer) {
      CHECK(fs->initializer->tag == AST_STATEMENT_EXPR
            || fs->initializer->tag == AST_STATEMENT_VAR);
      enum fallthrough initializer_fallthrough;
      if (!check_statement(bs, fs->initializer, &initializer_fallthrough,
                           &vardecl_to_push_or_null)) {
        goto fail;
      }

      CHECK(initializer_fallthrough == FALLTHROUGH_FROMTHETOP);
      if (vardecl_to_push_or_null) {
        var_pushed = 1;
      }
    }

    if (fs->has_condition) {
      struct ast_typeexpr boolean;
      init_name_type(&boolean, bs->es->cs->cm.boole);

      int condition_result = check_expr(bs->es, fs->condition, &boolean);
      ast_typeexpr_destroy(&boolean);
      if (!condition_result) {
        goto fail;
      }
    }

    if (fs->has_increment) {
      struct ast_typeexpr anything = ast_unknown_garbage();
      int increment_result = check_expr(bs->es, fs->increment, &anything);
      ast_typeexpr_destroy(&anything);
      if (!increment_result) {
        goto fail;
      }
    }

    enum fallthrough body_fallthrough;
    if (!check_expr_bracebody(bs, &fs->body, &body_fallthrough)) {
      goto fail;
    }

    if (fs->has_condition) {
      fallthrough = max_fallthrough(FALLTHROUGH_FROMTHETOP, body_fallthrough);
    } else {
      fallthrough = FALLTHROUGH_NEVER;
    }

    if (var_pushed) {
      exprscope_pop_var(bs->es);
      free_ast_vardecl(&vardecl_to_push_or_null);
    }
    CHECK(vardecl_to_push_or_null == NULL);

  } break;
  case AST_STATEMENT_SWITCH: {
    struct ast_switch_statement *ss = &s->u.switch_statement;
    struct swartchspec spec;
    if (!check_swartch(bs->es, ss->swartch, &spec)) {
      goto switch_fail;
    }

    fallthrough = FALLTHROUGH_NEVER;

    size_t i = 0;
    for (size_t e = ss->cased_statements_count; i < e; i++) {
      struct ast_cased_statement *cas = &ss->cased_statements[i];
      for (size_t j = 0; j < i; j++) {
        if (cas->pattern.is_default) {
          if (ss->cased_statements[j].pattern.is_default) {
            METERR(bs->es->cs, cas->meta, "Overlapping default switch cases.%s", "\n");
            goto switch_fail_spec;
          }
        } else {
          if (!ss->cased_statements[j].pattern.is_default
              && ss->cased_statements[j].pattern.u.constructor.constructor_name.value
              == cas->pattern.u.constructor.constructor_name.value) {
            METERR(bs->es->cs, cas->meta, "Overlapping (duplicate) switch cases.%s", "\n");
            goto switch_fail_spec;
          }
        }
      }

      enum fallthrough cas_fallthrough;
      if (cas->pattern.is_default) {
        if (!check_expr_bracebody(bs, &cas->body, &cas_fallthrough)) {
          goto switch_fail_spec;
        }
        struct constructor_num cn;
        cn.value = spec.concrete_enumspec.enumfields_count;
        ast_case_pattern_info_specify(&cas->pattern.u.default_pattern.info, cn);
      } else {
        struct ast_constructor_pattern *constructor = &cas->pattern.u.constructor;
        struct varwind varwind;
        if (!check_constructor(bs->es, &spec, constructor, &varwind)) {
          goto switch_fail_spec;
        }

        if (!check_expr_bracebody(bs, &cas->body, &cas_fallthrough)) {
          varwind_destroy(&varwind);
          goto switch_fail_spec;
        }

        varwind_unwind_destroy(bs->es, &varwind);
      }
      fallthrough = max_fallthrough(fallthrough, cas_fallthrough);
    }

    swartchspec_destroy(&spec);
    break;
  switch_fail_spec:
    swartchspec_destroy(&spec);
  switch_fail:
    goto fail;
  } break;
  default:
    UNREACHABLE();
  }

  *fallthrough_out = fallthrough;
  *vardecl_to_push_or_null_out = vardecl_to_push;
  return 1;
 fail:
  return 0;
}

/* fallthrough_out says if evaluating the bracebody could "fall off
the end", i.e. there isn't a return statement in every branch. */
int check_expr_bracebody(struct bodystate *bs,
                         struct ast_bracebody *x,
                         enum fallthrough *fallthrough_out) {
  struct ast_vardecl_ptr_slice vardecls_pushed = SLICE_INITIALIZER;
  int ret = 0;

  enum fallthrough reachable = FALLTHROUGH_FROMTHETOP;

  for (size_t i = 0, e = x->statements_count; i < e; i++) {
    struct ast_statement *s = &x->statements[i];
    enum fallthrough fallthrough;
    struct ast_vardecl *vardecl_to_push_or_null;
    if (!check_statement(bs, s, &fallthrough,
                         &vardecl_to_push_or_null)) {
      goto fail;
    }

    reachable = compose_fallthrough(reachable, fallthrough);
    if (vardecl_to_push_or_null) {
      ast_vardecl_ptr_slice_push(&vardecls_pushed,
                                 vardecl_to_push_or_null);
    }
  }

  *fallthrough_out = reachable;
  ret = 1;
 fail:
  for (size_t i = 0; i < vardecls_pushed.count; i++) {
    exprscope_pop_var(bs->es);
  }
  ast_vardecl_ptr_slice_destroy(&vardecls_pushed, free_ast_vardecl);
  return ret;
}

int check_expr_funcbody(struct exprscope *es,
                        struct ast_bracebody *x,
                        struct ast_typeexpr *partial_type,
                        struct ast_typeexpr *out) {
  int ret = 0;

  struct bodystate bs;
  bodystate_init(&bs, es, partial_type);

  enum fallthrough fallthrough;
  if (!check_expr_bracebody(&bs, x, &fallthrough)) {
    goto fail;
  }

  ident_value void_ident = es->cs->cm.voide;
  if (partial_type->tag == AST_TYPEEXPR_NAME
      && partial_type->u.name.value == void_ident) {
    if (!bs.have_exact_return_type) {
      bs.have_exact_return_type = 1;
      ast_typeexpr_init_copy(&bs.exact_return_type, partial_type);
    } else {
      CHECK(bs.exact_return_type.tag == AST_TYPEEXPR_NAME
            && bs.exact_return_type.u.name.value == void_ident);
    }
  } else {
    if (fallthrough != FALLTHROUGH_NEVER) {
      METERR(es->cs, x->meta, "not all control paths return a value.%s", "\n");
      goto fail;
    }
  }

  /* We should have an exact return type because we didn't fallthrough
  (or the type was void and have_exact_return_type has been set). */
  CHECK(bs.have_exact_return_type);

  struct typeexpr_traits discard;
  if (!check_typeexpr_traits(es->cs, &bs.exact_return_type, es, &discard)) {
    goto fail;
  }

  *out = bs.exact_return_type;
  bs.have_exact_return_type = 0;

  ret = 1;
 fail:
  bodystate_destroy(&bs);
  return ret;
}

int check_expr_lambda(struct exprscope *es,
                      struct ast_lambda *x,
                      struct ast_typeexpr *partial_type,
                      struct ast_typeexpr *out) {
  CHECK_DBG("check_expr_lambda\n");
  size_t func_params_count = x->params_count;
  size_t args_count = size_add(func_params_count, 1);

  struct ast_typeexpr funcexpr;
  {
    struct ast_typeexpr *args = malloc_mul(sizeof(*args), args_count);
    size_t i;
    for (i = 0; i < func_params_count; i++) {
      for (size_t j = 0; j < i; j++) {
        if (x->params[i].name.value == x->params[j].name.value) {
          METERR(es->cs, x->params[i].meta, "Duplicate lambda parameter name %.*s.\n",
                 IM_P(es->cs->im, x->params[i].name.value));
          goto fail_args_up_to_i;
        }
      }
      if (!check_var_shadowing(es, &x->params[i].name)) {
        goto fail_args_up_to_i;
      }

      if (!check_typeexpr(es->cs, es->generics, &x->params[i].type, NULL)) {
        goto fail_args_up_to_i;
      }

      replace_generics(es, &x->params[i].type, &args[i]);

      struct typeexpr_traits discard;
      if (!check_typeexpr_traits(es->cs, &args[i], es, &discard)) {
        ast_typeexpr_destroy(&args[i]);
        goto fail_args_up_to_i;
      }
    }

    if (0) {
    fail_args_up_to_i:
      SLICE_FREE(args, i, ast_typeexpr_destroy);
      return 0;
    }

    replace_generics(es, &x->return_type, &args[func_params_count]);

    funcexpr.tag = AST_TYPEEXPR_APP;
    ast_typeapp_init(&funcexpr.u.app, ast_meta_make_garbage(),
                     make_ast_ident(es->cs->cm.func),
                     ast_typeexpr_array_make(args, args_count));
  }

  if (!unify_directionally(es->cs->im, partial_type, &funcexpr)) {
    METERR(es->cs, x->meta, "lambda type does not match expression type.%s", "\n");
    goto fail_funcexpr;
  }

  /* Because lambdas can't capture variables, we make a fresh exprscope. */

  struct ast_vardecl *replaced_vardecls
    = malloc_mul(sizeof(*replaced_vardecls), func_params_count);
  size_t replaced_vardecls_size = func_params_count;

  for (size_t i = 0; i < func_params_count; i++) {
    struct ast_typeexpr type;
    ast_typeexpr_init_copy(&type, &funcexpr.u.app.params.ptr[i]);
    struct ast_ident name_copy;
    ast_ident_init_copy(&name_copy, &x->params[i].name);
    ast_vardecl_init(&replaced_vardecls[i],
                     ast_meta_make_copy(&x->params[i].meta),
                     name_copy,
                     type);
  }

  /* The funcbody does not need to be a statically computable
  expression, because the lambda is not evaluated. */
  struct exprscope bb_es;
  exprscope_init(&bb_es, es->cs, es->generics, es->generics_substitutions,
                 es->generics_substitutions_count,
                 es->accessible, es->accessible_count,
                 STATIC_COMPUTATION_NO, NULL);

  for (size_t i = 0; i < func_params_count; i++) {
    int res = exprscope_push_var(&bb_es, &replaced_vardecls[i]);
    /* Pushing the var should succeed, because we already called
    check_var_shadowing above. */
    CHECK(res);
  }

  struct ast_typeexpr computed_return_type;
  if (!check_expr_funcbody(
          &bb_es,
          &x->body,
          &funcexpr.u.app.params.ptr[size_sub(funcexpr.u.app.params.count, 1)],
          &computed_return_type)) {
    CHECK_DBG("check_expr_funcbody fails\n");
    goto fail_bb_es;
  }

  ast_typeexpr_destroy(&computed_return_type);
  exprscope_destroy(&bb_es);
  SLICE_FREE(replaced_vardecls, replaced_vardecls_size, ast_vardecl_destroy);

  for (size_t i = 0, e = x->params_count; i < e; i++) {
    struct ast_typeexpr concrete_param_type;
    ast_typeexpr_init_copy(&concrete_param_type, &funcexpr.u.app.params.ptr[i]);
    ast_var_info_specify(&x->params[i].var_info, concrete_param_type);
  }

  *out = funcexpr;
  CHECK_DBG("check_expr_lambda succeeds\n");
  return 1;

 fail_bb_es:
  exprscope_destroy(&bb_es);
  SLICE_FREE(replaced_vardecls, replaced_vardecls_size, ast_vardecl_destroy);
 fail_funcexpr:
  ast_typeexpr_destroy(&funcexpr);
  return 0;
}

int check_expr_magic_binop(struct exprscope *es,
                           struct ast_binop_expr *x,
                           struct ast_typeexpr *partial_type,
                           struct ast_typeexpr *out,
                           int *is_lvalue_out) {
  int ret = 0;
  struct ast_typeexpr no_partial = ast_unknown_garbage();

  if (!check_expr(es, x->lhs, &no_partial)) {
    goto cleanup;
  }

  switch (x->operator) {
  case AST_BINOP_ASSIGN: {
    if (!check_expr(es, x->rhs, ast_expr_type(x->lhs))) {
      goto cleanup;
    }
    if (es->computation == STATIC_COMPUTATION_YES) {
      METERR(es->cs, x->meta, "Assignment within statically evaluated expression.%s", "\n");
      goto cleanup;
    }
    if (!ast_expr_is_lvalue(x->lhs)) {
      METERR(es->cs, x->meta, "Trying to assign to non-lvalue.%s", "\n");
      goto cleanup;
    }
    if (!exact_typeexprs_equal(es->cs->im, ast_expr_type(x->lhs), ast_expr_type(x->rhs))) {
      METERR(es->cs, x->meta, "Assignment with non-matching types.%s", "\n");
      goto cleanup;
    }

    if (!unify_directionally(es->cs->im, partial_type, ast_expr_type(x->lhs))) {
      METERR(es->cs, x->meta, "LHS type of assignment does not match contextual type.%s", "\n");
      goto cleanup;
    }

    ast_typeexpr_init_copy(out, ast_expr_type(x->lhs));
    *is_lvalue_out = 1;

    ret = 1;
  } break;
  case AST_BINOP_LOGICAL_OR:
  case AST_BINOP_LOGICAL_AND: {
    if (!check_expr(es, x->rhs, &no_partial)) {
      goto cleanup;
    }
    struct ast_typeexpr boolean;
    init_name_type(&boolean, es->cs->cm.boole);

    if (!unify_directionally(es->cs->im, &boolean, ast_expr_type(x->lhs))) {
      METERR(es->cs, x->meta, "LHS of and/or is non-boolean.%s", "\n");
      goto logical_cleanup_boolean;
    }

    if (!unify_directionally(es->cs->im, &boolean, ast_expr_type(x->rhs))) {
      METERR(es->cs, x->meta, "RHS of and/or is non-boolean.%s", "\n");
      goto logical_cleanup_boolean;
    }

    if (!unify_directionally(es->cs->im, partial_type, &boolean)) {
      METERR(es->cs, x->meta, "And/or expression in non-boolean context.%s", "\n");
      goto logical_cleanup_boolean;
    }

    *out = boolean;
    *is_lvalue_out = 0;

    ret = 1;
    goto cleanup;
  logical_cleanup_boolean:
    ast_typeexpr_destroy(&boolean);
    goto cleanup;
  } break;
  default:
    UNREACHABLE();
  }

 cleanup:
  ast_typeexpr_destroy(&no_partial);
  return ret;
}

int check_expr_binop(struct exprscope *es,
                     struct ast_binop_expr *x,
                     struct ast_typeexpr *partial_type,
                     struct ast_typeexpr *out,
                     int *is_lvalue_out) {
  CHECK(is_magic_binop(x->operator));
  return check_expr_magic_binop(es, x, partial_type, out, is_lvalue_out);
}

int view_ptr_target(struct common_idents *cm,
                    struct ast_typeexpr *ptr_type,
                    struct ast_typeexpr **target_out) {
  if (ptr_type->tag != AST_TYPEEXPR_APP) {
    return 0;
  }

  if (ptr_type->u.app.name.value != cm->ptr) {
    return 0;
  }

  if (ptr_type->u.app.params.count != 1) {
    return 0;
  }

  *target_out = &ptr_type->u.app.params.ptr[0];
  return 1;
}

void wrap_in_ptr(struct common_idents *cm,
                 struct ast_typeexpr *target,
                 struct ast_typeexpr *ptr_out) {
  ptr_out->tag = AST_TYPEEXPR_APP;
  struct ast_typeexpr *params = malloc_mul(sizeof(*params), 1);
  ast_typeexpr_init_copy(&params[0], target);
  ast_typeapp_init(&ptr_out->u.app, ast_meta_make_garbage(),
                   make_ast_ident(cm->ptr), ast_typeexpr_array_make(params, 1));
}

int check_expr_magic_unop(struct exprscope *es,
                          struct ast_expr *y,
                          struct ast_typeexpr *partial_type) {
  CHECK(y->tag == AST_EXPR_UNOP);
  struct ast_unop_expr *x = &y->u.unop_expr;
  if (es->computation == STATIC_COMPUTATION_YES) {
    METERR(es->cs, x->meta, "Magic unops not allowed in static expressions.%s", "\n");
    return 0;
  }

  int ret = 0;
  struct ast_typeexpr no_partial = ast_unknown_garbage();

  if (!check_expr(es, x->rhs, &no_partial)) {
    goto cleanup;
  }

  switch (x->operator) {
  case AST_UNOP_DEREFERENCE: {
    struct ast_typeexpr *rhs_target;
    if (!view_ptr_target(&es->cs->cm, ast_expr_type(x->rhs), &rhs_target)) {
      METERR(es->cs, x->meta, "Trying to dereference a non-pointer.%s", "\n");
      goto cleanup;
    }

    if (!unify_directionally(es->cs->im, partial_type, rhs_target)) {
      METERR(es->cs, x->meta, "Pointer dereference results in wrong type.%s", "\n");
      goto cleanup;
    }

    struct ast_typeexpr return_type;
    ast_typeexpr_init_copy(&return_type, rhs_target);
    ast_expr_update(y, ast_expr_info_typechecked_no_temporary(1, return_type));
    ret = 1;
  } break;
  case AST_UNOP_ADDRESSOF: {
    if (!ast_expr_is_lvalue(x->rhs)) {
      METERR(es->cs, x->meta, "Trying to take the address of a non-lvalue.%s", "\n");
      goto cleanup;
    }

    struct ast_typeexpr pointer_type;
    wrap_in_ptr(&es->cs->cm, ast_expr_type(x->rhs), &pointer_type);

    if (!unify_directionally(es->cs->im, partial_type, &pointer_type)) {
      METERR(es->cs, x->meta, "Addressof results in wrong type.%s", "\n");
      ast_typeexpr_destroy(&pointer_type);
      goto cleanup;
    }

    /* addressof returns a pointer, which is a temporary though it is also trivial. */
    ast_expr_update(y, ast_expr_info_typechecked_trivial_temporary(0, pointer_type));
    ret = 1;
  } break;
  case AST_UNOP_NEGATE:
  case AST_UNOP_CONVERT:
  case AST_UNOP_UPCONVERT:
  case AST_UNOP_LOGICAL_NOT:
  case AST_UNOP_BITWISE_NOT:
  default:
    UNREACHABLE();
  }

 cleanup:
  ast_typeexpr_destroy(&no_partial);
  return ret;
}

int check_expr_unop(struct exprscope *es,
                    struct ast_expr *x,
                    struct ast_typeexpr *partial_type) {
  CHECK(is_magic_unop(x->u.unop_expr.operator));
  return check_expr_magic_unop(es, x, partial_type);
}

int lookup_fields_field_type(struct checkstate *cs,
                             struct ast_vardecl *fields,
                             size_t fields_count,
                             struct ast_ident *field_name,
                             struct ast_typeexpr *field_type_out) {
  for (size_t i = 0; i < fields_count; i++) {
    if (fields[i].name.value == field_name->value) {
      ast_typeexpr_init_copy(field_type_out, &fields[i].type);
      return 1;
    }
  }

  METERR(cs, field_name->meta, "Field name not found.%s", "\n");
  return 0;
}

int lookup_field_type(struct exprscope *es,
                      struct ast_typeexpr *type,
                      struct ast_fieldname *fieldname,
                      struct ast_typeexpr *field_type_out) {
  switch (type->tag) {
  case AST_TYPEEXPR_NAME: {
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(&es->cs->nt, type->u.name.value,
                                   no_param_list_arity(),
                                   &ent)) {
      struct databuf buf;
      databuf_init(&buf);
      sprint_typeexpr(&buf, es->cs->im, type);
      METERR(es->cs, fieldname->meta,
             "ICE: lookup_field_type cannot find type, '%*s'.\n",
             size_to_int(buf.count), buf.buf);
      CRASH("Crashing.");
    }

    if (ent->is_primitive) {
      METERR(es->cs, fieldname->meta, "Looking up field on primitive type %.*s.",
             IM_P(es->cs->im, type->u.name.value));
      return 0;
    }

    struct ast_deftype *deftype = ent->deftype;
    CHECK(!deftype->generics.has_type_params);
    if (!deftype_is_accessible(es, deftype)) {
      METERR(es->cs, fieldname->meta, "Looking up field on inaccessible type '%.*s'.\n", IM_P(es->cs->im, type->u.name.value));
      return 0;
    }

    switch (deftype->rhs.tag) {
    case AST_DEFTYPE_RHS_TYPE: {
      return lookup_field_type(es, &deftype->rhs.u.type, fieldname, field_type_out);
    } break;
    case AST_DEFTYPE_RHS_ENUMSPEC:
      METERR(es->cs, fieldname->meta, "Looking up field on enumspec type.%s", "\n");
      return 0;
    default:
      UNREACHABLE();
    }
  } break;
  case AST_TYPEEXPR_APP: {
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(&es->cs->nt, type->u.app.name.value,
                                   param_list_arity(type->u.app.params.count),
                                   &ent)) {
      CRASH("lookup_field_type sees an invalid generic type.");
    }
    if (ent->is_primitive) {
      METERR(es->cs, fieldname->meta, "Looking up field on primitive type.%s", "\n");
      return 0;
    }

    struct ast_deftype *deftype = ent->deftype;
    CHECK(deftype->generics.has_type_params
          && deftype->generics.params.count == type->u.app.params.count);
    if (!deftype_is_accessible(es, deftype)) {
      METERR(es->cs, fieldname->meta, "Looking up field on inaccessible type '%.*s'.",
             IM_P(es->cs->im, type->u.app.name.value));
      return 0;
    }

    switch (deftype->rhs.tag) {
    case AST_DEFTYPE_RHS_TYPE: {
      struct ast_typeexpr concrete_deftype_type;
      do_replace_generics(&deftype->generics,
                          type->u.app.params.ptr,
                          type->u.app.params.count,
                          &deftype->rhs.u.type,
                          &concrete_deftype_type);

      int ret = lookup_field_type(es, &concrete_deftype_type, fieldname,
                                  field_type_out);
      ast_typeexpr_destroy(&concrete_deftype_type);
      return ret;
    } break;
    case AST_DEFTYPE_RHS_ENUMSPEC:
      METERR(es->cs, fieldname->meta, "Looking up field on enumspec type.%s", "\n");
      return 0;
    default:
      UNREACHABLE();
    }
  } break;
  case AST_TYPEEXPR_STRUCTE: {
    return lookup_fields_field_type(es->cs,
                                    type->u.structe.fields,
                                    type->u.structe.fields_count,
                                    &fieldname->ident,
                                    field_type_out);
  } break;
  case AST_TYPEEXPR_UNIONE: {
    return lookup_fields_field_type(es->cs,
                                    type->u.unione.fields,
                                    type->u.unione.fields_count,
                                    &fieldname->ident,
                                    field_type_out);
  }
  case AST_TYPEEXPR_ARRAY: {
    METERR(es->cs, fieldname->meta, "Looking up field on array type.%s", "\n");
    return 0;
  } break;
  default:
    UNREACHABLE();
  }
}

int check_expr_local_field_access(
    struct exprscope *es,
    struct ast_expr *y,
    struct ast_typeexpr *partial_type) {
  struct ast_local_field_access *x = &y->u.local_field_access;
  int ret = 0;
  struct ast_typeexpr lhs_partial_type = ast_unknown_garbage();

  if (!check_expr(es, x->lhs, &lhs_partial_type)) {
    goto cleanup;
  }

  if (ast_expr_type(x->lhs)->tag == AST_TYPEEXPR_ARRAY
      && x->fieldname.ident.value == es->cs->cm.array_length_fieldname) {
    struct ast_typeexpr size_type;
    init_name_type(&size_type, es->cs->cm.size_type_name);

    if (!unify_directionally(es->cs->im, partial_type, &size_type)) {
      ast_typeexpr_destroy(&size_type);
      METERR(es->cs, x->meta, "Array .%s field used in place with bad type.\n",
             ARRAY_LENGTH_FIELDNAME);
      goto cleanup;
    }

    ast_expr_update(y, ast_expr_info_typechecked_trivial_temporary(0, size_type));
    ret = 1;
    goto cleanup;
  }


  struct ast_typeexpr field_type;
  if (!lookup_field_type(es,
                         ast_expr_type(x->lhs),
                         &x->fieldname,
                         &field_type)) {
    goto cleanup;
  }

  if (!unify_directionally(es->cs->im, partial_type, &field_type)) {
    METERR(es->cs, x->meta, "Field '%.*s' has wrong type for expression context.\n",
           IM_P(es->cs->im, x->fieldname.ident.value));
    goto cleanup_field_type;
  }

  ast_expr_update(y, expr_info_typechecked_subobject(field_type, &x->lhs->info));
  ret = 1;
  goto cleanup;

 cleanup_field_type:
  ast_typeexpr_destroy(&field_type);
 cleanup:
  ast_typeexpr_destroy(&lhs_partial_type);
  return ret;
}

int check_expr_deref_field_access(
    struct exprscope *es,
    struct ast_deref_field_access *x,
    struct ast_typeexpr *partial_type,
    struct ast_typeexpr *out) {
  if (es->computation == STATIC_COMPUTATION_YES) {
    METERR(es->cs, x->meta, "Dereferencing field access disallowed in static computation.%s", "\n");
    return 0;
  }
  int ret = 0;
  /* Even though we know the lhs is supposed to be a ptr, we shouldn't
  put that info into the context when type checking it. */
  struct ast_typeexpr lhs_partial_type = ast_unknown_garbage();

  if (!check_expr(es, x->lhs, &lhs_partial_type)) {
    goto cleanup;
  }

  struct ast_typeexpr *ptr_target;
  if (!view_ptr_target(&es->cs->cm, ast_expr_type(x->lhs), &ptr_target)) {
    METERR(es->cs, x->meta, "Dereferencing field access expects ptr type.%s", "\n");
    goto cleanup;
  }

  struct ast_typeexpr field_type;
  if (!lookup_field_type(es,
                         ptr_target,
                         &x->fieldname,
                         &field_type)) {
    goto cleanup;
  }

  if (!unify_directionally(es->cs->im, partial_type, &field_type)) {
    METERR(es->cs, x->meta, "Dereferencing field access results in wrong type.%s", "\n");
    goto cleanup_field_type;
  }

  *out = field_type;
  ret = 1;
  goto cleanup;

 cleanup_field_type:
  ast_typeexpr_destroy(&field_type);
 cleanup:
  ast_typeexpr_destroy(&lhs_partial_type);
  return ret;
}

int check_index_expr(struct exprscope *es,
                     struct ast_expr *y,
                     struct ast_typeexpr *partial_type) {
  struct ast_index_expr *a = &y->u.index_expr;
  struct ast_typeexpr no_partial_type = ast_unknown_garbage();;

  if (!check_expr(es, a->lhs, &no_partial_type)) {
    goto fail;
  }

  struct ast_typeexpr rhs_partial_type;
  init_name_type(&rhs_partial_type, es->cs->cm.size_type_name);
  int check_rhs_result = check_expr(es, a->rhs, &rhs_partial_type);
  ast_typeexpr_destroy(&rhs_partial_type);
  if (!check_rhs_result) {
    goto fail;
  }

  struct ast_typeexpr *lhs_type = ast_expr_type(a->lhs);

  struct ast_typeexpr *lhs_target;
  if (!view_ptr_target(&es->cs->cm, lhs_type, &lhs_target)) {
    if (lhs_type->tag != AST_TYPEEXPR_ARRAY) {
      METERR(es->cs, a->meta, "Indexing into a non-pointer, non-array type.%s", "\n");
      goto fail;
    }

    lhs_target = lhs_type->u.arraytype.param;
  } else {
    if (lhs_target->tag != AST_TYPEEXPR_ARRAY) {
      METERR(es->cs, a->meta, "Indexing via pointer into a non-array type.%s", "\n");
      goto fail;
    }
    lhs_target = lhs_target->u.arraytype.param;
  }

  if (!unify_directionally(es->cs->im, partial_type, lhs_target)) {
    METERR(es->cs, a->meta, "Indexing returns wrong type.%s", "\n");
    goto fail;
  }

  struct ast_typeexpr return_type;
  ast_typeexpr_init_copy(&return_type, lhs_target);

  struct ast_expr_info expr_info;

  if (lhs_type->tag == AST_TYPEEXPR_ARRAY) {
    expr_info = expr_info_typechecked_subobject(return_type, &a->lhs->info);
  } else {
    expr_info = ast_expr_info_typechecked_no_temporary(1, return_type);
  }

  ast_typeexpr_destroy(&no_partial_type);

  ast_expr_update(y, expr_info);
  return 1;

 fail:
  ast_typeexpr_destroy(&no_partial_type);
  return 0;
}

int chase_struct_field_types(struct checkstate *cs,
                             struct ast_meta *meta,
                             struct ast_typeexpr *type,
                             size_t expected_field_count,
                             struct ast_typeexpr *structe_or_array_type_out) {
  switch (type->tag) {
  case AST_TYPEEXPR_NAME: {
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(&cs->nt, type->u.name.value,
                                   no_param_list_arity(),
                                   &ent)) {
      METERR(cs, *meta, "ICE?  Type '%.*s' not found when checking for struct fields.\n",
             IM_P(cs->im, type->u.name.value));
      return 0;
    }

    if (ent->is_primitive) {
      METERR(cs, *meta, "Using a struct initializer on primitive type '%.*s'\n",
             IM_P(cs->im, type->u.name.value));
      return 0;
    }

    struct ast_deftype *deftype = ent->deftype;
    CHECK(deftype);
    switch (deftype->disposition) {
    case AST_DEFTYPE_NOT_CLASS: {
      switch (deftype->rhs.tag) {
      case AST_DEFTYPE_RHS_TYPE: {
        return chase_struct_field_types(cs, meta, &deftype->rhs.u.type,
                                        expected_field_count,
                                        structe_or_array_type_out);
      } break;
      case AST_DEFTYPE_RHS_ENUMSPEC: {
        METERR(cs, *meta, "Using a struct initializer on enum type '%.*s'\n",
               IM_P(cs->im, type->u.name.value));
        return 0;
      } break;
      default:
        UNREACHABLE();
      }
    } break;
    case AST_DEFTYPE_CLASS_DEFAULT_COPY_MOVE_DESTROY: /* fallthrough */
    case AST_DEFTYPE_CLASS_DEFAULT_MOVE: /* fallthrough */
    case AST_DEFTYPE_CLASS_NO_DEFAULTS: {
      METERR(cs, *meta, "Using a struct initializer on class type '%.*s'\n",
             IM_P(cs->im, type->u.name.value));
      return 0;
    } break;
    default:
      UNREACHABLE();
    }
  } break;
  case AST_TYPEEXPR_APP: {
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(&cs->nt, type->u.app.name.value,
                                   param_list_arity(type->u.app.params.count),
                                   &ent)) {
      METERR(cs, *meta, "ICE?  Type '%.*s[...]' not found when checking for struct fields.\n",
             IM_P(cs->im, type->u.app.name.value));
      return 0;
    }

    /* TODO: So much fucking copying and pasting. */

    if (ent->is_primitive) {
      METERR(cs, *meta, "Using a struct initializer on primitive type '%.*s'\n",
             IM_P(cs->im, type->u.app.name.value));
      return 0;
    }

    struct ast_deftype *deftype = ent->deftype;
    CHECK(deftype);
    switch (deftype->disposition) {
    case AST_DEFTYPE_NOT_CLASS: {
      switch (deftype->rhs.tag) {
      case AST_DEFTYPE_RHS_TYPE: {
        struct ast_typeexpr replaced_type;
        do_replace_generics(&deftype->generics,
                            type->u.app.params.ptr,
                            type->u.app.params.count,
                            &deftype->rhs.u.type,
                            &replaced_type);

        int ret = chase_struct_field_types(cs, meta, &replaced_type,
                                           expected_field_count,
                                           structe_or_array_type_out);
        ast_typeexpr_destroy(&replaced_type);
        return ret;
      } break;
      case AST_DEFTYPE_RHS_ENUMSPEC: {
        METERR(cs, *meta, "Using a struct initializer on enum type '%.*s'\n",
               IM_P(cs->im, type->u.app.name.value));
        return 0;
      } break;
      default:
        UNREACHABLE();
      }
    } break;
    case AST_DEFTYPE_CLASS_DEFAULT_COPY_MOVE_DESTROY: /* fallthrough */
    case AST_DEFTYPE_CLASS_DEFAULT_MOVE: /* fallthrough */
    case AST_DEFTYPE_CLASS_NO_DEFAULTS: {
      METERR(cs, *meta, "Using a struct initializer on class type '%.*s'\n",
             IM_P(cs->im, type->u.app.name.value));
      return 0;
    } break;
    default:
      UNREACHABLE();
    }
  } break;
  case AST_TYPEEXPR_STRUCTE: {
    if (type->u.structe.fields_count != expected_field_count) {
      METERR(cs, *meta, "Struct initializer has wrong number of fields.%s", "\n");
      return 0;
    }

    ast_typeexpr_init_copy(structe_or_array_type_out, type);
    return 1;
  } break;
  case AST_TYPEEXPR_UNIONE: {
    METERR(cs, *meta, "Using a struct initializer on a union type.%s", "\n");
    return 0;
  } break;
  case AST_TYPEEXPR_ARRAY: {
    uint32_t arrlen = unsafe_numeric_literal_u32(&type->u.arraytype.number);
    if (arrlen != size_to_uint32(expected_field_count)) {
      METERR(cs, *meta, "Struct initializer has wrong number of fields for array.%s", "\n");
      return 0;
    }
    ast_typeexpr_init_copy(structe_or_array_type_out, type);
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}

int check_expr_strinit(struct exprscope *es,
                       enum allow_incomplete ai,
                       struct ast_expr *x,
                       struct ast_typeexpr *partial_type) {
  if (!is_complete(partial_type)) {
    if (ai == ALLOW_INCOMPLETE_YES) {
      ast_expr_update(x, ast_expr_info_incomplete());
      return 1;
    } else {
      METERR(es->cs, x->u.strinit.meta,
             "Structure literal used ambiguously.%s", "\n");
      goto fail;
    }
  }

  struct ast_typeexpr structish_type;
  if (!chase_struct_field_types(es->cs,
                                &x->u.strinit.meta, partial_type,
                                x->u.strinit.exprs_count,
                                &structish_type)) {
    goto fail;
  }

  if (structish_type.tag == AST_TYPEEXPR_STRUCTE) {
    CHECK(structish_type.u.structe.fields_count == x->u.strinit.exprs_count);

    for (size_t i = 0, e = x->u.strinit.exprs_count; i < e; i++) {
      if (!check_expr(es, &x->u.strinit.exprs[i],
                      &structish_type.u.structe.fields[i].type)) {
        goto fail_structish_type;
      }
    }
  } else if (structish_type.tag == AST_TYPEEXPR_ARRAY) {
    for (size_t i = 0, e = x->u.strinit.exprs_count; i < e; i++) {
      if (!check_expr(es, &x->u.strinit.exprs[i],
                      structish_type.u.arraytype.param)) {
        goto fail_structish_type;
      }
    }
  } else {
    UNREACHABLE();
  }

  struct ast_typeexpr concrete_type;
  ast_typeexpr_init_copy(&concrete_type, partial_type);
  struct ast_typeexpr temporary_type;
  ast_typeexpr_init_copy(&temporary_type, &concrete_type);
  ast_strinit_set_structish_type(&x->u.strinit, structish_type);
  ast_expr_update(x,
                  ast_expr_info_typechecked_temporary(
                      0,
                      concrete_type,
                      temporary_type,
                      1,
                      exprscope_temptag(es)));
  return 1;
 fail_structish_type:
  ast_typeexpr_destroy(&structish_type);
 fail:
  return 0;
}

void replace_name_expr_params(struct exprscope *es,
                              struct ast_name_expr *x,
                              struct ast_name_expr *out) {
  struct ast_name_expr copy;
  ast_name_expr_init_copy(&copy, x);

  if (copy.has_params) {
    for (size_t i = 0, e = copy.params.count; i < e; i++) {
      struct ast_typeexpr replaced;
      replace_generics(es, &copy.params.ptr[i], &replaced);
      ast_typeexpr_destroy(&copy.params.ptr[i]);
      copy.params.ptr[i] = replaced;
    }
  }

  *out = copy;
}

struct ast_numeric_literal numeric_literal_from_u32(uint32_t value) {
  struct int8_slice digits = SLICE_INITIALIZER;

  if (value == 0) {
    int8_slice_push(&digits, 0);
  } else {
    do {
      int8_t units = value % 10;
      int8_slice_push(&digits, units);
      value = value / 10;
    } while (value != 0);
    for (size_t i = 0, j = digits.count - 1; i < j; i++, j--) {
      int8_t tmp = digits.ptr[i];
      digits.ptr[i] = digits.ptr[j];
      digits.ptr[j] = tmp;
    }
  }
  struct ast_numeric_literal ret;
  ast_numeric_literal_init(&ret, ast_meta_make_garbage(), AST_NUMERIC_LITERAL_DEC,
                           int8_array_make(digits.ptr, digits.count));
  return ret;
}

int check_expr_ai(struct exprscope *es,
                  enum allow_incomplete ai,
                  struct ast_expr *x,
                  struct ast_typeexpr *partial_type) {
  if (ast_expr_checked_and_complete(x)) {
    return 1;
  }
  switch (x->tag) {
  case AST_EXPR_NAME: {
    struct ast_name_expr replaced_name;
    replace_name_expr_params(es, &x->u.name, &replaced_name);

    enum match_result match_result;
    struct ast_typeexpr name_type;
    int is_lvalue;
    struct def_instantiation *inst_or_null;
    int lookup_result = exprscope_lookup_name(es, &replaced_name, partial_type,
                                              &name_type, &is_lvalue, &inst_or_null,
                                              ai == ALLOW_INCOMPLETE_NO,
                                              &match_result);
    ast_name_expr_destroy(&replaced_name);
    if (!lookup_result) {
      if (match_result == MATCH_AMBIGUOUSLY && ai == ALLOW_INCOMPLETE_YES) {
        ast_expr_update(x, ast_expr_info_incomplete());
        return 1;
      } else {
        return 0;
      }
    }
    ast_expr_update(x, ast_expr_info_typechecked_no_temporary(is_lvalue, name_type));
    ast_name_expr_info_mark_inst(&x->u.name.info, inst_or_null);
    return 1;
  } break;
  case AST_EXPR_NUMERIC_LITERAL: {
    struct ast_typeexpr combined_type;
    {
      struct ast_typeexpr num_type = ast_numeric_garbage();
      int res = combine_partial_types(es->cs->im, partial_type, &num_type, &combined_type);
      ast_typeexpr_destroy(&num_type);
      if (!res) {
        METERR(es->cs, x->u.numeric_literal.meta, "Numeric literal in bad place.%s", "\n");
        return 0;
      }
    }
    if (is_complete(&combined_type)) {
      ast_expr_update(x, ast_expr_info_typechecked_no_temporary(0, combined_type));
    } else {
      if (ai == ALLOW_INCOMPLETE_YES) {
        ast_expr_update(x, ast_expr_info_incomplete_typed(combined_type));
      } else {
        ast_typeexpr_destroy(&combined_type);
        METERR(es->cs, x->u.numeric_literal.meta, "Numeric literal used ambiguously.%s", "\n");
        return 0;
      }
    }
    return 1;
  } break;
  case AST_EXPR_BOOL_LITERAL: {
    struct ast_typeexpr bool_type;
    init_name_type(&bool_type, es->cs->cm.boole);
    if (!unify_directionally(es->cs->im, partial_type, &bool_type)) {
      METERR(es->cs, x->u.bool_literal.meta, "Bool literal in bad place.%s", "\n");
      ast_typeexpr_destroy(&bool_type);
      return 0;
    }
    ast_expr_update(x, ast_expr_info_typechecked_no_temporary(0, bool_type));
    return 1;
  } break;
  case AST_EXPR_NULL_LITERAL: {
    struct ast_typeexpr unknown = ast_unknown_garbage();
    struct ast_typeexpr combined_type;
    {
      struct ast_typeexpr ptr_type;
      wrap_in_ptr(&es->cs->cm, &unknown, &ptr_type);
      ast_typeexpr_destroy(&unknown);

      int res = combine_partial_types(es->cs->im, partial_type, &ptr_type,
                                      &combined_type);
      ast_typeexpr_destroy(&ptr_type);
      if (!res) {
        METERR(es->cs, x->u.null_literal.meta, "Null literal in bad place.%s", "\n");
        return 0;
      }
    }
    if (is_complete(&combined_type)) {
      ast_expr_update(x, ast_expr_info_typechecked_no_temporary(0, combined_type));
    } else {
      if (ai == ALLOW_INCOMPLETE_YES) {
        ast_expr_update(x, ast_expr_info_incomplete_typed(combined_type));
      } else {
        ast_typeexpr_destroy(&combined_type);
        METERR(es->cs, x->u.null_literal.meta, "Null literal used ambiguously.%s", "\n");
        return 0;
      }
    }
    return 1;
  } break;
  case AST_EXPR_VOID_LITERAL: {
    struct ast_typeexpr void_type;
    init_name_type(&void_type, es->cs->cm.voide);
    if (!unify_directionally(es->cs->im, partial_type, &void_type)) {
      METERR(es->cs, x->u.void_literal.meta, "Void literal in bad place.%s", "\n");
      ast_typeexpr_destroy(&void_type);
      return 0;
    }
    ast_expr_update(x, ast_expr_info_typechecked_no_temporary(0, void_type));
    return 1;
  } break;
  case AST_EXPR_CHAR_LITERAL: {
    /* TODO: This is an exact copy/paste of the
    AST_EXPR_NUMERIC_LITERAL case except for error messages. */
    struct ast_typeexpr combined_type;
    {
      struct ast_typeexpr num_type = ast_numeric_garbage();
      int res = combine_partial_types(es->cs->im, partial_type, &num_type, &combined_type);
      ast_typeexpr_destroy(&num_type);
      if (!res) {
        METERR(es->cs, x->u.char_literal.meta, "Char literal in bad place.%s", "\n");
        return 0;
      }
    }
    if (is_complete(&combined_type)) {
      ast_expr_update(x, ast_expr_info_typechecked_no_temporary(0, combined_type));
    } else {
      if (ai == ALLOW_INCOMPLETE_YES) {
        ast_expr_update(x, ast_expr_info_incomplete_typed(combined_type));
      } else {
        ast_typeexpr_destroy(&combined_type);
        METERR(es->cs, x->u.char_literal.meta, "Char literal used ambiguously.%s", "\n");
        return 0;
      }
    }
    return 1;
  } break;
  case AST_EXPR_STRING_LITERAL: {
    if (es->computation == STATIC_COMPUTATION_YES) {
      METERR(es->cs, *ast_expr_ast_meta(x), "String literals disallowed in static computation.%s", "\n");
      return 0;
    }
    uint32_t array_size = size_to_uint32(x->u.string_literal.values.count);
    struct ast_typeexpr array_type;
    {
      struct ast_typeexpr char_type;
      init_name_type(&char_type, es->cs->cm.char_standin_type_name);
      array_type.tag = AST_TYPEEXPR_ARRAY;
      struct ast_numeric_literal number = numeric_literal_from_u32(array_size);
      ast_arraytype_init(&array_type.u.arraytype, ast_meta_make_garbage(), number, char_type);
    }
    if (!unify_directionally(es->cs->im, partial_type, &array_type)) {
      METERR(es->cs, x->u.string_literal.meta, "String literal in bad place.%s", "\n");
      ast_typeexpr_destroy(&array_type);
      return 0;
    }

    ast_expr_update(x, ast_expr_info_typechecked_no_temporary(0, array_type));
    return 1;
  } break;
  case AST_EXPR_FUNCALL: {
    return check_expr_funcall(es, ai, x, partial_type);
  } break;
  case AST_EXPR_INDEX: {
    return check_index_expr(es, x, partial_type);
  } break;
  case AST_EXPR_UNOP: {
    return check_expr_unop(es, x, partial_type);
  } break;
  case AST_EXPR_BINOP: {
    struct ast_typeexpr return_type;
    int is_lvalue;
    if (!check_expr_binop(es, &x->u.binop_expr, partial_type,
                          &return_type, &is_lvalue)) {
      return 0;
    }
    /* Assignment opers return no temporary, and logical opers return
    a trivial temporary. */
    ast_expr_update(x, ast_expr_info_typechecked_no_or_trivial_temporary(is_lvalue, return_type));
    return 1;
  } break;
  case AST_EXPR_LAMBDA: {
    struct ast_typeexpr type;
    if (!check_expr_lambda(es, &x->u.lambda, partial_type, &type)) {
      return 0;
    }
    /* Function pointers (whenever they get supported) for C-style
    functions are trivial.*/
    ast_expr_update(x, ast_expr_info_typechecked_trivial_temporary(0, type));
    return 1;
  } break;
  case AST_EXPR_LOCAL_FIELD_ACCESS: {
    return check_expr_local_field_access(es, x, partial_type);
  } break;
  case AST_EXPR_DEREF_FIELD_ACCESS: {
    struct ast_typeexpr type;
    if (!check_expr_deref_field_access(es, &x->u.deref_field_access,
                                       partial_type, &type)) {
      return 0;
    }
    /* No temporary, because we're derefing a pointer. */
    ast_expr_update(x, ast_expr_info_typechecked_no_temporary(1, type));
    return 1;
  } break;
  case AST_EXPR_TYPED: {
    struct ast_typeexpr combined_partial_type;
    {
      struct ast_typeexpr replaced_partial_type;
      replace_generics(es, &x->u.typed_expr.type, &replaced_partial_type);

      int res = combine_partial_types(es->cs->im, partial_type, &replaced_partial_type,
                                      &combined_partial_type);
      ast_typeexpr_destroy(&replaced_partial_type);
      if (!res) {
        METERR(es->cs, x->u.typed_expr.meta, "Typed expr specified type does not match context.%s", "\n");
        return 0;
      }
    }

    int check_result = check_expr(es, x->u.typed_expr.expr, &combined_partial_type);
    ast_typeexpr_destroy(&combined_partial_type);
    if (!check_result) {
      return 0;
    }

    ast_expr_update(x, ast_expr_info_typechecked_identical(
                        &x->u.typed_expr.expr->info));
    return 1;
  } break;
  case AST_EXPR_STRINIT: {
    return check_expr_strinit(es, ai, x, partial_type);
  } break;
  default:
    UNREACHABLE();
  }
}

int check_expr(struct exprscope *es,
               struct ast_expr *x,
               struct ast_typeexpr *partial_type) {
  return check_expr_ai(es, ALLOW_INCOMPLETE_NO, x, partial_type);
}

int check_def(struct checkstate *cs, struct ast_def *a) {
  if (!check_generics_shadowing(cs, &a->generics)) {
    return 0;
  }

  if (!check_typeexpr(cs, &a->generics, ast_def_typeexpr(a), NULL)) {
    return 0;
  }

  /* We can only typecheck the def by instantiating it -- so we check
  the ones with no template params. */
  if (!a->generics.has_type_params) {
    struct ast_typeexpr unified;
    struct def_entry *ent;
    struct def_instantiation *inst;
    enum match_result res = name_table_match_def(
        cs,
        &cs->nt,
        &a->name,
        NULL, 0, /* (no generics) */
        ast_def_typeexpr(a),
        REPORT_MODE_ALL,
        &unified,
        &ent,
        &inst);
    if (res != MATCH_SUCCESS) {
      /* Multiple matching, I guess. */
      return 0;
    }
    CHECK(exact_typeexprs_equal(cs->im, &unified, ast_def_typeexpr(a)));
    CHECK(!ent->is_primitive);

    ast_typeexpr_destroy(&unified);

    int ret;
    if (!inst->typecheck_started) {
      CHECK(!inst->annotated_rhs_computed);
      inst->typecheck_started = 1;
      struct exprscope es;
      exprscope_init(&es, cs, &a->generics, NULL, 0,
                     ent->accessible, ent->accessible_count,
                     STATIC_COMPUTATION_YES, ent);
      struct ast_expr annotated_rhs;
      ast_expr_init_copy(&annotated_rhs, &a->rhs);
      ret = check_expr(&es, &annotated_rhs, ast_def_typeexpr(a));
      if (ret) {
        di_set_annotated_rhs(inst, annotated_rhs);
      } else {
        ast_expr_destroy(&annotated_rhs);
      }
      exprscope_destroy(&es);
    } else {
      ret = 1;
    }

    return ret;
  } else {
    return 1;
  }
}

int check_extern_def(struct checkstate *cs, struct ast_extern_def *a) {
  struct ast_generics generics;
  ast_generics_init_no_params(&generics);
  int ret = check_typeexpr(cs, &generics, &a->type, NULL);
  ast_generics_destroy(&generics);
  return ret;
}

int check_access_sanity(struct checkstate *cs, struct ast_access *a) {
  /* Check that the type the access block refers to actually exists, and is a class. */
  struct deftype_entry *ent;
  if (!name_table_lookup_deftype(&cs->nt, a->name.value, a->arity, &ent)) {
    METERR(cs, a->meta, "access block refers to non-existant type.%s", "\n");
    return 0;
  }
  if (ent->is_primitive
      || ent->deftype->disposition == AST_DEFTYPE_NOT_CLASS) {
    METERR(cs, a->meta, "access block refers to non-class type.%s", "\n");
    return 0;
  }
  return 1;
}

int check_toplevel_typewise(struct checkstate *cs, struct ast_toplevel *a);

int check_toplevels_typewise(struct checkstate *cs,
                             struct ast_toplevel *toplevels,
                             size_t toplevels_count) {
  for (size_t i = 0; i < toplevels_count; i++) {
    if (!check_toplevel_typewise(cs, &toplevels[i])) {
      return 0;
    }
  }
  return 1;
}

int check_toplevel_typewise(struct checkstate *cs, struct ast_toplevel *a) {
  switch (a->tag) {
  case AST_TOPLEVEL_IMPORT:
    /* We already parsed and loaded the import. */
    return 1;
  case AST_TOPLEVEL_DEF:
    /* We're checking types here, see check_toplevel_defwise for defs. */
    return 1;
  case AST_TOPLEVEL_EXTERN_DEF:
    return 1;
  case AST_TOPLEVEL_DEFTYPE:
    return check_deftype(cs, lookup_deftype(&cs->nt, &a->u.deftype));
  case AST_TOPLEVEL_ACCESS: {
    if (!check_access_sanity(cs, &a->u.access)) {
      return 0;
    }
    return check_toplevels_typewise(cs, a->u.access.toplevels,
                                    a->u.access.toplevels_count);
  } break;
  default:
    UNREACHABLE();
  }
}

int check_toplevel_defwise(struct checkstate *cs, struct ast_toplevel *a);

int check_toplevels_defwise(struct checkstate *cs,
                            struct ast_toplevel *toplevels,
                            size_t toplevels_count) {
  for (size_t i = 0; i < toplevels_count; i++) {
    if (!check_toplevel_defwise(cs, &toplevels[i])) {
      return 0;
    }
  }
  return 1;
}

int check_toplevel_defwise(struct checkstate *cs, struct ast_toplevel *a) {
  switch (a->tag) {
  case AST_TOPLEVEL_IMPORT:
    /* We already parsed and loaded the import. */
    return 1;
  case AST_TOPLEVEL_DEF:
    return check_def(cs, &a->u.def);
  case AST_TOPLEVEL_EXTERN_DEF:
    return check_extern_def(cs, &a->u.extern_def);
  case AST_TOPLEVEL_DEFTYPE:
    /* Do nothing, we're checking defs here. */
    return 1;
  case AST_TOPLEVEL_ACCESS: {
    /* This check is directly redundant because we call
    check_toplevels_typewise first. */
    if (!check_access_sanity(cs, &a->u.access)) {
      return 0;
    }
    return check_toplevels_defwise(cs, a->u.access.toplevels,
                                   a->u.access.toplevels_count);
  } break;
  default:
    UNREACHABLE();
  }
}

uint32_t unsafe_numeric_literal_u32(struct ast_numeric_literal *a) {
  uint32_t ret;
  /* TODO: This check depends on user-supplied value. */
  int success = numeric_literal_to_u32(a, &ret);
  CHECK(success);
  return ret;
}

#define NUMERIC_LITERAL_OOR "Numeric literal out of range.\n"

int numeric_literal_to_u32(struct ast_numeric_literal *a, uint32_t *out) {
  CHECK(a->digits.count > 0);
  uint32_t base;
  switch (a->tag) {
  case AST_NUMERIC_LITERAL_DEC:
    base = 10;
    break;
  case AST_NUMERIC_LITERAL_HEX:
    base = 16;
    break;
  default:
    UNREACHABLE();
  }

  uint32_t built_value = 0;
  for (size_t i = 0, e = a->digits.count; i < e; i++) {
    if (!try_uint32_mul(built_value, base, &built_value)) {
      ERR_DBG(NUMERIC_LITERAL_OOR);
      return 0;
    }
    if (!try_uint32_add(built_value, a->digits.ptr[i], &built_value)) {
      ERR_DBG(NUMERIC_LITERAL_OOR);
      return 0;
    }
  }

  *out = built_value;
  return 1;
}

int squash_u32_to_i32(uint32_t value, int32_t *out) {
  if (value > 0x7FFFFFFFul) {
    ERR_DBG(NUMERIC_LITERAL_OOR);
    return 0;
  }
  CHECK(value <= INT32_MAX);
  *out = (int32_t)value;
  return 1;
}

int numeric_literal_to_i32(struct ast_numeric_literal *a, int32_t *out) {
  /* TODO: There's no way to plainly represent INT32_MIN.  We should
  get static evaluation of "arbitrary numeric constants"
  implemented. */
  uint32_t value;
  if (!numeric_literal_to_u32(a, &value)) {
    return 0;
  }
  return squash_u32_to_i32(value, out);
}

int squash_u32_to_u8(uint32_t value, uint8_t *out) {
  if (value > 0xFF) {
    ERR_DBG(NUMERIC_LITERAL_OOR);
    return 0;
  }
  CHECK(value <= UINT8_MAX);
  *out = (uint8_t)value;
  return 1;
}

int numeric_literal_to_u8(struct ast_numeric_literal *a, uint8_t *out) {
  uint32_t value;
  if (!numeric_literal_to_u32(a, &value)) {
    return 0;
  }
  return squash_u32_to_u8(value, out);
}

int squash_u32_to_i8(uint32_t value, int8_t *out) {
  if (value > 0x7F) {
    ERR_DBG(NUMERIC_LITERAL_OOR);
    return 0;
  }
  CHECK(value <= INT8_MAX);
  *out = (int8_t)value;
  return 1;
}

int numeric_literal_to_i8(struct ast_numeric_literal *a, int8_t *out) {
  uint32_t value;
  if (!numeric_literal_to_u32(a, &value)) {
    return 0;
  }
  return squash_u32_to_i8(value, out);
}

int eval_static_numeric_literal(struct checkstate *cs,
                                struct ast_typeexpr *type,
                                struct ast_numeric_literal *a,
                                struct static_value *out) {
  CHECK(type->tag == AST_TYPEEXPR_NAME);
  CHECK(is_numeric_type(cs->im, type));

  if (type->u.name.value == cs->cm.i32_type_name) {
    int32_t value;
    if (!numeric_literal_to_i32(a, &value)) {
      return 0;
    }
    static_value_init_i32(out, value);
    return 1;
  } else if (type->u.name.value == cs->cm.u32_type_name) {
    uint32_t value;
    if (!numeric_literal_to_u32(a, &value)) {
      return 0;
    }
    static_value_init_u32(out, value);
    return 1;
  } else if (type->u.name.value == cs->cm.u8_type_name) {
    uint8_t value;
    if (!numeric_literal_to_u8(a, &value)) {
      return 0;
    }
    static_value_init_u8(out, value);
    return 1;
  } else {
    METERR(cs, a->meta, "Compiler incomplete: Numeric literal resolves to type '%.*s', "
           "which this lame compiler cannot statically evaluate.\n",
           IM_P(cs->im, type->u.name.value));
    return 0;
  }
}

int eval_static_value(struct checkstate *cs, struct ast_expr *expr,
                      struct static_value *out);

int32_t st_i32(struct static_value *value) {
  CHECK(value->tag == STATIC_VALUE_I32);
  return value->u.i32_value;
}

uint32_t st_u32(struct static_value *value) {
  CHECK(value->tag == STATIC_VALUE_U32);
  return value->u.u32_value;
}

uint8_t st_u8(struct static_value *value) {
  CHECK(value->tag == STATIC_VALUE_U8);
  return value->u.u8_value;
}

int st_bool(struct static_value *value) {
  CHECK(value->tag == STATIC_VALUE_BOOL);
  CHECK(value->u.bool_value == 0 || value->u.bool_value == 1);
  return value->u.bool_value;
}

int apply_static_funcall(struct static_value *func,
                         struct static_value *params,
                         size_t params_count,
                         struct static_value *out) {
  if (func->tag != STATIC_VALUE_PRIMITIVE_OP) {
    ERR_DBG("Trying to statically evaluate non-primitive funcall.\n");
    return 0;
  }

  /* TODO: We're missing a lot of primitive ops here now.  Implement
  those which... are needed. */
  switch (params_count) {
  case 1:
    switch (func->u.primitive_op.tag) {
    case PRIMITIVE_OP_CONVERT_U8_TO_U8: {
      CHECK(params[0].tag == STATIC_VALUE_U8);
      static_value_init_u8(out, params[0].u.u8_value);
      return 1;
    } break;
    case PRIMITIVE_OP_CONVERT_U8_TO_I32: {
      CHECK(params[0].tag == STATIC_VALUE_U8);
      static_value_init_i32(out, params[0].u.u8_value);
      return 1;
    } break;
    case PRIMITIVE_OP_CONVERT_U8_TO_U32: {
      CHECK(params[0].tag == STATIC_VALUE_U8);
      static_value_init_u32(out, params[0].u.u8_value);
      return 1;
    } break;
    case PRIMITIVE_OP_CONVERT_I32_TO_U8: {
      CHECK(params[0].tag == STATIC_VALUE_I32);
      int32_t val = params[0].u.i32_value;
      uint8_t result;
      if (!try_int32_to_uint8(val, &result)) {
        ERR_DBG("Could not convert i32 %"PRIi32" to a u8.\n", val);
        return 0;
      }
      static_value_init_u8(out, result);
      return 1;
    } break;
    case PRIMITIVE_OP_CONVERT_I32_TO_I32: {
      CHECK(params[0].tag == STATIC_VALUE_I32);
      static_value_init_i32(out, params[0].u.i32_value);
      return 1;
    } break;
    case PRIMITIVE_OP_CONVERT_I32_TO_U32: {
      CHECK(params[0].tag == STATIC_VALUE_I32);
      int32_t val = params[0].u.i32_value;
      uint32_t result;
      if (!try_int32_to_uint32(val, &result)) {
        ERR_DBG("Could not convert i32 %"PRIi32" to a u32.\n", val);
        return 0;
      }
      static_value_init_u32(out, result);
      return 1;
    } break;
    case PRIMITIVE_OP_CONVERT_U32_TO_U8: {
      CHECK(params[0].tag == STATIC_VALUE_U32);
      uint32_t val = params[0].u.u32_value;
      uint8_t result;
      if (!try_uint32_to_uint8(val, &result)) {
        ERR_DBG("Could not convert u32 %"PRIu32" to a u8.\n", val);
        return 0;
      }
      static_value_init_u8(out, result);
      return 1;
    } break;
    case PRIMITIVE_OP_CONVERT_U32_TO_I32: {
      CHECK(params[0].tag == STATIC_VALUE_U32);
      uint32_t val = params[0].u.u32_value;
      int32_t result;
      if (!try_uint32_to_int32(val, &result)) {
        ERR_DBG("Could not convert u32 %"PRIu32" to an i32.\n", val);
        return 0;
      }
      static_value_init_i32(out, result);
      return 1;
    } break;
    case PRIMITIVE_OP_CONVERT_U32_TO_U32: {
      CHECK(params[0].tag == STATIC_VALUE_U32);
      static_value_init_u32(out, params[0].u.u32_value);
      return 1;
    } break;
    case PRIMITIVE_OP_NEGATE_I32: {
      CHECK(params[0].tag == STATIC_VALUE_I32);
      int32_t val = params[0].u.i32_value;
      int32_t result;
      if (!try_int32_sub(0, val, &result)) {
        ERR_DBG("Could not negate i32 %"PRIi32".\n", val);
        return 0;
      }
      static_value_init_i32(out, result);
      return 1;
    } break;
    case PRIMITIVE_OP_BIT_NOT_U8: {
      CHECK(params[0].tag == STATIC_VALUE_U8);
      uint8_t val = params[0].u.u8_value;
      static_value_init_u8(out, ~val);
      return 1;
    } break;
    case PRIMITIVE_OP_BIT_NOT_I32: {
      CHECK(params[0].tag == STATIC_VALUE_I32);
      int32_t val = params[0].u.i32_value;
      static_value_init_i32(out, ~val);
      return 1;
    } break;
    case PRIMITIVE_OP_BIT_NOT_U32: {
      CHECK(params[0].tag == STATIC_VALUE_U32);
      uint32_t val = params[0].u.u32_value;
      static_value_init_u32(out, ~val);
      return 1;
    } break;
    default:
      CRASH("Primitive op not implemented for static eval.");
    }
    break;
  case 2: {
    int succ_i32 = 0;
    int succ_u32 = 0;
    int succ_u8 = 0;
    int32_t val_i32 = 0; /* Initialized to make cl shut up. */
    uint32_t val_u32 = 0; /* Initialized to make cl shut up. */
    uint8_t val_u8 = 0; /* Initialized to make cl shut up. */
    switch (func->u.primitive_op.tag) {
    case PRIMITIVE_OP_ADD_I32:
      succ_i32 = try_int32_add(st_i32(&params[0]), st_i32(&params[1]), &val_i32);
      break;
    case PRIMITIVE_OP_SUB_I32:
      succ_i32 = try_int32_sub(st_i32(&params[0]), st_i32(&params[1]), &val_i32);
      break;
    case PRIMITIVE_OP_MUL_I32:
      succ_i32 = try_int32_mul(st_i32(&params[0]), st_i32(&params[1]), &val_i32);
      break;
    case PRIMITIVE_OP_DIV_I32:
      if (st_i32(&params[0]) < 0 || st_i32(&params[1]) < 0) {
        ERR_DBG("Negative static value division.\n");
        return 0;
      }
      if (st_i32(&params[1]) == 0) {
        ERR_DBG("Static value division by zero.\n");
        return 0;
      }
      val_i32 = int32_div(st_i32(&params[0]), st_i32(&params[1]));
      succ_i32 = 1;
      break;
    case PRIMITIVE_OP_MOD_I32:
      if (st_i32(&params[0]) < 0 || st_i32(&params[1]) < 0) {
        ERR_DBG("Negative static value modulo.\n");
        return 0;
      }
      if (st_i32(&params[1]) == 0) {
        ERR_DBG("Static value modulo by zero.\n");
        return 0;
      }
      val_i32 = int32_positive_mod(st_i32(&params[0]), st_i32(&params[1]));
      succ_i32 = 1;
      break;
    case PRIMITIVE_OP_LT_I32:
      val_i32 = (st_i32(&params[0]) < st_i32(&params[1]));
      succ_i32 = 1;
      break;
    case PRIMITIVE_OP_LE_I32:
      val_i32 = (st_i32(&params[0]) <= st_i32(&params[1]));
      succ_i32 = 1;
      break;
    case PRIMITIVE_OP_GT_I32:
      val_i32 = (st_i32(&params[0]) > st_i32(&params[1]));
      succ_i32 = 1;
      break;
    case PRIMITIVE_OP_GE_I32:
      val_i32 = (st_i32(&params[0]) >= st_i32(&params[1]));
      succ_i32 = 1;
      break;
    case PRIMITIVE_OP_EQ_I32:
      val_i32 = (st_i32(&params[0]) == st_i32(&params[1]));
      succ_i32 = 1;
      break;
    case PRIMITIVE_OP_NE_I32:
      val_i32 = (st_i32(&params[0]) != st_i32(&params[1]));
      succ_i32 = 1;
      break;
    case PRIMITIVE_OP_BIT_XOR_I32:
      val_i32 = (st_i32(&params[0]) ^ st_i32(&params[1]));
      succ_i32 = 1;
      break;
    case PRIMITIVE_OP_BIT_OR_I32:
      val_i32 = (st_i32(&params[0]) | st_i32(&params[1]));
      succ_i32 = 1;
      break;
    case PRIMITIVE_OP_BIT_AND_I32:
      val_i32 = (st_i32(&params[0]) & st_i32(&params[1]));
      succ_i32 = 1;
      break;
    case PRIMITIVE_OP_BIT_LEFTSHIFT_I32: {
      int32_t left = st_i32(&params[0]);
      int32_t right = st_i32(&params[1]);
      if (left < 0 || right < 0 || right >= 32) {
        break;
      } else {
        int64_t left64 = left;
        left64 <<= right;
        if (left64 > INT32_MAX) {
          break;
        } else {
          val_i32 = (int32_t)left64;
          succ_i32 = 1;
        }
      }
    } break;
    case PRIMITIVE_OP_BIT_RIGHTSHIFT_I32: {
      int32_t left = st_i32(&params[0]);
      int32_t right = st_i32(&params[1]);
      if (left < 0 || right < 0 || right >= 32) {
        ERR_DBG("Invalid i32 right-shift.\n");
        return 0;
      } else {
        val_i32 = (left >> right);
        succ_i32 = 1;
      }
    } break;

    case PRIMITIVE_OP_ADD_U32:
      succ_u32 = try_uint32_add(st_u32(&params[0]), st_u32(&params[1]), &val_u32);
      break;
    case PRIMITIVE_OP_SUB_U32:
      succ_u32 = try_uint32_sub(st_u32(&params[0]), st_u32(&params[1]), &val_u32);
      break;
    case PRIMITIVE_OP_MUL_U32:
      succ_u32 = try_uint32_mul(st_u32(&params[0]), st_u32(&params[1]), &val_u32);
      break;
    case PRIMITIVE_OP_DIV_U32:
      succ_u32 = try_uint32_div(st_u32(&params[0]), st_u32(&params[1]), &val_u32);
      break;
    case PRIMITIVE_OP_MOD_U32:
      succ_u32 = try_uint32_mod(st_u32(&params[0]), st_u32(&params[1]), &val_u32);
      break;
    case PRIMITIVE_OP_LT_U32:
      val_u32 = (st_u32(&params[0]) < st_u32(&params[1]));
      succ_u32 = 1;
      break;
    case PRIMITIVE_OP_LE_U32:
      val_u32 = (st_u32(&params[0]) <= st_u32(&params[1]));
      succ_u32 = 1;
      break;
    case PRIMITIVE_OP_GT_U32:
      val_u32 = (st_u32(&params[0]) > st_u32(&params[1]));
      succ_u32 = 1;
      break;
    case PRIMITIVE_OP_GE_U32:
      val_u32 = (st_u32(&params[0]) >= st_u32(&params[1]));
      succ_u32 = 1;
      break;
    case PRIMITIVE_OP_EQ_U32:
      val_u32 = (st_u32(&params[0]) == st_u32(&params[1]));
      succ_u32 = 1;
      break;
    case PRIMITIVE_OP_NE_U32:
      val_u32 = (st_u32(&params[0]) != st_u32(&params[1]));
      succ_u32 = 1;
      break;
    case PRIMITIVE_OP_BIT_XOR_U32:
      val_u32 = (st_u32(&params[0]) ^ st_u32(&params[1]));
      succ_u32 = 1;
      break;
    case PRIMITIVE_OP_BIT_OR_U32:
      val_u32 = (st_u32(&params[0]) | st_u32(&params[1]));
      succ_u32 = 1;
      break;
    case PRIMITIVE_OP_BIT_AND_U32:
      val_u32 = (st_u32(&params[0]) & st_u32(&params[1]));
      succ_u32 = 1;
      break;
    case PRIMITIVE_OP_BIT_LEFTSHIFT_U32: {
      if (st_u32(&params[1]) >= 32) {
        break;
      } else {
        uint64_t left64 = st_u32(&params[0]);
        left64 <<= st_u32(&params[1]);
        if (left64 > UINT32_MAX) {
          break;
        } else {
          val_u32 = (uint32_t)left64;
          succ_u32 = 1;
        }
      }
    } break;
    case PRIMITIVE_OP_BIT_RIGHTSHIFT_U32: {
      if (st_u32(&params[1]) >= 32) {
        break;
      } else {
        val_u32 = (st_u32(&params[0]) >> st_u32(&params[1]));
        succ_u32 = 1;
      }
    } break;

    case PRIMITIVE_OP_ADD_U8:
      succ_u8 = try_uint8_add(st_u8(&params[0]), st_u8(&params[1]), &val_u8);
      break;
    case PRIMITIVE_OP_SUB_U8:
      succ_u8 = try_uint8_sub(st_u8(&params[0]), st_u8(&params[1]), &val_u8);
      break;
    case PRIMITIVE_OP_MUL_U8:
      succ_u8 = try_uint8_mul(st_u8(&params[0]), st_u8(&params[1]), &val_u8);
      break;
    case PRIMITIVE_OP_DIV_U8:
      succ_u8 = try_uint8_div(st_u8(&params[0]), st_u8(&params[1]), &val_u8);
      break;
    case PRIMITIVE_OP_MOD_U8:
      succ_u8 = try_uint8_mod(st_u8(&params[0]), st_u8(&params[1]), &val_u8);
      break;
    case PRIMITIVE_OP_LT_U8:
      val_u8 = (st_u8(&params[0]) < st_u8(&params[1]));
      succ_u8 = 1;
      break;
    case PRIMITIVE_OP_LE_U8:
      val_u8 = (st_u8(&params[0]) <= st_u8(&params[1]));
      succ_u8 = 1;
      break;
    case PRIMITIVE_OP_GT_U8:
      val_u8 = (st_u8(&params[0]) > st_u8(&params[1]));
      succ_u8 = 1;
      break;
    case PRIMITIVE_OP_GE_U8:
      val_u8 = (st_u8(&params[0]) >= st_u8(&params[1]));
      succ_u8 = 1;
      break;
    case PRIMITIVE_OP_EQ_U8:
      val_u8 = (st_u8(&params[0]) == st_u8(&params[1]));
      succ_u8 = 1;
      break;
    case PRIMITIVE_OP_NE_U8:
      val_u8 = (st_u8(&params[0]) != st_u8(&params[1]));
      succ_u8 = 1;
      break;
    case PRIMITIVE_OP_BIT_XOR_U8:
      val_u8 = (st_u8(&params[0]) ^ st_u8(&params[1]));
      succ_u8 = 1;
      break;
    case PRIMITIVE_OP_BIT_OR_U8:
      val_u8 = (st_u8(&params[0]) | st_u8(&params[1]));
      succ_u8 = 1;
      break;
    case PRIMITIVE_OP_BIT_AND_U8:
      val_u8 = (st_u8(&params[0]) & st_u8(&params[1]));
      succ_u8 = 1;
      break;
    case PRIMITIVE_OP_BIT_LEFTSHIFT_U8: {
      if (st_u8(&params[1]) >= 8) {
        break;
      } else {
        uint32_t left32 = st_u8(&params[0]);
        left32 <<= st_u8(&params[1]);
        if (left32 > UINT8_MAX) {
          break;
        } else {
          val_u8 = (uint8_t)left32;
          succ_u8 = 1;
        }
      }
    } break;
    case PRIMITIVE_OP_BIT_RIGHTSHIFT_U8: {
      if (st_u8(&params[1]) >= 8) {
        break;
      } else {
        val_u8 = (st_u8(&params[0]) >> st_u8(&params[1]));
        succ_u8 = 1;
      }
    } break;
    default:
      UNREACHABLE();
    }

    if (succ_i32) {
      static_value_init_i32(out, val_i32);
    } else if (succ_u32) {
      static_value_init_u32(out, val_u32);
    } else if (succ_u8) {
      static_value_init_u8(out, val_u8);
    } else {
      ERR_DBG("Binary operation cannot be statically evaluated.\n");
      return 0;
    }
    return 1;
  } break;
  default:
    CRASH("Static funcall has not 1 and not 2 arguments.");
  }
}

int eval_static_funcall(struct checkstate *cs,
                        struct ast_funcall *funcall,
                        struct static_value *out) {
  int ret = 0;
  struct static_value func_value;
  if (!eval_static_value(cs, &funcall->func->expr, &func_value)) {
    goto cleanup;
  }

  size_t params_count = funcall->args.count;
  struct static_value *params = malloc_mul(sizeof(*params), params_count);
  size_t i = 0;
  for (; i < params_count; i++) {
    if (!eval_static_value(cs, &funcall->args.ptr[i].expr, &params[i])) {
      goto cleanup_params;
    }
  }

  if (!apply_static_funcall(&func_value, params, params_count, out)) {
    goto cleanup_params;
  }

  ret = 1;

 cleanup_params:
  SLICE_FREE(params, i, static_value_destroy);
  static_value_destroy(&func_value);
 cleanup:
  return ret;
}

/* expr must have been annotated by typechecking. */
int eval_static_value(struct checkstate *cs,
                      struct ast_expr *expr,
                      struct static_value *out) {
  switch (expr->tag) {
  case AST_EXPR_NAME: {
    struct def_instantiation *inst_or_null;
    if (!ast_name_expr_info_get_inst(&expr->u.name.info, &inst_or_null)) {
      CRASH("Could not lookup instantation.");
    }
    CHECK(inst_or_null);
    static_value_init_copy(out, di_value(inst_or_null));
    return 1;
  } break;
  case AST_EXPR_NUMERIC_LITERAL:
    return eval_static_numeric_literal(cs,
                                       ast_expr_type(expr),
                                       &expr->u.numeric_literal, out);
  case AST_EXPR_BOOL_LITERAL:
    static_value_init_bool(out, expr->u.bool_literal.value);
    return 1;
  case AST_EXPR_NULL_LITERAL:
    CRASH("Null literal static value evaluation is not supported.\n");
  case AST_EXPR_VOID_LITERAL:
    CRASH("Void literal static value evaluation is not supported.\n");
  case AST_EXPR_CHAR_LITERAL:
    static_value_init_u8(out, expr->u.char_literal.value);
    return 1;
  case AST_EXPR_STRING_LITERAL:
    CRASH("String literal static value evaluation is not supported.\n");
  case AST_EXPR_FUNCALL:
    return eval_static_funcall(cs, &expr->u.funcall, out);
  case AST_EXPR_INDEX: {
    /* Array indexing might be deemed such, but right now non-integer,
    non-lambda types are not deemed statically evaluable. */
    CRASH("Pointer indexing should not have been deemed statically "
          "evaluable.\n");
  } break;
  case AST_EXPR_UNOP: {
    CRASH("No (magic) unop exprs should have been deemed "
          "statically evaluable.\n");
  } break;
  case AST_EXPR_BINOP: {
    CRASH("No (magic) binop exprs should have been deemed "
          "statically evaluable.\n");
  } break;
  case AST_EXPR_LAMBDA: {
    struct ast_expr copy;
    ast_expr_init_copy(&copy, expr);
    static_value_init_typechecked_lambda(out, copy);
    return 1;
  } break;
  case AST_EXPR_LOCAL_FIELD_ACCESS: {
    CRASH("No local field access expression should have been deemed "
          "statically evaluable.\n");
  } break;
  case AST_EXPR_DEREF_FIELD_ACCESS: {
    CRASH("No deref field access expression should have been deemed"
          "statically evaluable.\n");
  } break;
  case AST_EXPR_TYPED:
    return eval_static_value(cs, expr->u.typed_expr.expr, out);
  case AST_EXPR_STRINIT:
    CRASH("No struct literal should have been deemed statically evaluable.\n");
  default:
    UNREACHABLE();
  }
}

int compute_static_values(struct checkstate *cs, struct def_entry *ent) {
  int is_primitive = ent->is_primitive;
  CHECK(is_primitive || ent->def != NULL);
  for (size_t i = 0, e = ent->instantiations.count; i < e; i++) {
    struct def_instantiation *inst = ent->instantiations.ptr[i];

    if (is_primitive) {
      switch (ent->primitive_op.tag) {
      case PRIMITIVE_OP_SIZEOF: {
        CHECK(inst->substitutions_count == 1);
        uint32_t size = gp_sizeof(&cs->nt, &inst->substitutions[0]);
        static_value_init_size(cs->arch, di_value_for_set(inst), size);
      } break;
      case PRIMITIVE_OP_ALIGNOF: {
        CHECK(inst->substitutions_count == 1);
        uint32_t alignment = gp_alignof(&cs->nt, &inst->substitutions[0]);
        static_value_init_size(cs->arch, di_value_for_set(inst), alignment);
      } break;
      case PRIMITIVE_OP_ENUMVOID: {
        static_value_init_enumvoid(
            di_value_for_set(inst),
            ent->primitive_op.u.enumconstruct_number,
            gp_sizeof(&cs->nt, &inst->type));
      } break;
      default:
        static_value_init_primitive_op(di_value_for_set(inst), ent->primitive_op);
        break;
      }
    } else {
      if (!inst->typecheck_started) {
        /* TODO: This check failed once (see git blame, change NotComputed of that commit to NotComputed@[gn_annot]()) to trigger this, while NotComputed() doesn't see this problem. */
        ERR_DBG("inst->typecheck_started failed on def %.*s.\n", IM_P(cs->im, ent->name));
        CHECK(inst->typecheck_started);
      }
      struct static_value value;
      if (!eval_static_value(cs, di_annotated_rhs(inst), &value)) {
        return 0;
      }
      static_value_init_move(di_value_for_set(inst), &value);
    }
  }

  return 1;
}

int chase_def_entry_acyclicity(struct checkstate *cs, struct def_entry *ent) {
  if (ent->known_acyclic) {
    return 1;
  }
  if (ent->acyclicity_being_chased) {
    CHECK(ent->def);
    METERR(cs, ent->def->meta, "Cyclic reference in static expressions.%s", "\n");
    return 0;
  }
  ent->acyclicity_being_chased = 1;
  for (size_t i = 0, e = ent->static_references.count; i < e; i++) {
    if (!chase_def_entry_acyclicity(cs, ent->static_references.ptr[i])) {
      return 0;
    }
  }
  CHECK(ent->acyclicity_being_chased == 1);
  ent->acyclicity_being_chased = 0;
  CHECK(ent->known_acyclic == 0);
  ent->known_acyclic = 1;

  if (!ent->is_extern) {
    if (!compute_static_values(cs, ent)) {
      return 0;
    }
  }

  return 1;
}

int check_def_acyclicity(struct checkstate *cs) {
  for (size_t i = 0, e = cs->nt.defs.count; i < e; i++) {
    struct def_entry *ent = cs->nt.defs.ptr[i];
    if (!chase_def_entry_acyclicity(cs, ent)) {
      return 0;
    }
  }
  return 1;
}

int chase_modules_and_typecheck(struct checkstate *cs,
                                ident_value first_module) {
  checkstate_import_primitives(cs);

  int ret = 0;
  if (!chase_imports(cs, first_module)) {
    goto fail;
  }

  // We check types first so that we don't go checking their traits (in a def) before we've figured out the type's flatness.
  for (size_t i = 0, e = cs->imports.count; i < e; i++) {
    struct ast_file *file = cs->imports.ptr[i].file;
    if (!check_toplevels_typewise(cs, file->toplevels, file->toplevels_count)) {
      goto fail;
    }
  }

  for (size_t i = 0, e = cs->imports.count; i < e; i++) {
    struct ast_file *file = cs->imports.ptr[i].file;
    if (!check_toplevels_defwise(cs, file->toplevels, file->toplevels_count)) {
      goto fail;
    }
  }

  if (!check_def_acyclicity(cs)) {
    goto fail;
  }

  ret = 1;
 fail:
  return ret;
}

int test_check_module_with_ctx(struct identmap *im,
                               void *loader_ctx,
                               module_loader *loader,
                               ident_value name) {
  enum target_platform platform = TARGET_PLATFORM_WIN_32BIT;
  struct checkstate cs;
  checkstate_init(&cs, im, loader_ctx, loader, platform);

  int ret = chase_modules_and_typecheck(&cs, name);

  checkstate_destroy(&cs);
  return ret;
}

/* TODO: Remove this. */
int test_check_module(struct identmap *im,
                      module_loader *loader,
                      ident_value name) {
  return test_check_module_with_ctx(im, NULL, loader, name);
}

int read_module_file(void *ctx,
                     const uint8_t *module_name,
                     size_t module_name_count,
                     char **filepath_out,
                     size_t *filepath_size_out,
                     uint8_t **data_out,
                     size_t *data_size_out) {
  CHECK(ctx == NULL);
  char *filename;
  size_t filename_count;
  alloc_half_strcat(module_name, module_name_count,
                    ".ki",
                    &filename, &filename_count);

  if (!read_file(filename, data_out, data_size_out)) {
    ERR("Could not read file %.*s.\n",
        size_to_int(filename_count), filename);
    free(filename);
    return 0;
  } else {
    *filepath_out = filename;
    *filepath_size_out = filename_count;
    return 1;
  }
}

struct test_module {
  const char *name;
  const char *data;
};

int load_test_module(struct test_module *a, size_t a_count,
                     const uint8_t *name, size_t name_count,
                     uint8_t **data_out, size_t *data_count_out) {
  for (size_t i = 0; i < a_count; i++) {
    if (strlen(a[i].name) == name_count
        && 0 == memcmp(a[i].name, name, name_count)) {
      STATIC_CHECK(sizeof(uint8_t) == 1);
      size_t data_count = strlen(a[i].data);
      uint8_t *data = malloc_mul(data_count, sizeof(uint8_t));
      ok_memcpy(data, a[i].data, data_count);
      *data_out = data;
      *data_count_out = data_count;
      return 1;
    }
  }
  return 0;
}

struct test_case {
  struct test_module *modules;
  size_t num_modules;
};

int testcase_module_loader(void *ctx,
                           const uint8_t *name, size_t name_count,
                           char **filepath_out, size_t *filepath_count_out,
                           uint8_t **data_out, size_t *data_count_out) {
  (void)ctx, (void)filepath_out, (void)filepath_count_out;
  struct test_case *tc = ctx;
  return load_test_module(tc->modules, tc->num_modules, name, name_count,
                          data_out, data_count_out);
}

int check_testcase(struct identmap *im, const char *name, struct test_case cas) {
  ident_value foo = identmap_intern_c_str(im, "foo");
  DBG("check_testcase %s...\n", name);
  if (!test_check_module_with_ctx(im, &cas, &testcase_module_loader, foo)) {
    DBG("%s fails\n", name);
    return 0;
  }
  return 1;
}

int check_foocase(struct identmap *im, const char *name, const char *data) {
  struct test_module mod = { "foo", NULL };
  mod.data = data;
  struct test_case cas;
  cas.modules = &mod;
  cas.num_modules = 1;
  return check_testcase(im, name, cas);
}

int check_negcase(struct identmap *im, const char *name, const char *data) {
  ident_value foo = identmap_intern_c_str(im, "foo");
  DBG("check_testcase !%s...\n", name);
  struct test_module mod = { "foo", NULL };
  mod.data = data;
  struct test_case cas;
  cas.modules = &mod;
  cas.num_modules = 1;
  if (test_check_module_with_ctx(im, &cas, &testcase_module_loader, foo)) {
    DBG("!%s fails\n", name);
    return 0;
  }
  return 1;
}

int check_file_testcases(struct identmap *im) {
  struct test_module mods1[] = { { "foo",
                                   "import bar;\n"
                                   "\n"
                                   "def x i32 = 3;"
                                   "struct dword { x u32; }\n" },
                                 { "bar",
                                   "import foo;\n"
                                   "\n"
                                   "def y u32 = 5;\n" } };
  struct test_case case1;
  case1.modules = mods1;
  case1.num_modules = 2;
  int pass = 1;
  pass &= check_testcase(im, "check_file_test_1", case1);
  pass &= check_foocase(im, "check_file_test_2",
                        "def x i32 = 3;"
                        "struct dword { x u32; }\n"
                        "struct blah { x dword; }\n"
                        "struct feh { x ptr[blah]; }\n"
                        "struct quux { x ptr[quux]; }\n");
  /* Fails because foo holds itself flatly. */
  pass &= check_negcase(im, "check_file_test_3",
                        "def x i32 = 3;"
                        "struct foo { x bar; }\n"
                        "struct bar { x foo; }\n");
  pass &= check_foocase(im, "check_file_test_4",
                        "def x i32 = 3;"
                        "struct foo { "
                        "x u32; y i32; z ptr[foo]; }\n");
  pass &= check_foocase(im, "check_file_test_4",
                        "def x i32 = 3;"
                        "struct foo { "
                        "x u32; y i32; z ptr[foo]; }\n");
  pass &= check_foocase(im, "check_file_test_5",
                        "def x i32 = 3;"
                        "struct[T] foo { x T; }");
  pass &= check_foocase(im, "check_file_test_6",
                        "def x i32 = 3;"
                        "struct[T] foo { "
                        "count u32; p ptr[T]; }\n");
  /* Fails because bar recursively holds itself through a template
  parameter. */
  pass &= check_negcase(im, "check_file_test_7",
                        "struct[T, U] foo { x ptr[T]; y U; }\n"
                        "struct bar { z foo[u32, bar]; }\n");
  pass &= check_foocase(im, "check_file_test_8",
                        "struct[T, U] foo { x ptr[T]; y U; }\n"
                        "struct bar { z foo[bar, u32]; }\n");

  return pass;
}

int check_def_testcases(struct identmap *im) {
  int pass = 1;
  pass &= check_foocase(im, "check_file_test_def_1",
                        "def x i32 = 3;\n");
  /* Passes now because numeric literals are flexible. */
  pass &= check_foocase(im, "check_file_test_def_2",
                        "def x u32 = 3;\n");
  pass &= check_foocase(im, "check_file_test_def_3",
                        "def[] x i32 = 3;\n"
                        "def y i32 = x;\n");
  return pass;
}

int check_lambda_testcases(struct identmap *im) {
  int pass = 1;
  pass &= check_foocase(im, "check_file_test_lambda_1",
                        "def x i32 = 3;\n"
                        "def y fn[i32, i32] = func(z i32)i32 {\n"
                        "  return x;\n"
                        "};\n");
  /* Fails because k is a u32. */
  pass &= check_negcase(im, "check_file_test_lambda_4",
                        "def k u32 = k;\n"
                        "def x i32 = 3;\n"
                        "def y fn[i32, i32] = func(z i32)i32 {\n"
                        "  return k;\n"
                        "};\n");
  /* Passes despite x shadowing a global, because that's allowed. */
  pass &= check_foocase(im, "check_file_test_lambda_5",
                        "def x i32 = 3;\n"
                        "def y fn[i32, i32] = func(z i32)i32 {\n"
                        "  x i32 = 4;\n"
                        "  return z;\n"
                        "};\n");
  /* Fails because z shadows a local. */
  pass &= check_negcase(im, "check_file_test_lambda_6",
                        "def x i32 = 3;\n"
                        "def y fn[i32, i32] = func(z i32)i32 {\n"
                        "  z i32 = 4;\n"
                        "  return x;\n"
                        "};\n");
  pass &= check_foocase(im, "check_file_test_lambda_7",
                        "def x i32 = 3;\n"
                        "def y fn[i32, i32] = func(z i32)i32 {\n"
                        "  return x + z + 5;\n"
                        "};\n");
  /* Fails because x is a u32. */
  pass &= check_negcase(im, "check_file_test_lambda_8",
                        "def x u32 = x;\n"
                        "def y fn[i32, i32] = func(z i32)i32 {\n"
                        "  return x + z + 5;\n"
                        "};\n");
  /* Fails because typechecking can't see that 5 is an i32. */
  pass &= check_negcase(im, "check_file_test_lambda_9",
                        "def x i32 = 3;\n"
                        "def y fn[i32, i32] = func(z i32)i32 {\n"
                        "  return -x + z + -5;\n"
                        "};\n");
  /* Fails because you can't negate a u32. */
  pass &= check_negcase(im, "check_file_test_lambda_10",
                        "def x u32 = 3;\n"
                        "def y fn[i32, i32] = func(z i32)i32 {\n"
                        "  return -x;\n"
                        "};\n");
  pass &= check_foocase(im, "check_file_test_lambda_11",
                        "struct foo { x i32; y i32; }\n"
                        "def y fn[foo, i32] = func(z foo) i32 {\n"
                        "  return z.x;\n"
                        "};\n");
  /* Fails because the field x has type u32. */
  pass &= check_negcase(im, "check_file_test_lambda_12",
                        "struct foo { x u32; y i32; }\n"
                        "def y fn[foo, i32] = func(z foo) i32 {\n"
                        "  return z.x;\n"
                        "};\n");
  pass &= check_foocase(im, "check_file_test_lambda_13",
                        "struct[T] foo { x T; y i32; }\n"
                        "def y fn[foo[i32], i32] = func(z foo[i32]) i32 {\n"
                        "  return z.x + z.y;\n"
                        "};\n");
  /* Fails because z.x is a u32. */
  pass &= check_negcase(im, "check_file_test_lambda_14",
                        "struct[T] foo { x T; y i32; }\n"
                        "def y fn[foo[u32], i32] = func(z foo[u32]) i32 {\n"
                        "  return z.x + z.y;\n"
                        "};\n");
  pass &= check_foocase(im, "check_file_test_lambda_15",
                        "def y fn[i32, i32] = func(z i32) i32 {\n"
                        "  k fn[i32, i32] = func(m i32) i32 {\n"
                        "    return m + m;\n"
                        "  };\n"
                        "  return k(z) + k(z);\n"
                        "};\n");

  /* Fails because the inner lambda tries to capture "z". */
  pass &= check_negcase(im, "check_file_test_lambda_16",
                        "def y fn[i32, i32] = func(z i32) i32 {\n"
                        "  k fn[i32, i32] = func(m i32) i32 {\n"
                        "    return m + z;\n"
                        "  };\n"
                        "  return k(z) + k(z);\n"
                        "};\n");
  pass &= check_foocase(im, "check_file_test_lambda_17",
                        "struct foo { x i32; y i32; }\n"
                        "def y fn[ptr[foo], i32] = func(z ptr[foo]) i32 {\n"
                        "  return z->x;\n"
                        "};\n");
  pass &= check_foocase(
      im, "check_file_test_lambda_18",
      "struct[T] foo { x T; y i32; }\n"
      "def y fn[ptr[foo[i32]], i32] = func(z ptr[foo[i32]]) i32 {\n"
      "  return z->x + z->y;\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_lambda_19",
      "struct[T] foo { x T; y i32; }\n"
      "def y fn[ptr[foo[i32]], i32] = func(z ptr[foo[i32]]) i32 {\n"
      "  return (*z).x + (&(*z))->y;\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_lambda_20",
      "struct[T] foo { x T; y i32; }\n"
      "def y fn[ptr[foo[i32]], i32] = func(z ptr[foo[i32]]) i32 {\n"
      "  if (z->x < 3 && z->y > 19) {\n"
      "    z->x = (*z).y + 5;\n"
      "  }\n"
      "  return (*z).x + (&(*z))->y;\n"
      "};\n");
  /* Fails because assignment mismatches types. */
  pass &= check_negcase(
      im, "check_file_test_lambda_21",
      "struct[T] foo { x T; y i32; }\n"
      "def y fn[ptr[foo[i32]], i32] = func(z ptr[foo[i32]]) i32 {\n"
      "  if (z->x < 3 && z->y > 19) {\n"
      "    z->x = z;\n"
      "  }\n"
      "  return (*z).x + (&(*z))->y;\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_lambda_22",
      "def[T] foo fn[ptr[T], T] = func(x ptr[T]) T { return *x; };\n"
      "def bar fn[i32] = func() i32 {\n"
      "  x i32 = 3;\n"
      "  return foo(&x);\n"
      "};\n");
  /* Fails because the def does not match. */
  pass &= check_negcase(
      im, "check_file_test_lambda_23",
      "def[T] foo fn[ptr[T], T] = func(x ptr[T]) T { return *x; };\n"
      "def bar fn[i32] = func() i32 {\n"
      "  x i32 = 3;\n"
      "  return foo(x);\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_lambda_24",
      "def[T] fac fn[T, T] = func(x T) T {\n"
      "  if (x == 0) {\n"
      "    return 1;\n"
      "  } else {\n"
      "    return x * fac(x - 1);\n"
      "  }\n"
      "};\n"
      "def bar fn[i32] = func() i32 {\n"
      "  x i32 = 5;\n"
      "  return fac(x);\n"
      "};\n");
  /* Fails because of recursive template instantiation. */
  pass &= check_negcase(
      im, "check_file_test_lambda_25",
      "struct[T] foo { x i32; }\n"
      "def[T] biggefy fn[T, foo[T]] = func(x T) foo[T] {\n"
      "  return biggefy(x);\n"
      "};\n"
      "def[T] rec fn[T, i32] = func(x T) i32 {\n"
      "  return rec(biggefy(x));\n"
      "};\n"
      "def bar fn[i32] = func() i32 {\n"
      "  x u32 = 5;\n"
      "  return rec(x);\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_lambda_26",
      "def x i32 = 3;\n"
      "def y u32 = 3 + 4;\n"
      "def z fn[i32, i32] = func(k i32) i32 { return k + 1; };\n");

  /* Fails because you can't evaluate z(3) statically. */
  pass &= check_negcase(
      im, "check_file_test_lambda_27",
      "def x i32 = z(3);\n"
      "def z fn[i32, i32] = func(k i32) i32 { return k + 1; };\n");
  pass &= check_foocase(
      im, "check_file_test_lambda_28",
      "def y i32 = -x;\n"
      "def x i32 = -3;\n");
  /* Fails because of cyclic reference. */
  pass &= check_negcase(
      im, "check_file_test_lambda_29",
      "def y i32 = -x;\n"
      "def x i32 = -y;\n");
  return pass;
}

int check_extern_testcases(struct identmap *im) {
  int pass = 1;
  pass &= check_foocase(
      im, "check_file_test_extern_1",
      "extern putchar fn[i32, i32];\n"
      "def foo fn[i32] = func()i32 {\n"
      "  putchar(65);\n"
      "  putchar(10);\n"
      "  return 1;\n"
      "};\n");
  /* Fails because putchar is called with the wrong type. */
  pass &= check_negcase(
      im, "check_file_test_extern_2",
      "extern putchar fn[i32, i32];\n"
      "def foo fn[i32] = func()i32 {\n"
      "  putchar(@[u32]65);\n"
      "  putchar(10);\n"
      "  return 1;\n"
      "};\n");
  /* Fails because putchar has a nonsense return type. */
  pass &= check_negcase(
      im, "check_file_test_extern_3",
      "extern putchar fn[i32, quack];\n"
      "def foo fn[i32] = func()i32 {\n"
      "  putchar(65);\n"
      "  putchar(10);\n"
      "  return 1;\n"
      "};\n");
  return pass;
}

int check_more_testcases(struct identmap *im) {
  int pass = 1;

  pass &= check_foocase(
      im, "check_file_test_more_1",
      "def foo fn[u32, u32] = func(x u32) u32 {\n"
      "  return x + 4;\n"
      "};"
      "def bar fn[i32, i32] = foo;\n"
      "def foo fn[i32, i32] = func(x i32) i32 {\n"
      "  return x + 3;\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_2",
      "def foo fn[u32, u32] = func(x u32) u32 {\n"
      "  return x + 4;\n"
      "};"
      "def bar fn[i32, i32] = foo;\n"
      "def[T] foo fn[T, i32] = func(x T) i32 {\n"
      "  return x + 3;\n"
      "};\n");

  /* Fails because foo's instatiation won't typecheck. */
  pass &= check_negcase(
      im, "check_file_test_more_3",
      "def foo fn[u32, u32] = func(x u32) u32 {\n"
      "  return x + 4;\n"
      "};"
      "def bar fn[u32, i32] = foo;\n"
      "def[T] foo fn[T, i32] = func(x T) i32 {\n"
      "  return x + 3;\n"
      "};\n");
  /* Fails because foo lacks a return statement. */
  pass &= check_negcase(
      im, "check_file_test_more_4",
      "def foo fn[u32, u32] = func(x u32) u32 {\n"
      "  x + x;\n"
      "};");
  pass &= check_foocase(
      im, "check_file_test_more_5",
      "def foo i32 = 7;\n"
      "def bar i32 = 5 << foo;\n");
  /* Fails because shift overflows. */
  pass &= check_negcase(
      im, "check_file_test_more_6",
      "def foo i32 = 30;\n"
      "def bar i32 = 5 << foo;\n");

  pass &= check_foocase(
      im, "check_file_test_more_7",
      "def[T] foo fn[T, T] = func(x T) T {\n"
      "  y T = x;\n"
      "  return y;\n"
      "};\n"
      "def bar fn[i32] = func() i32 {\n"
      "  return foo(3);\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_8",
      "def[T] add32 fn[i32, T, i32] = func(x i32, y T) i32 {\n"
      "  z i32 = 4;\n"
      "  return x + z;\n"
      "};\n"
      "def bar fn[i32] = func() i32 {\n"
      "  return add32(3, @[u32]4);\n"
      "};\n");
  /* Fails because return type in return expression is wrong. */
  pass &= check_negcase(
      im, "check_file_test_more_9",
      "def foo fn[i32] = func() i32 {\n"
      "  return @[u32] 4;\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_10",
      "def x i32 = ~ @[u32]4;\n");

  pass &= check_foocase(
      im, "check_file_test_more_11",
      "def[T] foo fn[ptr[T], i32, T] = func(p ptr[T], i i32) T {\n"
      "  ret T = p[i];\n"
      "  p[i] = p[i + 1];\n"
      "  return ret;\n"
      "};\n");
  /* Fails because vec3 and [3]u32 are different types. */
  pass &= check_negcase(
      im, "check_file_test_more_12",
      "struct vec3 { x ^[3]u32; }\n"
      "def foo fn[^[3]u32, vec3] = func(arr ^[3]u32) vec3 {\n"
      "  v vec3 = arr;\n"
      "  return v;\n"
      "};\n");
  /* Passes because the conversion of ~@[u32]3 can be inferred. */
  pass &= check_foocase(
      im, "check_file_test_more_13",
      "def foo fn[i32] = func() i32 {\n"
      "  return 2 + ~ @[u32]3;\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_14",
      "def foo fn[i32] = func() i32 {\n"
      "  return 2 + @[i32](~ @[u32]3);\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_15",
      "def foo fn[i32] = func() i32 {\n"
      "  x i32 = 5;\n"
      "  while (x > 3) {\n"
      "    x = x + 1;\n"
      "  }\n"
      "  return x;\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_16",
      "def foo fn[i32] = func() i32 {\n"
      "  x i32 = 5;\n"
      "  while !(x > 3) {\n"
      "    x = x + 1;\n"
      "  }\n"
      "  return x;\n"
      "};\n");
  /* Fails because some control paths don't return a value. */
  pass &= check_negcase(
      im, "check_file_test_more_17",
      "def foo fn[i32] = func() i32 {\n"
      "  x i32 = 2;\n"
      "  if (x < 3) {\n"
      "    x = x + 1;\n"
      "  } else {\n"
      "    return x;\n"
      "  }\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_18",
      "def foo fn[i32] = func() i32 {\n"
      "  acc u32 = 0;\n"
      "  for i u32 = 0; i < 10; i = i + 1 {\n"
      "    acc = acc + i;\n"
      "  }\n"
      "  return ~acc;\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_19",
      "def[T] foo fn[i32, T] = func(x i32) T {\n"
      "  // Why not test '@[T]' works where T is generic.\n"
      "  y T = @[T](~x);\n"
      "  return y;\n"
      "};\n"
      "def bar fn[fn[i32, i16], i32, i16] = func(x fn[i32, i16], y i32) i16 {\n"
      "  return x(y);\n"
      "};\n"
      "def baz fn[i16] = func() i16 {\n"
      "  return bar(foo, 4);\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_20",
      "def[T] foo fn[i32, T] = func(x i32) T {\n"
      "  // Why not test '@[T]' works where T is generic.\n"
      "  y T = @[T]~x;\n"
      "  return y;\n"
      "};\n"
      "def bar fn[fn[i32, i16], i32, i16] = func(x fn[i32, i16], y i32) i16 {\n"
      "  return x(y);\n"
      "};\n"
      "def baz fn[i16] = func() i16 {\n"
      "  return bar(foo@[i16], 4);\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_21",
      "struct ty { x i32; y i32; }\n"
      "def foo fn[size] = func() size {\n"
      "  return sizeof@[ty];\n"
      "};\n");
  /* Fails because whatever is not the name of a defclass type. */
  pass &= check_negcase(
      im, "check_file_test_more_22",
      "defclass ty { x i32; y i32; }\n"
      "access whatever {\n"
      "def foo fn[*ty, i32] = func(t *ty) i32 {\n"
      "  return t->x;\n"
      "};\n"
      "}\n");
  /* Fails because ty[] has bad arity. */
  pass &= check_negcase(
      im, "check_file_test_more_23",
      "defclass ty { x i32; y i32; }\n"
      "access ty[] {\n"
      "def foo fn[*ty, i32] = func(t *ty) i32 {\n"
      "  return t->x;\n"
      "};\n"
      "}\n");
  /* Fails because ty[_, _] has bad arity. */
  pass &= check_negcase(
      im, "check_file_test_more_24",
      "defclass ty { x i32; y i32; }\n"
      "access ty[_, _] {\n"
      "def foo fn[*ty, i32] = func(t *ty) i32 {\n"
      "  return t->x;\n"
      "};\n"
      "}\n");
  pass &= check_foocase(
      im, "check_file_test_more_25",
      "defclass ty { x i32; y i32; }\n"
      "access ty {\n"
      "def foo fn[*ty, i32] = func(t *ty) i32 {\n"
      "  return t->x;\n"
      "};\n"
      "}\n");
  pass &= check_foocase(
      im, "check_file_test_more_26",
      "defclass[T] ty { x T; y T; }\n"
      "access ty[_] {\n"
      "def[T] foo fn[*ty[T], T] = func(t *ty[T]) T {\n"
      "  return t->x;\n"
      "};\n"
      "}\n"
      "def bar fn[*ty[i32], i32] = foo;\n");
  /* Fails because we try to access a field of a defclass type. */
  pass &= check_negcase(
      im, "check_file_test_more_27",
      "defclass[T] ty { x T; y T; }\n"
      "def[T] foo fn[*ty[T], T] = func(t *ty[T]) T {\n"
      "  return t->x;\n"
      "};\n"
      "def bar fn[*ty[i32], i32] = foo;\n");
  pass &= check_foocase(
      im, "check_file_test_more_28",
      "struct ty { x i32; }\n"
      "def foo fn[ty, i32] = func(t ty) i32 {\n"
      "  return t.x;\n"
      "};\n");
  /* Fails (unlike more_28) because ty is defclass, and the conversion
  operator is private.  Or, now, we just have field access private. */
  pass &= check_negcase(
      im, "check_file_test_more_29",
      "defclass ty { field i32; }\n"
      "def foo fn[ty, i32] = func(t ty) i32 {\n"
      "  return t.field;\n"
      "};\n");
  /* Fails because the type ty lacks an explicit destructor. */
  pass &= check_negcase(
      im, "check_file_test_more_30a",
      "defclass ty { field i32; }\n"
      "access ty {\n"
      "def foo fn[ty, i32] = func(t ty) i32 {\n"
      "  return t.field;\n"
      "};\n"
      "}\n");
  pass &= check_foocase(
      im, "check_file_test_more_30b",
      "defclass copy ty { field i32; }\n"
      "access ty {\n"
      "def foo fn[ty, i32] = func(t ty) i32 {\n"
      "  return t.field;\n"
      "};\n"
      "}\n");

  /* Fails (like more_29) because ty is defclass, and the conversion
  operator is private. Or, now, the field. */
  pass &= check_negcase(
      im, "check_file_test_more_31",
      "defclass[T] ty { field i32; }\n"
      "def[T] foo fn[ty[T], i32] = func(t ty[T]) i32 {\n"
      "  return t.field;\n"
      "};\n"
      "def bar fn[ty[u32], i32] = foo;\n");
  /* Fails because ty[u32] lacks an explicit destructor. */
  pass &= check_negcase(
      im, "check_file_test_more_32a",
      "defclass[T] ty { field i32; }\n"
      "access ty[_] {\n"
      "def[T] foo fn[ty[T], i32] = func(t ty[T]) i32 {\n"
      "  return t.field;\n"
      "};\n"
      "}\n"
      "def bar fn[ty[u32], i32] = foo;\n");
  pass &= check_foocase(
      im, "check_file_test_more_32b",
      "defclass[T] copy ty { field i32; }\n"
      "access ty[_] {\n"
      "def[T] foo fn[ty[T], i32] = func(t ty[T]) i32 {\n"
      "  return t.field;\n"
      "};\n"
      "}\n"
      "def bar fn[ty[u32], i32] = foo;\n");
  /* Fails (unlike more_28) because ty is defclass, and the conversion
  operator is private. */
  pass &= check_negcase(
      im, "check_file_test_more_33",
      "defclass ty { field i32; }\n"
      "def foo fn[*ty, *i32] = func(t *ty) *i32 {\n"
      "  return &t->field;\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_34",
      "defclass ty { field i32; }\n"
      "access ty {\n"
      "def foo fn[*ty, *i32] = func(t *ty) *i32 {\n"
      "  return &t->field;\n"
      "};\n"
      "}\n");
  /* Fails (like more_29) because ty is defclass, and the conversion
  operator is private. */
  pass &= check_negcase(
      im, "check_file_test_more_35",
      "defclass[T] ty { field i32; }\n"
      "def[T] foo fn[*ty[T], *i32] = func(t *ty[T]) *i32 {\n"
      "  return &t->field;\n"
      "};\n"
      "def bar fn[*ty[u32], *i32] = foo;\n");

  pass &= check_foocase(
      im, "check_file_test_more_36",
      "defclass[T] ty { field i32; }\n"
      "access ty[_] {\n"
      "def[T] foo fn[*ty[T], *i32] = func(t *ty[T]) *i32 {\n"
      "  return &t->field;\n"
      "};\n"
      "}\n"
      "def bar fn[*ty[u32], *i32] = foo;\n");
  pass &= check_foocase(
      im, "check_file_test_more_37",
      "defclass copy ty { field i32; }\n"
      "access ty {\n"
      "def do_init fn[*ty, void] = func(t *ty) void {\n"
      "  ret void;\n"
      "  return ret;\n"
      "};\n"
      "}\n"
      "def foo fn[i32] = func() i32 {\n"
      "  k ty;\n"
      "  return 1;\n"
      "};\n");
  /* Fails because blah is not the name of a type. */
  pass &= check_negcase(
      im, "check_file_test_more_38",
      "def foo fn[i32] = func() i32 {\n"
      "  x blah;\n"
      "  return 1;\n"
      "};\n");
  /* Fails because k is not default-initializable. */
  pass &= check_negcase(
      im, "check_file_test_more_39",
      "defclass copy ty { field i32; }\n"
      "def foo fn[i32] = func() i32 {\n"
      "  k ty;\n"
      "  return 1;\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_40",
      "defclass copy ty { field i32; }\n"
      "access ty {\n"
      "def do_init fn[*ty, void] = func(t *ty) void {\n"
      "  ret void;\n"
      "  return ret;\n"
      "};\n"
      "}\n"
      "struct ty2 { x i32; y ty; }\n"
      "def foo fn[i32] = func() i32 {\n"
      "  k ty2;\n"
      "  return 1;\n"
      "};\n");

  /* Fails because k is not default-initializable. */
  pass &= check_negcase(
      im, "check_file_test_more_41",
      "defclass copy ty { field i32; }\n"
      "struct ty2 { x i32; y ty; }\n"
      "def foo fn[i32] = func() i32 {\n"
      "  k ty2;\n"
      "  return 1;\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_42",
      "def x u8 = '\\x12';\n");
  /* Passes because the char literal value is in range for an i8. */
  pass &= check_foocase(
      im, "check_file_test_more_43",
      "def x i8 = '\\x12';\n");
  /* Fails because string literals not allowed in static evaluation. */
  /* TODO: String literals should be allowed, this should pass. */
  pass &= check_negcase(
      im, "check_file_test_more_44",
      "def x ^[5]u8 = \"pq\\x12rs\";\n");
  /* Fails because the array size is wrong. */
  pass &= check_negcase(
      im, "check_file_test_more_45",
      "def x ^[6]u8 = \"pq\\x12rs\";\n");
  pass &= check_foocase(
      im, "check_file_test_more_46",
      "struct pq { p i32; q i32; }\n"
      "enum ty {\n"
      "  c1 void;\n"
      "  c2 pq;\n"
      "}\n"
      "def foo fn[ty, ty] = func(x ty) ty {\n"
      "  v void;\n"
      "  y ty = c1(v);\n"
      "  u pq;\n"
      "  y = c2(u);\n"
      "  return y;\n"
      "};\n");
  /* Fails because c2 passed wrong type. */
  pass &= check_negcase(
      im, "check_file_test_more_47",
      "struct pq { p i32; q i32; }\n"
      "struct pqu { p i32; q u32; }\n"
      "enum ty {\n"
      "  c1 void;\n"
      "  c2 pq;\n"
      "}\n"
      "def foo fn[ty, ty] = func(x ty) ty {\n"
      "  v void;\n"
      "  y ty = c1(v);\n"
      "  u pqu;\n"
      "  y = c2(u);\n"
      "  return y;\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_48-a",
      "struct[T] pq { p T; q T; }\n"
      "enum[T] ty {\n"
      "  c1 void;\n"
      "  c2 pq[T];\n"
      "}\n"
      "def foo fn[ty[i32], ty[i32]] = func(x ty[i32]) ty[i32] {\n"
      "  v void;\n"
      "  y ty[i32] = c1(v);\n"
      "  u pq[i32];\n"
      "  y = c2(u);\n"
      "  return y;\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_48-b",
      "struct[T] pq { p T; q T; }\n"
      "enum[T] ty {\n"
      "  c1 void;\n"
      "  c2 pq[T];\n"
      "}\n"
      "def foo fn[ty[i32], ty[i32]] = func(x ty[i32]) ty[i32] {\n"
      "  v void;\n"
      "  y ty[i32] = c1;\n"
      "  u pq[i32];\n"
      "  y = c2(u);\n"
      "  return y;\n"
      "};\n");
  /* Fails because c2 returns wrong type. */
  pass &= check_negcase(
      im, "check_file_test_more_49",
      "struct[T] pq { p T; q T; }\n"
      "enum[T] ty {\n"
      "  c1 void;\n"
      "  c2 pq[T];\n"
      "}\n"
      "def foo fn[ty[i32], ty[i32]] = func(x ty[i32]) ty[i32] {\n"
      "  v void;\n"
      "  y ty[i32] = c1(v);\n"
      "  u pq[u32];\n"
      "  y = c2(u);\n"
      "  return y;\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_50a",
      "struct pq { p i32; q i32; }\n"
      "enum ty {\n"
      "  c1 void;\n"
      "  c2 pq;\n"
      "}\n"
      "def foo fn[ty, i32] = func(x ty) i32 {\n"
      "  switch x {\n"
      "    case c1(v void): { return -1; }\n"
      "    case c2(s pq): {\n"
      "      return s.p + s.q;\n"
      "    }\n"
      "  }\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_50b",
      "struct pq { p i32; q i32; }\n"
      "enum ty {\n"
      "  c1 void;\n"
      "  c2 pq;\n"
      "}\n"
      "def foo fn[ty, i32] = func(x ty) i32 {\n"
      "  switch &x {\n"
      "    case &c1(v void): { return -1; }\n"
      "    case &c2(s pq): {\n"
      "      return s.p + s.q;\n"
      "    }\n"
      "  }\n"
      "};\n");

  /* Fails because a control path in the switch does not return a value. */
  pass &= check_negcase(
      im, "check_file_test_more_51",
      "struct pq { p i32; q i32; }\n"
      "enum ty {\n"
      "  c1 void;\n"
      "  c2 pq;\n"
      "}\n"
      "def foo fn[ty, i32] = func(x ty) i32 {\n"
      "  switch x {\n"
      "    case c1(v void): { return -1; }\n"
      "    case c2(s pq): {\n"
      "      s.p + s.q;\n"
      "    }\n"
      "  }\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_52",
      "def foo fn[i32, void] = func(x i32) void {\n"
      "  p *_ = &x;\n"
      "  q _ = *p;\n"
      "  r var = q;\n"
      "  s i32 = r;\n"
      "  ret void;\n"
      "  return ret;\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_53",
      "struct pq { p i32; q i32; }\n"
      "enum ty {\n"
      "  c1 void;\n"
      "  c2 pq;\n"
      "}\n"
      "def foo fn[ty, i32] = func(x ty) i32 {\n"
      "  switch x {\n"
      "    case c1(v void): { return -1; }\n"
      "    case c2(s): {\n"
      "      return s.p + s.q;\n"
      "    }\n"
      "  }\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_54",
      "def foo fn[i32, void] = func(x i32) void {\n"
      "  p *_ = &x;\n"
      "  q _ = *p;\n"
      "  if (x == 3) {\n"
      "    return;\n"
      "  }\n"
      "  r var = q;\n"
      "  s i32 = r;\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_55",
      "def foo = func(x i32, y u32) void {\n"
      "  @[i32] 1;\n"
      "};\n");

  pass &= check_foocase(
      im, "check_file_test_more_56",
      "func foo(x i32, y u32) void {\n"
      "  @[i32] 1;\n"
      "}\n");
  pass &= check_foocase(
      im, "check_file_test_more_57",
      "struct notsize { x u32; }\n"
      "func `~`(x u32) notsize {\n"
      "  ret notsize;\n"
      "  ret.x = x;\n"
      "  return ret;\n"
      "}\n"
      "func foo(x u32) notsize {\n"
      "  return ~(x + 1);\n"
      "}\n");
  /* Fails because x[0] is of wrong type. */
  pass &= check_negcase(
      im, "check_file_test_more_58",
      "func foo() i32 {\n"
      "  x ^[3]u32;\n"
      "  return x[0];\n"
      "}\n");
  pass &= check_foocase(
      im, "check_file_test_more_59",
      "func foo() bool {\n"
      "  x ^[3]u32;\n"
      "  return &x[0] == &x[1];\n"
      "}\n");
  /* Fails because ptr types don't match. */
  pass &= check_negcase(
      im, "check_file_test_more_60",
      "func foo() bool {\n"
      "  x ^[3]u32;\n"
      "  return &x[0] == &x;\n"
      "}\n");

  pass &= check_foocase(
      im, "check_file_test_more_61",
      "func foo(x osize, y size) osize {\n"
      "  return x + ~y;\n"
      "}\n");
  pass &= check_foocase(
      im, "check_file_test_more_62",
      "func foo(x ptr[i32]) bool {\n"
      "  return x == null;\n"
      "}\n");
  /* Fails because size is not a ptr[_]. */
  pass &= check_negcase(
      im, "check_file_test_more_63",
      "func foo(x size) bool {\n"
      "  return x == null;\n"
      "}\n");
  pass &= check_foocase(
      im, "check_file_test_more_64",
      "struct pq { p i32; q i32; }\n"
      "enum ty {\n"
      "  c1 void;\n"
      "  c2 pq;\n"
      "}\n"
      "def foo fn[ty, i32] = func(x ty) i32 {\n"
      "  switch x {\n"
      "    default: { return -1; }\n"
      "    case c2(s): {\n"
      "      return s.p + s.q;\n"
      "    }\n"
      "  }\n"
      "};\n");
  /* Fails because of overlapping default cases. */
  pass &= check_negcase(
      im, "check_file_test_more_65a",
      "struct pq { p i32; q i32; }\n"
      "enum ty {\n"
      "  c1 void;\n"
      "  c2 pq;\n"
      "}\n"
      "def foo fn[ty, i32] = func(x ty) i32 {\n"
      "  switch x {\n"
      "    default: { return -1; }\n"
      "    default: { return -2; }\n"
      "    case c2(s): {\n"
      "      return s.p + s.q;\n"
      "    }\n"
      "  }\n"
      "};\n");
  /* Fails because of overlapping default cases. */
  pass &= check_negcase(
      im, "check_file_test_more_65b",
      "struct pq { p i32; q i32; }\n"
      "enum ty {\n"
      "  c1 void;\n"
      "  c2 pq;\n"
      "}\n"
      "def foo fn[ty, i32] = func(x ty) i32 {\n"
      "  switch &x {\n"
      "    default: { return -1; }\n"
      "    default: { return -2; }\n"
      "    case &c2(s): {\n"
      "      return s.p + s.q;\n"
      "    }\n"
      "  }\n"
      "};\n");

  pass &= check_foocase(
      im, "check_file_test_more_66",
      "struct foo {\n"
      "  x i32;\n"
      "  y i32;\n"
      "}\n"
      "func bar(x i32) foo {\n"
      "  return { x, x };\n"
      "}\n");
  /* Fails because struct expr has wrong count. */
  pass &= check_negcase(
      im, "check_file_test_more_67",
      "struct foo {\n"
      "  x i32;\n"
      "  y i32;\n"
      "}\n"
      "func bar(x i32) foo {\n"
      "  return { x, x, x };\n"
      "}\n");
  /* Fails because struct expr has wrong type. */
  pass &= check_negcase(
      im, "check_file_test_more_68",
      "struct foo {\n"
      "  x i32;\n"
      "  y i32;\n"
      "}\n"
      "func bar(x i32, y u32) foo {\n"
      "  return { x, y };\n"
      "}\n");
  pass &= check_foocase(
      im, "check_file_test_more_69",
      "struct foo {\n"
      "  x i32;\n"
      "  y i32;\n"
      "}\n"
      "func bar(x i32) foo {\n"
      "  return { x, 5 };\n"
      "}\n");
  pass &= check_foocase(
      im, "check_file_test_more_70",
      "struct foo {\n"
      "  x i32;\n"
      "  y u32;\n"
      "}\n"
      "func bar(x i32) foo {\n"
      "  ret foo = { x, 7 };\n"
      "  return ret;\n"
      "}\n");
  pass &= check_foocase(
      im, "check_file_test_more_71",
      "struct foo {\n"
      "  x i32;\n"
      "  y u32;\n"
      "}\n"
      "func bar(x i32) foo {\n"
      "  return quux({ x, 7 });\n"
      "}\n"
      "func quux(x foo) foo {\n"
      "  return {99, ~x.x};\n"
      "}\n");
  pass &= check_foocase(
      im, "check_file_test_more_72",
      "struct[T] foo { }\n"
      "func[T, U] make(arr U) foo[T] {\n"
      "  return {};\n"
      "}\n"
      "func zed(blah foo[i32]) bool { return true; }\n"
      "func bar() bool {\n"
      "  return zed(make(\"test\"));\n"
      "}\n");
  pass &= check_foocase(
      im, "check_file_test_more_73",
      "struct pq { p i32; q i32; }\n"
      "enum ty {\n"
      "  c1 void;\n"
      "  c2 pq;\n"
      "}\n"
      "def foo fn[ty, i32] = func(x ty) i32 {\n"
      "  if case c2(s pq) = x {\n"
      "    return s.p + s.q;\n"
      "  }\n"
      "  return -1;\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_74",
      "struct pq { p i32; q i32; }\n"
      "enum ty {\n"
      "  c1 void;\n"
      "  c2 pq;\n"
      "}\n"
      "def foo fn[ty, i32] = func(x ty) i32 {\n"
      "  if case c2(s pq) = x {\n"
      "    return s.p + s.q;\n"
      "  } else {\n"
      "    return -1;\n"
      "  }\n"
      "};\n");
  /* Fails because pattern mismatch. */
  pass &= check_negcase(
      im, "check_file_test_more_75",
      "struct pq { p i32; q i32; }\n"
      "struct pq2 { p i32; q i32; }\n"
      "enum ty {\n"
      "  c1 void;\n"
      "  c2 pq;\n"
      "}\n"
      "def foo fn[ty, i32] = func(x ty) i32 {\n"
      "  if case c2(s pq2) = x {\n"
      "    return s.p + s.q;\n"
      "  } else {\n"
      "    return -1;\n"
      "  }\n"
      "};\n");
  pass &= check_foocase(
      im, "check_file_test_more_76",
      "func foo(x i32) ^[2]i32 {\n"
      "  y ^[2]i32 = {x, x + 1};\n"
      "  return {y[1], y[0]};\n"
      "}\n");
  pass &= check_negcase(
      im, "check_file_test_more_77",
      "func foo(x i32) ^[2]i32 {\n"
      "  y ^[2]i32 = {x, x + 1, x + 2};\n"
      "  return {y[1], y[0]};\n"
      "}\n");
  pass &= check_foocase(
      im, "check_file_test_more_78",
      "func foo(n i32) i32 {\n"
      "  if n == 0 {\n"
      "    return 1;\n"
      "  } else {\n"
      "    m i32 = n - 1;\n"
      "    return foo! m * n;\n"
      "  }\n"
      "}\n");

  return pass;
}

int test_check_file(void) {
  /* The way these tests are divided up is nonsense, e.g. 'lambda'
  includes many non-lambda-related test cases. */
  struct identmap im;
  identmap_init(&im);

  int pass = 1;
  pass &= check_file_testcases(&im);
  pass &= check_def_testcases(&im);
  pass &= check_lambda_testcases(&im);
  pass &= check_extern_testcases(&im);
  pass &= check_more_testcases(&im);

  identmap_destroy(&im);
  return pass;
}
