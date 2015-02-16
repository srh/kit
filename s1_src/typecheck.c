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
#include "slice.h"
#include "table.h"
#include "util.h"
#include "x86.h"

#define CHECK_DBG(...) do { } while (0)

struct exprscope;
int lookup_global_maybe_typecheck(struct checkstate *cs,
                                  struct exprscope *also_maybe_typecheck,
                                  struct ast_name_expr *name,
                                  struct ast_typeexpr *partial_type,
                                  int *def_exists_out,
                                  struct ast_typeexpr *out,
                                  int *is_lvalue_out,
                                  struct def_instantiation **inst_out);

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
  /* X86 -- 32-bit sizes */
  intern_primitive_type(cs, SIZE_TYPE_NAME, NULL, 0, 4, 4);

  int not_flatly_held[20] = { 0 };
  /* X86 -- 32-bit pointers */
  intern_primitive_type(cs, PTR_TYPE_NAME, not_flatly_held, 1, 4, 4);
  for (size_t i = 1; i < 21; i++) {
    /* X86 -- 32-bit function pointers */
    intern_primitive_type(cs, FUNC_TYPE_NAME, not_flatly_held, i, 4, 4);
  }
}

void init_func_type(struct ast_typeexpr *a, struct identmap *im,
                    ident_value *args, size_t args_count) {
  a->tag = AST_TYPEEXPR_APP;
  struct ast_ident name
    = make_ast_ident(identmap_intern_c_str(im, FUNC_TYPE_NAME));
  struct ast_typeexpr *params = malloc_mul(sizeof(*params), args_count);
  for (size_t i = 0; i < args_count; i++) {
    params[i].tag = AST_TYPEEXPR_NAME;
    params[i].u.name = make_ast_ident(args[i]);
  }
  ast_typeapp_init(&a->u.app, ast_meta_make_garbage(),
                   name, params, args_count);
}

void init_name_type(struct ast_typeexpr *a, ident_value name) {
  a->tag = AST_TYPEEXPR_NAME;
  a->u.name = make_ast_ident(name);
}

void init_generics_type(struct ast_typeexpr *a,
                        struct ast_ident *name,
                        struct ast_generics *generics) {
  if (!generics->has_type_params) {
    a->tag = AST_TYPEEXPR_NAME;
    ast_ident_init_copy(&a->u.name, name);
  } else {
    struct ast_ident name_copy;
    ast_ident_init_copy(&name_copy, name);

    size_t params_count = generics->params_count;
    struct ast_typeexpr *params = malloc_mul(sizeof(*params), params_count);

    for (size_t i = 0; i < params_count; i++) {
      init_name_type(&params[i], generics->params[i].value);
    }

    a->tag = AST_TYPEEXPR_APP;
    ast_typeapp_init(&a->u.app, ast_meta_make_garbage(),
                     name_copy, params, params_count);
  }
}

struct ast_typeexpr *expose_func_return_type(struct identmap *im,
                                             struct ast_typeexpr *func,
                                             size_t expected_params_count) {
  CHECK(func->tag == AST_TYPEEXPR_APP);
  CHECK(func->u.app.name.value == identmap_intern_c_str(im, FUNC_TYPE_NAME));
  CHECK(func->u.app.params_count == expected_params_count);
  return &func->u.app.params[size_sub(expected_params_count, 1)];
}

void copy_func_return_type(struct identmap *im,
                           struct ast_typeexpr *func,
                           size_t expected_params_count,
                           struct ast_typeexpr *out) {
  ast_typeexpr_init_copy(
      out, expose_func_return_type(im, func, expected_params_count));
}

void init_binop_func_type(struct ast_typeexpr *a, struct identmap *im,
                          const char *type_name) {
  ident_value name = identmap_intern_c_str(im, type_name);
  ident_value names[3];
  names[0] = names[1] = names[2] = name;
  init_func_type(a, im, names, 3);
}

void init_binop_compare_type(struct ast_typeexpr *a, struct identmap *im,
                             const char *type_name) {
  ident_value name = identmap_intern_c_str(im, type_name);
  ident_value bool_name = identmap_intern_c_str(im, BOOL_TYPE_NAME);
  ident_value names[3];
  names[0] = names[1] = name;
  names[2] = bool_name;
  init_func_type(a, im, names, 3);
}

void import_integer_binops(struct checkstate *cs,
                           const enum primitive_op_tag *primop_array,
                           const char *type_name) {
  struct ast_generics generics;
  ast_generics_init_no_params(&generics);
  struct ast_typeexpr binop_type;
  init_binop_func_type(&binop_type, cs->im, type_name);
  for (enum ast_binop op = AST_BINOP_ADD; op < AST_BINOP_LT; op++) {
    intern_binop(cs, op, primop_array, &generics, &binop_type);
  }
  for (enum ast_binop op = AST_BINOP_BIT_XOR;
       op < AST_BINOP_LOGICAL_OR;
       op++) {
    intern_binop(cs, op, primop_array, &generics, &binop_type);
  }
  ast_typeexpr_destroy(&binop_type);
  init_binop_compare_type(&binop_type, cs->im, type_name);
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
  init_binop_func_type(&binop_type, cs->im, BOOL_TYPE_NAME);
  for (enum ast_binop op = AST_BINOP_BIT_XOR;
       op < AST_BINOP_BIT_LEFTSHIFT;
       op++) {
    intern_binop(cs, op, binop_bool_primitive_ops, &generics, &binop_type);
  }
  ast_typeexpr_destroy(&binop_type);
  init_binop_compare_type(&binop_type, cs->im, BOOL_TYPE_NAME);
  for (enum ast_binop op = AST_BINOP_LT; op < AST_BINOP_BIT_XOR; op++) {
    intern_binop(cs, op, binop_bool_primitive_ops, &generics, &binop_type);
  }
  ast_typeexpr_destroy(&binop_type);
  ast_generics_destroy(&generics);
}

void import_integer_conversions(struct checkstate *cs) {
  const size_t num_types = 7;
  ident_value types[7];
  types[0] = identmap_intern_c_str(cs->im, U8_TYPE_NAME);
  types[1] = identmap_intern_c_str(cs->im, I8_TYPE_NAME);
  types[2] = identmap_intern_c_str(cs->im, U16_TYPE_NAME);
  types[3] = identmap_intern_c_str(cs->im, I16_TYPE_NAME);
  types[4] = identmap_intern_c_str(cs->im, U32_TYPE_NAME);
  types[5] = identmap_intern_c_str(cs->im, I32_TYPE_NAME);
  types[6] = identmap_intern_c_str(cs->im, SIZE_TYPE_NAME);

  ident_value convert = identmap_intern_c_str(cs->im, CONVERT_FUNCTION_NAME);

  struct ast_generics generics;
  ast_generics_init_no_params(&generics);

  enum primitive_op_tag conversions[7][7] = {
    {
      PRIMITIVE_OP_CONVERT_U8_TO_U8,
      PRIMITIVE_OP_CONVERT_U8_TO_I8,
      PRIMITIVE_OP_CONVERT_U8_TO_U16,
      PRIMITIVE_OP_CONVERT_U8_TO_I16,
      PRIMITIVE_OP_CONVERT_U8_TO_U32,
      PRIMITIVE_OP_CONVERT_U8_TO_I32,
      PRIMITIVE_OP_CONVERT_U8_TO_SIZE,
    }, {
      PRIMITIVE_OP_CONVERT_I8_TO_U8,
      PRIMITIVE_OP_CONVERT_I8_TO_I8,
      PRIMITIVE_OP_CONVERT_I8_TO_U16,
      PRIMITIVE_OP_CONVERT_I8_TO_I16,
      PRIMITIVE_OP_CONVERT_I8_TO_U32,
      PRIMITIVE_OP_CONVERT_I8_TO_I32,
      PRIMITIVE_OP_CONVERT_I8_TO_SIZE,
    }, {
      PRIMITIVE_OP_CONVERT_U16_TO_U8,
      PRIMITIVE_OP_CONVERT_U16_TO_I8,
      PRIMITIVE_OP_CONVERT_U16_TO_U16,
      PRIMITIVE_OP_CONVERT_U16_TO_I16,
      PRIMITIVE_OP_CONVERT_U16_TO_U32,
      PRIMITIVE_OP_CONVERT_U16_TO_I32,
      PRIMITIVE_OP_CONVERT_U16_TO_SIZE,
    }, {
      PRIMITIVE_OP_CONVERT_I16_TO_U8,
      PRIMITIVE_OP_CONVERT_I16_TO_I8,
      PRIMITIVE_OP_CONVERT_I16_TO_U16,
      PRIMITIVE_OP_CONVERT_I16_TO_I16,
      PRIMITIVE_OP_CONVERT_I16_TO_U32,
      PRIMITIVE_OP_CONVERT_I16_TO_I32,
      PRIMITIVE_OP_CONVERT_I16_TO_SIZE,
    }, {
      PRIMITIVE_OP_CONVERT_U32_TO_U8,
      PRIMITIVE_OP_CONVERT_U32_TO_I8,
      PRIMITIVE_OP_CONVERT_U32_TO_U16,
      PRIMITIVE_OP_CONVERT_U32_TO_I16,
      PRIMITIVE_OP_CONVERT_U32_TO_U32,
      PRIMITIVE_OP_CONVERT_U32_TO_I32,
      PRIMITIVE_OP_CONVERT_U32_TO_SIZE,
    }, {
      PRIMITIVE_OP_CONVERT_I32_TO_U8,
      PRIMITIVE_OP_CONVERT_I32_TO_I8,
      PRIMITIVE_OP_CONVERT_I32_TO_U16,
      PRIMITIVE_OP_CONVERT_I32_TO_I16,
      PRIMITIVE_OP_CONVERT_I32_TO_U32,
      PRIMITIVE_OP_CONVERT_I32_TO_I32,
      PRIMITIVE_OP_CONVERT_I32_TO_SIZE,
    }, {
      PRIMITIVE_OP_CONVERT_SIZE_TO_U8,
      PRIMITIVE_OP_CONVERT_SIZE_TO_I8,
      PRIMITIVE_OP_CONVERT_SIZE_TO_U16,
      PRIMITIVE_OP_CONVERT_SIZE_TO_I16,
      PRIMITIVE_OP_CONVERT_SIZE_TO_U32,
      PRIMITIVE_OP_CONVERT_SIZE_TO_I32,
      PRIMITIVE_OP_CONVERT_SIZE_TO_SIZE,
    },
  };

  for (size_t i = 0; i < num_types; i++) {
    for (size_t j = 0; j < num_types; j++) {
      struct ast_typeexpr func_type;
      ident_value names[2];
      names[0] = types[i];
      names[1] = types[j];
      init_func_type(&func_type, cs->im, names, 2);
      name_table_add_primitive_def(cs->im,
                                   &cs->nt,
                                   convert,
                                   make_primop(conversions[i][j]),
                                   &generics,
                                   &func_type);
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
  init_func_type(&type, cs->im, args, 2);
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

void import_sizeof_alignof(struct checkstate *cs) {
  struct ast_ident *param = malloc_mul(sizeof(*param), 1);
  param[0] = make_ast_ident(identmap_intern_c_str(cs->im, "T"));

  struct ast_generics generics;
  ast_generics_init_has_params(&generics, ast_meta_make_garbage(), param, 1);
  struct ast_typeexpr type;
  init_name_type(&type, identmap_intern_c_str(cs->im, SIZE_TYPE_NAME));

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
                        struct ast_typeexpr *out) {
  size_t params_count = size_add(count, 1);
  struct ast_typeexpr *params = malloc_mul(sizeof(*params), params_count);
  for (size_t i = 0; i < count; i++) {
    wrap_in_ptr(cs->im, target, &params[i]);
  }

  init_name_type(&params[count], identmap_intern_c_str(cs->im, VOID_TYPE_NAME));

  struct ast_ident name
    = make_ast_ident(identmap_intern_c_str(cs->im, FUNC_TYPE_NAME));
  out->tag = AST_TYPEEXPR_APP;
  ast_typeapp_init(&out->u.app, ast_meta_make_garbage(),
                   name, params, params_count);
}

void import_constructors(struct checkstate *cs) {
  ident_value t_ident = identmap_intern_c_str(cs->im, "T");
  struct ast_generics generics;
  {
    struct ast_ident *param = malloc_mul(sizeof(*param), 1);
    param[0] = make_ast_ident(t_ident);
    ast_generics_init_has_params(&generics, ast_meta_make_garbage(), param, 1);
  }

  struct ast_typeexpr target;
  init_name_type(&target, t_ident);

  {
    struct ast_typeexpr func1;
    make_ptr_func_type(cs, &target, 1, &func1);

    name_table_add_primitive_def(cs->im,
                                 &cs->nt,
                                 identmap_intern_c_str(cs->im, "init"),
                                 make_primop(PRIMITIVE_OP_INIT),
                                 &generics,
                                 &func1);
    name_table_add_primitive_def(cs->im,
                                 &cs->nt,
                                 identmap_intern_c_str(cs->im, "destroy"),
                                 make_primop(PRIMITIVE_OP_DESTROY),
                                 &generics,
                                 &func1);

    ast_typeexpr_destroy(&func1);
  }

  {
    struct ast_typeexpr func2;
    make_ptr_func_type(cs, &target, 2, &func2);

    name_table_add_primitive_def(cs->im,
                                 &cs->nt,
                                 identmap_intern_c_str(cs->im, "move"),
                                 make_primop(PRIMITIVE_OP_MOVE),
                                 &generics,
                                 &func2);
    name_table_add_primitive_def(cs->im,
                                 &cs->nt,
                                 identmap_intern_c_str(cs->im, "copy"),
                                 make_primop(PRIMITIVE_OP_COPY),
                                 &generics,
                                 &func2);

    ast_typeexpr_destroy(&func2);
  }

  ast_typeexpr_destroy(&target);
  ast_generics_destroy(&generics);
}

void checkstate_import_primitive_defs(struct checkstate *cs) {
  import_integer_binops(cs, binop_u8_primitive_ops, U8_TYPE_NAME);
  import_integer_binops(cs, binop_i8_primitive_ops, I8_TYPE_NAME);
  import_integer_binops(cs, binop_u16_primitive_ops, U16_TYPE_NAME);
  import_integer_binops(cs, binop_i16_primitive_ops, I16_TYPE_NAME);
  import_integer_binops(cs, binop_u32_primitive_ops, U32_TYPE_NAME);
  import_integer_binops(cs, binop_i32_primitive_ops, I32_TYPE_NAME);
  import_integer_binops(cs, binop_size_primitive_ops, SIZE_TYPE_NAME);

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
  }

  import_sizeof_alignof(cs);

  import_constructors(cs);
}

void checkstate_import_primitives(struct checkstate *cs) {
  checkstate_import_primitive_types(cs);
  checkstate_import_primitive_defs(cs);
}

void init_boolean_typeexpr(struct checkstate *cs, struct ast_typeexpr *a) {
  a->tag = AST_TYPEEXPR_NAME;
  a->u.name = make_ast_ident(identmap_intern_c_str(cs->im, BOOL_TYPE_NAME));
}

int resolve_import_filename_and_parse(struct checkstate *cs,
                                      module_loader *loader,
                                      ident_value name,
                                      struct ast_file *file_out) {
  int ret = 0;

  const void *module_name;
  size_t module_name_count;
  identmap_lookup(cs->im, name, &module_name, &module_name_count);

  uint8_t *data;
  size_t data_size;
  if (!(*loader)(module_name, module_name_count, &data, &data_size)) {
    ERR("Could not read file for module %.*s.\n",
        size_to_int(module_name_count), (const char *)module_name);
    goto fail;
  }

  struct error_info error_info;
  if (!parse_buf_file(cs->im, data, data_size, file_out, &error_info)) {
    ERR("Parse error in module %.*s at %"PRIz":%"PRIz".\n",
        size_to_int(module_name_count), (const char *)module_name,
        error_info.pos.line, error_info.pos.column);
    error_info_destroy(&error_info);
    goto fail_data;
  }

  ret = 1;
 fail_data:
  free(data);
 fail:
  return ret;
}

struct import_chase_state {
  ident_value *names;
  size_t names_count;
  size_t names_limit;

  struct defclass_ident *accessible;
  size_t accessible_count;
  size_t accessible_limit;
};

void defclass_ident_destroy(struct defclass_ident *dci) {
  (void)dci;
  /* Do nothing. */
}

void copy_make_unary_func_type(struct checkstate *cs,
                               struct ast_typeexpr *arg_type,
                               struct ast_typeexpr *return_type,
                               struct ast_typeexpr *out) {

  struct ast_ident name = make_ast_ident(identmap_intern_c_str(cs->im, FUNC_TYPE_NAME));
  struct ast_typeexpr *params = malloc_mul(sizeof(*params), 2);
  ast_typeexpr_init_copy(&params[0], arg_type);
  ast_typeexpr_init_copy(&params[1], return_type);

  out->tag = AST_TYPEEXPR_APP;
  ast_typeapp_init(&out->u.app, ast_meta_make_garbage(), name,
                   params, 2);
}

int add_enum_constructors(struct checkstate *cs,
                          struct ast_generics *generics,
                          struct ast_ident *name,
                          struct ast_enumspec *enumspec) {
  for (size_t i = 0, e = enumspec->enumfields_count; i < e; i++) {
    struct ast_vardecl *f = &enumspec->enumfields[i];

    if (f->name.value == name->value) {
      METERR(f->name.meta, "enum constructor %.*s matches enum name\n",
             IM_P(cs->im, f->name.value));
      return 0;
    }

    for (size_t j = 0; j < i; j++) {
      if (f->name.value == enumspec->enumfields[j].name.value) {
        METERR(f->name.meta, "enum constructor %.*s with duplicate name.\n",
               IM_P(cs->im, f->name.value));
        return 0;
      }
    }

    struct ast_typeexpr *params = malloc_mul(sizeof(*params), 2);
    ast_typeexpr_init_copy(&params[0], &f->type);

    if (!generics->has_type_params) {
      params[1].tag = AST_TYPEEXPR_NAME;
      ast_ident_init_copy(&params[1].u.name, name);
    } else {
      struct ast_typeexpr *app_params = malloc_mul(sizeof(*app_params), generics->params_count);
      for (size_t j = 0, je = generics->params_count; j < je; j++) {
        app_params[j].tag = AST_TYPEEXPR_NAME;
        ast_ident_init_copy(&app_params[j].u.name, &generics->params[j]);
      }
      struct ast_ident name_copy;
      ast_ident_init_copy(&name_copy, name);
      params[1].tag = AST_TYPEEXPR_APP;
      ast_typeapp_init(&params[1].u.app, ast_meta_make_garbage(),
                       name_copy, app_params, generics->params_count);
    }

    struct ast_typeexpr func_type;
    func_type.tag = AST_TYPEEXPR_APP;
    ast_typeapp_init(&func_type.u.app, ast_meta_make_garbage(),
                     make_ast_ident(identmap_intern_c_str(cs->im, FUNC_TYPE_NAME)),
                     params, 2);

    int success = name_table_add_primitive_def(
        cs->im,
        &cs->nt,
        f->name.value,
        make_enumconstruct_op(i),
        generics,
        &func_type);
    ast_typeexpr_destroy(&func_type);
    if (!success) {
      return 0;
    }
  }

  return 1;
}

int make_complete_lambda_typeexpr(struct identmap *im, struct ast_expr *a, struct ast_typeexpr *out) {
  switch (a->tag) {
  case AST_EXPR_LAMBDA: {
    struct ast_lambda *lam = &a->u.lambda;

    size_t args_count = size_add(lam->params_count, 1);
    struct ast_typeexpr *args = malloc_mul(sizeof(*args), args_count);
    for (size_t i = 0, e = lam->params_count; i < e; i++) {
      ast_typeexpr_init_copy(&args[i], &lam->params[i].type);
    }
    ast_typeexpr_init_copy(&args[lam->params_count], &lam->return_type);

    out->tag = AST_TYPEEXPR_APP;
    ast_typeapp_init(&out->u.app, ast_meta_make_garbage(),
                     make_ast_ident(identmap_intern_c_str(im, FUNC_TYPE_NAME)),
                     args, args_count);
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
      SLICE_PUSH(ics->names, ics->names_count, ics->names_limit,
                 toplevel->u.import.name.value);
    } break;
    case AST_TOPLEVEL_DEF: {
      if (!toplevel->u.def.has_typeexpr) {
        if (!make_complete_lambda_typeexpr(cs->im, &toplevel->u.def.rhs, &toplevel->u.def.typeexpr)) {
          METERR(toplevel->u.def.meta, "Incomplete type for def %.*s\n",
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
                              ics->accessible,
                              ics->accessible_count,
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
      SLICE_PUSH(ics->accessible, ics->accessible_count, ics->accessible_limit, dci);
      int success = chase_through_toplevels(cs, ics,
                                            toplevel->u.access.toplevels,
                                            toplevel->u.access.toplevels_count);
      SLICE_POP(ics->accessible, ics->accessible_count, defclass_ident_destroy);
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

int chase_imports(struct checkstate *cs, module_loader *loader,
                  ident_value name) {
  int ret = 0;
  struct import_chase_state ics = { NULL, 0, 0, NULL, 0, 0 };

  SLICE_PUSH(ics.names, ics.names_count, ics.names_limit, name);

  while (ics.names_count) {
    name = ics.names[--ics.names_count];

    for (size_t i = 0, e = cs->imports_count; i < e; i++) {
      if (cs->imports[i].import_name == name) {
        goto continue_outer;
      }
    }

    struct ast_file file;
    if (!resolve_import_filename_and_parse(cs, loader, name, &file)) {
      goto cleanup;
    }

    struct ast_file *heap_file = malloc(sizeof(*heap_file));
    CHECK(heap_file);
    *heap_file = file;
    struct import imp;
    imp.import_name = name;
    imp.file = heap_file;
    SLICE_PUSH(cs->imports, cs->imports_count, cs->imports_limit, imp);

    if (!chase_through_toplevels(cs, &ics, heap_file->toplevels, heap_file->toplevels_count)) {
      goto cleanup;
    }

  continue_outer:
    continue;
  }

  ret = 1;
 cleanup:
  free(ics.names);
  free(ics.accessible);
  return ret;
}

int lookup_import(struct checkstate *cs, ident_value name,
                  struct ast_file **file_out) {
  for (size_t i = 0, e = cs->imports_count; i < e; i++) {
    if (cs->imports[i].import_name == name) {
      *file_out = cs->imports[i].file;
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
  for (size_t i = 0, e = a->params_count; i < e; i++) {
    if (a->params[i].value == name) {
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
      METERR(a->meta, "Unrecognized type name '%.*s'.\n", IM_P(cs->im, name));
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
                                 param_list_arity(a->params_count),
                                 &ent)) {
    METERR(a->meta, "Type lookup fail for %.*s, arity %"PRIz"\n",
           IM_P(cs->im, a->name.value), a->params_count);
    return 0;
  }

  if (flat_typeexpr) {
    if (!check_deftype(cs, ent)) {
      return 0;
    }
  }

  for (size_t i = 0, e = a->params_count; i < e; i++) {
    int f = deftype_entry_param_is_flatly_held(ent, i);
    if (!check_typeexpr(cs, generics, &a->params[i],
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
        METERR(field->meta, "struct/union fields have duplicate name %.*s\n",
               IM_P(cs->im, field->name.value));
        return 0;
      }
    }

    {
      size_t which_generic;
      if (generics_lookup_name(generics, field->name.value, &which_generic)) {
        METERR(field->meta,
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
  struct ast_typeexpr *params = malloc_mul(sizeof(*params), params_count);
  for (size_t i = 0; i < count; i++) {
    wrap_in_ptr(cs->im, a, &params[i]);
  }
  /* Unknown return type -- we'll match anything named
  "copy"/"move"/"destroy" with any return type. */
  params[count] = ast_unknown_garbage();

  struct ast_ident func_name = make_ast_ident(identmap_intern_c_str(cs->im, FUNC_TYPE_NAME));

  out->tag = AST_TYPEEXPR_APP;
  ast_typeapp_init(&out->u.app, ast_meta_make_garbage(), func_name, params, params_count);
}

/* Returns false if multiple matching definitions. */
int has_explicit_movecopydestroy(struct checkstate *cs,
                                 struct ast_typeexpr *a,
                                 /* copy/move takes 2, destroy takes 1. */
                                 size_t argdupes,
                                 const char *name,
                                 struct exprscope *also_typecheck,
                                 int *result_out,
                                 struct def_instantiation **inst_out) {
  struct ast_typeexpr func_type;
  make_pointee_func_lookup_type(cs, a, argdupes, &func_type);

  struct ast_name_expr func_name;
  ast_name_expr_init(&func_name, make_ast_ident(identmap_intern_c_str(cs->im, name)));

  size_t matching_defs = name_table_count_matching_defs(&cs->nt,
                                                        &func_name.ident,
                                                        NULL,
                                                        0,
                                                        &func_type);

  if (matching_defs < 2) {
    struct def_instantiation *inst = NULL;
    if (matching_defs == 1) {
      int def_exists_discard;
      struct ast_typeexpr unified;
      int lvalue_discard;
      if (!lookup_global_maybe_typecheck(cs,
                                         also_typecheck,
                                         &func_name,
                                         &func_type,
                                         &def_exists_discard,
                                         &unified,
                                         &lvalue_discard,
                                         &inst)) {
        METERR(*ast_typeexpr_meta(a), "Typecheck failed(?) when looking up %s definition.\n",
               name);
        goto fail;
      }
      ast_typeexpr_destroy(&unified);
    }

    ast_name_expr_destroy(&func_name);
    ast_typeexpr_destroy(&func_type);

    *result_out = (matching_defs == 1);
    *inst_out = inst;
    return 1;
  }

  METERR(*ast_typeexpr_meta(a), "Multiple matching '%s' definitions\n", name);
 fail:
  ast_name_expr_destroy(&func_name);
  ast_typeexpr_destroy(&func_type);
  return 0;
}

int has_explicit_copy(struct checkstate *cs, struct ast_typeexpr *a, struct exprscope *also_typecheck, int *result_out,
                      struct def_instantiation **inst_out) {
  return has_explicit_movecopydestroy(cs, a, 2, "do_copy", also_typecheck, result_out, inst_out);
}

int has_explicit_destroy(struct checkstate *cs, struct ast_typeexpr *a, struct exprscope *also_typecheck, int *result_out,
                         struct def_instantiation **inst_out) {
  return has_explicit_movecopydestroy(cs, a, 1, "do_destroy", also_typecheck, result_out, inst_out);
}

int has_explicit_move(struct checkstate *cs, struct ast_typeexpr *a, struct exprscope *also_typecheck, int *result_out,
                      struct def_instantiation **inst_out) {
  return has_explicit_movecopydestroy(cs, a, 2, "do_move", also_typecheck, result_out, inst_out);
}

int has_explicit_init(struct checkstate *cs, struct ast_typeexpr *a, struct exprscope *also_typecheck, int *result_out,
                      struct def_instantiation **inst_out) {
  return has_explicit_movecopydestroy(cs, a, 1, "do_init", also_typecheck, result_out, inst_out);
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

  struct typeexpr_traits rhs_traits = { 0 };
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
  if (!has_explicit_move(cs, a, also_typecheck, &explicit_move, &move_inst)) {
    return 0;
  }

  int explicit_copy;
  struct def_instantiation *copy_inst;
  if (!has_explicit_copy(cs, a, also_typecheck, &explicit_copy, &copy_inst)) {
    return 0;
  }

  int explicit_destroy;
  struct def_instantiation *destroy_inst;
  if (!has_explicit_destroy(cs, a, also_typecheck, &explicit_destroy, &destroy_inst)) {
    return 0;
  }

  int explicit_init;
  struct def_instantiation *init_inst;
  if (!has_explicit_init(cs, a, also_typecheck, &explicit_init, &init_inst)) {
    return 0;
  }

  if (lookup_move) {
    rhs_traits.movable = explicit_move ? TYPEEXPR_TRAIT_HAD : TYPEEXPR_TRAIT_LACKED;
  } else {
    if (explicit_move) {
      METERR(*deftype_meta, "Type has both implicit and explicit move.%s", "\n");
      return 0;
    }
  }

  if (lookup_copy) {
    rhs_traits.copyable = explicit_copy ? TYPEEXPR_TRAIT_HAD : TYPEEXPR_TRAIT_LACKED;
    if (!explicit_destroy) {
      METERR(*deftype_meta, "Type lacks explicit destructor.%s", "\n");
      return 0;
    }
  } else {
    if (explicit_copy) {
      METERR(*deftype_meta, "Type has both implicit and explicit copy.%s", "\n");
      return 0;
    }
    if (explicit_destroy) {
      METERR(*deftype_meta, "Type has both implicit and explicit destroy.%s", "\n");
      return 0;
    }
  }

  if (lookup_init) {
    rhs_traits.inittible = explicit_init ? TYPEEXPR_TRAIT_HAD : TYPEEXPR_TRAIT_LACKED;
  } else {
    if (explicit_init) {
      METERR(*deftype_meta, "Type has both implicit and explicit init.%s", "\n");
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
  if (!name_table_lookup_deftype(&cs->nt, a->u.name.value,
                                 no_param_list_arity(),
                                 &ent)) {
    METERR(*ast_typeexpr_meta(a), "Invalid type name %.*s found.\n", IM_P(cs->im, a->u.name.value));
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

  int ret = finish_checking_name_traits(cs,
                                        a,
                                        &ent->deftype->meta,
                                        ent->deftype->disposition,
                                        &ent->deftype->rhs,
                                        also_typecheck,
                                        out,
                                        insts_out);
  if (ret && concrete_deftype_rhs_out_or_null) {
    *has_concrete_deftype_rhs_out_or_null = 1;
    ast_deftype_rhs_init_copy(concrete_deftype_rhs_out_or_null, &ent->deftype->rhs);
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
  if (!name_table_lookup_deftype(&cs->nt, a->u.app.name.value,
                                 param_list_arity(a->u.app.params_count),
                                 &ent)) {
    CRASH("an invalid generic type");
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

  struct ast_deftype *deftype = ent->deftype;

  CHECK(deftype->generics.has_type_params
        && deftype->generics.params_count == a->u.app.params_count);

  struct ast_deftype_rhs concrete_deftype_rhs;
  do_replace_rhs_generics(&deftype->generics,
                          a->u.app.params,
                          a->u.app.params_count,
                          &deftype->rhs,
                          &concrete_deftype_rhs);

  int ret = finish_checking_name_traits(cs,
                                        a,
                                        &deftype->meta,
                                        deftype->disposition,
                                        &concrete_deftype_rhs,
                                        also_typecheck,
                                        out,
                                        insts_out);
  if (ret && concrete_deftype_rhs_out_or_null) {
    *has_concrete_deftype_rhs_out_or_null = 1;
    *concrete_deftype_rhs_out_or_null = concrete_deftype_rhs;
  } else {
    ast_deftype_rhs_destroy(&concrete_deftype_rhs);
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
  (void)also_typecheck;

  for (size_t i = 0, e = a->u.unione.fields_count; i < e; i++) {
    /* We don't typecheck the traits since we know they must have
    trivial copy constructors. */
    struct typeexpr_traits traits;
    if (!check_typeexpr_traits(cs, &a->u.unione.fields[i].type, NULL, &traits)) {
      return 0;
    }

    if (traits.movable != TYPEEXPR_TRAIT_TRIVIALLY_HAD) {
      METERR(a->u.unione.meta, "Union field %.*s is not trivially movable.\n",
             IM_P(cs->im, a->u.unione.fields[i].name.value));
      return 0;
    }

    if (traits.copyable != TYPEEXPR_TRAIT_TRIVIALLY_HAD) {
      METERR(a->u.unione.meta, "Union field %.*s is not trivially copyable.\n",
             IM_P(cs->im, a->u.unione.fields[i].name.value));
      return 0;
    }

    if (traits.inittible != TYPEEXPR_TRAIT_TRIVIALLY_HAD) {
      METERR(a->u.unione.meta, "Union field %.*s is not trivially initializable.\n",
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
    METERR(*ast_typeexpr_meta(a), "Incomplete type in bad place.%s", "\n");
    return 0;
  } break;
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
    out->inittible = TYPEEXPR_TRAIT_LACKED;
  }
  return ret;
}



int check_generics_shadowing(struct checkstate *cs,
                             struct ast_generics *a) {
  if (!a->has_type_params) {
    return 1;
  }

  for (size_t i = 0, e = a->params_count; i < e; i++) {
    ident_value name = a->params[i].value;
    for (size_t j = 0; j < i; j++) {
      if (name == a->params[j].value) {
        METERR(a->meta, "duplicate param names %.*s within same generics list.\n",
               IM_P(cs->im, name));
        return 0;
      }
    }

    if (name_table_shadowed(&cs->nt, name)) {
      METERR(a->meta, "generics list shadows global name %.*s.\n",
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
    METERR(a->meta, "deftype %.*s recursively held.\n",
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
  struct varnum varnum;
};

struct var_by_varnum {
  struct ast_typeexpr concrete_type;
};

void var_by_varnum_init(struct var_by_varnum *v,
                        struct ast_typeexpr *concrete_type_copyee) {
  ast_typeexpr_init_copy(&v->concrete_type, concrete_type_copyee);
}

void var_by_varnum_destroy(struct var_by_varnum *v) {
  ast_typeexpr_destroy(&v->concrete_type);
}

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
  /* TODO: We could probably have a stack of varnums that are in scope
  instead.  Edit: Or get rid of varnums?  They lost their luster when
  we got rid of labels and gotos. */
  struct varpair *vars;
  size_t vars_count;
  size_t vars_limit;

  size_t varnum_counter;

  struct var_by_varnum *vars_by_varnum;
  size_t vars_by_varnum_count;
  size_t vars_by_varnum_limit;

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
  CHECK(generics->params_count == (generics->has_type_params ?
                                   generics_substitutions_count : 0));
  es->cs = cs;
  es->generics = generics;
  es->generics_substitutions = generics_substitutions;
  es->generics_substitutions_count = generics_substitutions_count;
  es->accessible = accessible;
  es->accessible_count = accessible_count;
  es->computation = computation;
  es->entry_or_null = entry_or_null;
  es->vars = NULL;
  es->vars_count = 0;
  es->vars_limit = 0;
  es->varnum_counter = 0;
  es->vars_by_varnum = NULL;
  es->vars_by_varnum_count = 0;
  es->vars_by_varnum_limit = 0;
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
  free(es->vars);
  es->vars = NULL;
  es->vars_count = 0;
  es->vars_limit = 0;
  es->varnum_counter = 0;
  SLICE_FREE(es->vars_by_varnum, es->vars_by_varnum_count, var_by_varnum_destroy);
  es->vars_by_varnum_limit = 0;
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
  for (size_t i = 0, e = es->vars_count; i < e; i++) {
    if (es->vars[i].decl->name.value == name->value) {
      METERR(name->meta, "Variable name %.*s shadows local.\n",
             IM_P(es->cs->im, name->value));
      return 0;
    }
  }

  {
    size_t which_generic;
    if (generics_lookup_name(es->generics, name->value, &which_generic)) {
      METERR(name->meta, "Variable name %.*s shadows template parameter, which is gauche.\n",
             IM_P(es->cs->im, name->value));
      return 0;
    }
  }

  /* TODO: Decide whether to get rid of this. */
#if 0
  if (name_table_shadowed(&es->cs->nt, name->value)) {
    METERR(name->meta, "Variable name %.*s shadows a global def or type.\n",
           IM_P(es->cs->im, name->value));
    return 0;
  }
#endif

  return 1;
}

int exprscope_push_var(struct exprscope *es, struct ast_vardecl *var,
                       struct varnum *varnum_out) {
  if (!check_var_shadowing(es, &var->name)) {
    return 0;
  }

  struct varnum varnum;
  varnum.value = es->varnum_counter;
  es->varnum_counter = size_add(es->varnum_counter, 1);


  struct varpair pair;
  pair.decl = var;
  pair.varnum = varnum;
  SLICE_PUSH(es->vars, es->vars_count, es->vars_limit, pair);

  CHECK(varnum.value == es->vars_by_varnum_count);
  struct var_by_varnum vbv;
  var_by_varnum_init(&vbv, &var->type);
  SLICE_PUSH(es->vars_by_varnum, es->vars_by_varnum_count, es->vars_by_varnum_limit, vbv);

  *varnum_out = varnum;
  return 1;
}

void exprscope_pop_var(struct exprscope *es) {
  CHECK(es->vars_count > 0);
  --es->vars_count;
  es->vars[es->vars_count].decl = NULL;
  es->vars[es->vars_count].varnum.value = SIZE_MAX;
}

int unify_fields_directionally(struct ast_vardecl *partial_fields,
                               size_t partial_fields_count,
                               struct ast_vardecl *complete_fields,
                               size_t complete_fields_count) {
  if (partial_fields_count != complete_fields_count) {
    return 0;
  }

  for (size_t i = 0; i < partial_fields_count; i++) {
    if (partial_fields[i].name.value != complete_fields[i].name.value
        || !unify_directionally(&partial_fields[i].type,
                                &complete_fields[i].type)) {
      return 0;
    }
  }

  return 1;
}

int unify_directionally(struct ast_typeexpr *partial_type,
                        struct ast_typeexpr *complete_type) {
  CHECK(complete_type->tag != AST_TYPEEXPR_UNKNOWN);
  if (partial_type->tag == AST_TYPEEXPR_UNKNOWN) {
    return 1;
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
        || p_app->params_count != c_app->params_count) {
      return 0;
    }
    for (size_t i = 0, e = p_app->params_count; i < e; i++) {
      if (!unify_directionally(&p_app->params[i], &c_app->params[i])) {
        return 0;
      }
    }
    return 1;
  } break;
  case AST_TYPEEXPR_STRUCTE:
    return unify_fields_directionally(partial_type->u.structe.fields,
                                      partial_type->u.structe.fields_count,
                                      complete_type->u.structe.fields,
                                      complete_type->u.structe.fields_count);
  case AST_TYPEEXPR_UNIONE:
    return unify_fields_directionally(partial_type->u.unione.fields,
                                      partial_type->u.unione.fields_count,
                                      complete_type->u.unione.fields,
                                      complete_type->u.unione.fields_count);
  case AST_TYPEEXPR_ARRAY: {
    if (partial_type->u.arraytype.count != complete_type->u.arraytype.count) {
      return 0;
    }
    return unify_directionally(partial_type->u.arraytype.param,
                               complete_type->u.arraytype.param);
  } break;
  default:
    UNREACHABLE();
  }
}

int exact_typeexprs_equal(struct ast_typeexpr *a, struct ast_typeexpr *b) {
  return unify_directionally(a, b) && unify_directionally(b, a);
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
  ALLOW_INCOMPLETE_YES,
  ALLOW_INCOMPLETE_NO,
};

int check_expr(struct exprscope *es,
               struct ast_expr *x,
               struct ast_typeexpr *partial_type,
               struct ast_expr *annotated_out);

int check_expr_ai(struct exprscope *es,
                  enum allow_incomplete ai,
                  struct ast_expr *x,
                  struct ast_typeexpr *partial_type,
                  struct ast_expr *annotated_out);

/* TODO: Right now still nothing uses def_exists. */
int lookup_global_maybe_typecheck(struct checkstate *cs,
                                  struct exprscope *also_maybe_typecheck,
                                  struct ast_name_expr *name,
                                  struct ast_typeexpr *partial_type,
                                  /* Set even if the returns false, it
                                  means the def existed but failed
                                  access/typechecking.  Otherwise,
                                  means the def doesn't exist. */
                                  int *def_exists_out,
                                  struct ast_typeexpr *out,
                                  int *is_lvalue_out,
                                  struct def_instantiation **inst_out) {
  struct ast_typeexpr unified;
  struct def_entry *ent;
  struct def_instantiation *inst;
  int zero_defs;
  if (!name_table_match_def(cs->im,
                            &cs->nt,
                            &name->ident,
                            name->has_params ? name->params : NULL,
                            name->has_params ? name->params_count : 0,
                            partial_type,
                            &zero_defs,
                            &unified,
                            &ent,
                            &inst)) {
    *def_exists_out = !zero_defs;
    return 0;
  }
  *def_exists_out = 1;

  if (!also_maybe_typecheck) {
    *out = unified;
    /* Right now, there are no global variables. */
    *is_lvalue_out = 0;
    *inst_out = inst;
    return 1;
  }

  for (size_t i = 0, e = ent->private_to_count; i < e; i++) {
    if (!is_accessible(also_maybe_typecheck, ent->private_to[i])) {
      METERR(name->meta, "Access denied.%s", "\n");
      goto fail_unified;
    }
  }

  exprscope_note_static_reference(also_maybe_typecheck, ent);

  if (!ent->is_primitive && !ent->is_extern
      && ent->generics.has_type_params && !inst->typecheck_started) {
    CHECK(ent->def);
    CHECK(!inst->annotated_rhs_computed);
    if (cs->template_instantiation_recursion_depth
        == MAX_TEMPLATE_INSTANTIATION_RECURSION_DEPTH) {
      METERR(ent->def->meta, "Max template instantiation recursion depth exceeded.%s", "\n");
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
    if (!check_expr(&scope, &ent->def->rhs, &unified, &annotated_rhs)) {
      exprscope_destroy(&scope);
      cs->template_instantiation_recursion_depth--;
      DBG("... when instantiating '%.*s'.\n", IM_P(cs->im, ent->name));
      goto fail_unified;
    }

    /* We just assume there's no temporaries, no constructors or
    destructors, because the expression was statically computable. */

    di_set_annotated_rhs(inst, annotated_rhs);

    exprscope_destroy(&scope);
    cs->template_instantiation_recursion_depth--;
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
                          struct def_instantiation **inst_or_null_out) {
  for (size_t i = es->vars_count; i-- > 0; ) {
    struct ast_vardecl *decl = es->vars[i].decl;
    if (decl->name.value != name->ident.value) {
      continue;
    }

    if (!unify_directionally(partial_type, &decl->type)) {
      METERR(name->meta, "Type mismatch for vardecl %.*s lookup.\n",
             IM_P(es->cs->im, name->ident.value));
      return 0;
    }

    ast_typeexpr_init_copy(out, &decl->type);
    *is_lvalue_out = 1;
    *inst_or_null_out = NULL;  /* NULL because it's a local. */
    return 1;
  }

  /* inst_or_null_out gets initialized to a non-NULL value. */
  int def_exists_discard;
  return lookup_global_maybe_typecheck(es->cs, es, name, partial_type, &def_exists_discard,
                                       out, is_lvalue_out, inst_or_null_out);
}

void numeric_literal_type(struct identmap *im,
                          struct ast_numeric_literal *a,
                          struct ast_typeexpr *out) {
  out->tag = AST_TYPEEXPR_NAME;
  const char *type_name;
  switch (a->numeric_type) {
  case AST_NUMERIC_TYPE_SIGNED:
    type_name = I32_TYPE_NAME;
    break;
  case AST_NUMERIC_TYPE_UNSIGNED:
    type_name = U32_TYPE_NAME;
    break;
  default:
    UNREACHABLE();
  }
  out->u.name = make_ast_ident(identmap_intern_c_str(im, type_name));
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
  CHECK(generics->params_count == generics_substitutions_count);
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
    size_t params_count = app->params_count;
    struct ast_typeexpr *params = malloc_mul(sizeof(*params),
                                             params_count);

    for (size_t i = 0, e = params_count; i < e; i++) {
      do_replace_generics(generics, generics_substitutions,
                          generics_substitutions_count,
                          &app->params[i], &params[i]);
    }

    struct ast_ident name;
    ast_ident_init_copy(&name, &app->name);

    out->tag = AST_TYPEEXPR_APP;
    ast_typeapp_init(&out->u.app, ast_meta_make_copy(&a->u.app.meta),
                     name, params, params_count);
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
    ast_arraytype_init(&out->u.arraytype, ast_meta_make_copy(&a->u.arraytype.meta),
                       a->u.arraytype.count, param);
  } break;
  case AST_TYPEEXPR_UNKNOWN: {
    out->tag = AST_TYPEEXPR_UNKNOWN;
    ast_unknown_init(&out->u.unknown, ast_meta_make_copy(&a->u.unknown.meta));
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
  CHECK(generics->params_count == generics_substitutions_count);
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
        METERR(*ast_expr_ast_meta(annotated_expr),
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
      METERR(*ast_expr_ast_meta(annotated_expr),
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
                                  struct ast_expr **exprs_annotated_out) {
  struct ast_typeexpr local_partial = ast_unknown_garbage();
  struct ast_expr *exprs_annotated = malloc_mul(sizeof(*exprs_annotated),
                                                args_count);
  size_t i;
  for (i = 0; i < args_count; i++) {
    struct ast_expr arg_expr_annotated;
    if (!check_expr(es, &args[i].expr, &local_partial, &arg_expr_annotated)) {
      goto fail_exprs_annotated;
    }
    exprs_annotated[i] = arg_expr_annotated;
  }

  *exprs_annotated_out = exprs_annotated;
  ast_typeexpr_destroy(&local_partial);
  return 1;
 fail_exprs_annotated:
  SLICE_FREE(exprs_annotated, i, ast_expr_destroy);
  ast_typeexpr_destroy(&local_partial);
  return 0;
}

int check_funcall_funcexpr(struct exprscope *es,
                           struct ast_expr *exprs_annotated,
                           size_t args_count,
                           struct ast_typeexpr *partial_type,
                           struct ast_expr *func,
                           struct ast_expr *annotated_func_out) {
  struct ast_typeexpr funcexpr;
  {
    size_t args_types_count = size_add(args_count, 1);
    struct ast_typeexpr *args_types = malloc_mul(sizeof(*args_types),
                                                 args_types_count);

    for (size_t i = 0; i < args_count; i++) {
      ast_typeexpr_init_copy(&args_types[i], ast_expr_type(&exprs_annotated[i]));
    }
    ast_typeexpr_init_copy(&args_types[args_count], partial_type);

    ident_value func_ident = identmap_intern_c_str(es->cs->im, FUNC_TYPE_NAME);

    funcexpr.tag = AST_TYPEEXPR_APP;
    ast_typeapp_init(&funcexpr.u.app, ast_meta_make_garbage(),
                     make_ast_ident(func_ident),
                     args_types, args_types_count);
  }

  struct ast_expr annotated_func;
  int ret = check_expr(es, func, &funcexpr, &annotated_func);
  ast_typeexpr_destroy(&funcexpr);
  if (ret) {
    *annotated_func_out = annotated_func;
  }
  return ret;
}

int check_expr_funcall(struct exprscope *es,
                       struct ast_funcall *x,
                       struct ast_typeexpr *partial_type,
                       struct ast_expr *annotated_out) {
  size_t args_count = x->args_count;
  struct ast_expr *exprs_annotated;
  if (!check_funcall_args_firstcheck(es, x->args, args_count, &exprs_annotated)) {
    goto fail;
  }
  size_t exprs_annotated_offset = 0;

  struct ast_expr annotated_func;
  if (!check_funcall_funcexpr(es, exprs_annotated, args_count, partial_type,
                              x->func, &annotated_func)) {
    goto fail_cleanup_exprs_annotated;
  }

  struct ast_exprcall *args_annotated = malloc_mul(sizeof(*args_annotated),
                                                   args_count);
  /* Tells fail_cleanup_args_types_and_annotated which exprs in
  exprs_annotated are still there (those in [exprs_annotated_offset,
  args_count)). */
  size_t i;
  for (i = 0; i < args_count; i++) {
    struct ast_expr arg_expr_annotated = exprs_annotated[i];
    exprs_annotated_offset = i + 1;

    struct ast_exprcatch arg_exprcatch;
    if (!compute_and_check_exprcatch(es, &arg_expr_annotated, &arg_exprcatch)) {
      ast_expr_destroy(&arg_expr_annotated);
      goto fail_cleanup_args_annotated;
    }

    ast_exprcall_init_annotated(&args_annotated[i], arg_exprcatch, arg_expr_annotated);
  }
  free(exprs_annotated);
  exprs_annotated = NULL;

  struct ast_typeexpr return_type;
  copy_func_return_type(es->cs->im, ast_expr_type(&annotated_func),
                        size_add(args_count, 1), &return_type);

  struct ast_typeexpr temporary_type;
  ast_typeexpr_init_copy(&temporary_type, &return_type);

  /* TODO: This'll end up being the wrong place to typecheck this
  temporary type. */
  struct typeexpr_traits discard;
  if (!check_typeexpr_traits(es->cs, &temporary_type, es, &discard)) {
    goto fail_cleanup_temporary_type;
  }

  struct ast_expr_info expr_info;
  expr_info = ast_expr_info_typechecked_temporary(
      0,
      return_type,
      temporary_type,
      1,
      exprscope_temptag(es));

  ast_expr_partial_init(annotated_out, AST_EXPR_FUNCALL, expr_info);
  ast_funcall_init(&annotated_out->u.funcall, ast_meta_make_copy(&x->meta),
                   annotated_func, args_annotated, args_count);

  return 1;
  /* Don't fallthrough -- args_annotated was moved into annotated_out. */
 fail_cleanup_temporary_type:
  ast_typeexpr_destroy(&temporary_type);
  ast_typeexpr_destroy(&return_type);
  ast_expr_destroy(&annotated_func);
  SLICE_FREE(args_annotated, args_count, ast_exprcall_destroy);
  return 0;
  /* Don't fallthrough -- args_types was moved into funcexpr. */
 fail_cleanup_args_annotated:
  SLICE_FREE(args_annotated, i, ast_exprcall_destroy);
 fail_cleanup_exprs_annotated:
  for (size_t k = exprs_annotated_offset; k < args_count; k++) {
    ast_expr_destroy(&exprs_annotated[k]);
  }
  free(exprs_annotated);
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
                                   param_list_arity(concrete_type->u.app.params_count),
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
                                   concrete_type->u.app.params,
                                   concrete_type->u.app.params_count,
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
                         struct ast_bracebody *annotated_out,
                         enum fallthrough *fallthrough_out);

int check_statement(struct bodystate *bs,
                    struct ast_statement *s,
                    struct ast_statement *annotated_out,
                    enum fallthrough *fallthrough_out,
                    struct ast_vardecl **vardecl_to_push_or_null_out) {
  enum fallthrough fallthrough;
  struct ast_vardecl *vardecl_to_push = NULL;

  switch (s->tag) {
  case AST_STATEMENT_EXPR: {
    struct ast_typeexpr anything = ast_unknown_garbage();
    struct ast_expr annotated_expr;
    if (!check_expr(bs->es, s->u.expr, &anything, &annotated_expr)) {
      ast_typeexpr_destroy(&anything);
      goto fail;
    }
    ast_typeexpr_destroy(&anything);
    annotated_out->tag = AST_STATEMENT_EXPR;
    ast_expr_alloc_move(annotated_expr, &annotated_out->u.expr);
    fallthrough = FALLTHROUGH_FROMTHETOP;
  } break;
  case AST_STATEMENT_RETURN: {
    int has_expr = s->u.return_statement.has_expr;
    struct ast_expr annotated_expr = { 0 };
    struct ast_typeexpr void_type = { 0 };
    struct ast_typeexpr *expr_type;
    if (has_expr) {
      if (!check_expr(bs->es, s->u.return_statement.expr, bs->partial_type,
                      &annotated_expr)) {
        goto fail;
      }
      expr_type = ast_expr_type(&annotated_expr);
    } else {
      init_name_type(&void_type,
                     identmap_intern_c_str(bs->es->cs->im, VOID_TYPE_NAME));
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
      if (!exact_typeexprs_equal(&bs->exact_return_type, expr_type)) {
        METERR(s->u.return_statement.meta,
               "Return statements with conflicting return types.%s", "\n");
        if (has_expr) {
          ast_expr_destroy(&annotated_expr);
        } else {
          ast_typeexpr_destroy(&void_type);
        }
        goto fail;
      }
      if (!has_expr) {
        ast_typeexpr_destroy(&void_type);
      }
    }
    annotated_out->tag = AST_STATEMENT_RETURN;
    if (has_expr) {
      ast_return_statement_init(&annotated_out->u.return_statement,
                                ast_meta_make_copy(&s->u.return_statement.meta),
                                annotated_expr);
    } else {
      ast_return_statement_init_no_expr(
          &annotated_out->u.return_statement,
          ast_meta_make_copy(&s->u.return_statement.meta));

    }
    fallthrough = FALLTHROUGH_NEVER;
  } break;
  case AST_STATEMENT_VAR: {
    struct ast_typeexpr replaced_incomplete_type;
    replace_generics(bs->es, &s->u.var_statement.decl_.type, &replaced_incomplete_type);

    struct ast_typeexpr concrete_type;

    int has_rhs = s->u.var_statement.has_rhs;
    struct ast_expr annotated_rhs = { 0 };  /* Initialize to appease cl. */
    if (has_rhs) {
      if (!check_expr(bs->es, s->u.var_statement.rhs, &replaced_incomplete_type, &annotated_rhs)) {
        ast_typeexpr_destroy(&replaced_incomplete_type);
        goto fail;
      }
      ast_typeexpr_destroy(&replaced_incomplete_type);
      ast_typeexpr_init_copy(&concrete_type, ast_expr_type(&annotated_rhs));
    } else {
      if (!is_concrete(&replaced_incomplete_type)) {
        METERR(s->u.var_statement.decl_.meta, "Var statement without initializer has incomplete type.%s", "\n");
        goto fail;
      }

      concrete_type = replaced_incomplete_type;
    }

    struct typeexpr_traits traits;
    if (!check_typeexpr_traits(bs->es->cs, &concrete_type, bs->es, &traits)) {
      ast_typeexpr_destroy(&concrete_type);
      goto fail;
    }

    if (!has_rhs && traits.inittible == TYPEEXPR_TRAIT_LACKED) {
        METERR(s->u.var_statement.meta, "Variable %.*s not default-initializable.\n",
               IM_P(bs->es->cs->im, s->u.var_statement.decl_.name.value));
        ast_typeexpr_destroy(&concrete_type);
        goto fail;
    }

    struct ast_ident name;
    ast_ident_init_copy(&name, &s->u.var_statement.decl_.name);

    struct ast_typeexpr concrete_type_copy;
    ast_typeexpr_init_copy(&concrete_type_copy, &concrete_type);

    struct ast_vardecl *replaced_decl = malloc(sizeof(*replaced_decl));
    CHECK(replaced_decl);
    ast_vardecl_init(replaced_decl, ast_meta_make_copy(&s->u.var_statement.decl_.meta),
                     name, concrete_type_copy);

    struct varnum varnum;
    if (!exprscope_push_var(bs->es, replaced_decl, &varnum)) {
      free_ast_vardecl(&replaced_decl);
      ast_typeexpr_destroy(&concrete_type);
      if (has_rhs) {
        ast_expr_destroy(&annotated_rhs);
      }
      goto fail;
    }

    vardecl_to_push = replaced_decl;

    struct ast_vardecl decl;
    ast_vardecl_init_copy(&decl, &s->u.var_statement.decl_);
    ast_var_info_specify(&decl.var_info, varnum, concrete_type);
    annotated_out->tag = AST_STATEMENT_VAR;
    if (has_rhs) {
      ast_var_statement_init_with_rhs(&annotated_out->u.var_statement,
                                      ast_meta_make_copy(&s->u.var_statement.meta),
                                      decl, annotated_rhs);
    } else {
      ast_var_statement_init_without_rhs(&annotated_out->u.var_statement,
                                         ast_meta_make_copy(&s->u.var_statement.meta),
                                         decl);
    }
    fallthrough = FALLTHROUGH_FROMTHETOP;
  } break;
  case AST_STATEMENT_IFTHEN: {
    struct ast_typeexpr boolean;
    init_boolean_typeexpr(bs->es->cs, &boolean);

    struct ast_expr annotated_condition;
    if (!check_expr(bs->es, s->u.ifthen_statement.condition, &boolean, &annotated_condition)) {
      ast_typeexpr_destroy(&boolean);
      goto fail;
    }
    ast_typeexpr_destroy(&boolean);

    struct ast_bracebody annotated_body;
    enum fallthrough body_fallthrough;
    if (!check_expr_bracebody(bs, &s->u.ifthen_statement.body,
                              &annotated_body, &body_fallthrough)) {
      ast_expr_destroy(&annotated_condition);
      goto fail;
    }

    annotated_out->tag = AST_STATEMENT_IFTHEN;
    ast_ifthen_statement_init(
        &annotated_out->u.ifthen_statement,
        ast_meta_make_copy(&s->u.ifthen_statement.meta),
        annotated_condition,
        annotated_body);
    fallthrough = max_fallthrough(FALLTHROUGH_FROMTHETOP, body_fallthrough);
  } break;
  case AST_STATEMENT_IFTHENELSE: {
    struct ast_typeexpr boolean;
    init_boolean_typeexpr(bs->es->cs, &boolean);

    struct ast_expr annotated_condition;
    if (!check_expr(bs->es, s->u.ifthenelse_statement.condition, &boolean,
                    &annotated_condition)) {
      ast_typeexpr_destroy(&boolean);
      goto fail;
    }
    ast_typeexpr_destroy(&boolean);

    struct ast_bracebody annotated_thenbody;
    enum fallthrough thenbody_fallthrough;
    if (!check_expr_bracebody(bs, &s->u.ifthenelse_statement.thenbody,
                              &annotated_thenbody, &thenbody_fallthrough)) {
      ast_expr_destroy(&annotated_condition);
      goto fail;
    }

    struct ast_bracebody annotated_elsebody;
    enum fallthrough elsebody_fallthrough;
    if (!check_expr_bracebody(bs, &s->u.ifthenelse_statement.elsebody,
                              &annotated_elsebody, &elsebody_fallthrough)) {
      ast_bracebody_destroy(&annotated_thenbody);
      ast_expr_destroy(&annotated_condition);
      goto fail;
    }

    annotated_out->tag = AST_STATEMENT_IFTHENELSE;
    ast_ifthenelse_statement_init(
        &annotated_out->u.ifthenelse_statement,
        ast_meta_make_copy(&s->u.ifthenelse_statement.meta),
        annotated_condition,
        annotated_thenbody,
        annotated_elsebody);

    fallthrough = max_fallthrough(thenbody_fallthrough, elsebody_fallthrough);
  } break;
  case AST_STATEMENT_WHILE: {
    struct ast_typeexpr boolean;
    init_boolean_typeexpr(bs->es->cs, &boolean);

    struct ast_expr annotated_condition;
    if (!check_expr(bs->es, s->u.while_statement.condition, &boolean,
                    &annotated_condition)) {
      ast_typeexpr_destroy(&boolean);
      goto fail;
    }
    ast_typeexpr_destroy(&boolean);

    struct ast_bracebody annotated_body;
    enum fallthrough body_fallthrough;
    if (!check_expr_bracebody(bs, &s->u.while_statement.body, &annotated_body,
                              &body_fallthrough)) {
      ast_expr_destroy(&annotated_condition);
      goto fail;
    }

    annotated_out->tag = AST_STATEMENT_WHILE;
    ast_while_statement_init(
        &annotated_out->u.while_statement,
        ast_meta_make_copy(&s->u.while_statement.meta),
        annotated_condition,
        annotated_body);

    fallthrough = max_fallthrough(FALLTHROUGH_FROMTHETOP, body_fallthrough);
  } break;
  case AST_STATEMENT_FOR: {
    struct ast_for_statement *fs = &s->u.for_statement;

    struct ast_statement annotated_initializer = { 0 };
    struct ast_vardecl *vardecl_to_push_or_null = NULL;
    int var_pushed = 0;
    if (fs->has_initializer) {
      CHECK(fs->initializer->tag == AST_STATEMENT_EXPR
            || fs->initializer->tag == AST_STATEMENT_VAR);
      enum fallthrough initializer_fallthrough;
      if (!check_statement(bs, fs->initializer, &annotated_initializer,
                           &initializer_fallthrough, &vardecl_to_push_or_null)) {
        goto for_fail;
      }

      CHECK(initializer_fallthrough == FALLTHROUGH_FROMTHETOP);
      if (vardecl_to_push_or_null) {
        var_pushed = 1;
      }
    }

    struct ast_expr annotated_condition = { 0 };
    if (fs->has_condition) {
      struct ast_typeexpr boolean;
      init_boolean_typeexpr(bs->es->cs, &boolean);

      if (!check_expr(bs->es, fs->condition, &boolean, &annotated_condition)) {
        ast_typeexpr_destroy(&boolean);
        goto for_fail_annotated_initializer;
      }
      ast_typeexpr_destroy(&boolean);
    }

    struct ast_expr annotated_increment = { 0 };
    if (fs->has_increment) {
      struct ast_typeexpr anything = ast_unknown_garbage();
      if (!check_expr(bs->es, fs->increment, &anything, &annotated_increment)) {
        ast_typeexpr_destroy(&anything);
        goto for_fail_annotated_condition;
      }
      ast_typeexpr_destroy(&anything);
    }

    enum fallthrough body_fallthrough;
    struct ast_bracebody annotated_body;
    if (!check_expr_bracebody(bs, &fs->body, &annotated_body, &body_fallthrough)) {
      goto for_fail_annotated_increment;
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

    struct ast_statement *ini = NULL;
    struct ast_expr *con = NULL;
    struct ast_expr *inc = NULL;
    if (fs->has_initializer) {
      ast_statement_alloc_move(annotated_initializer, &ini);
    }
    if (fs->has_condition) {
      ast_expr_alloc_move(annotated_condition, &con);
    }
    if (fs->has_increment) {
      ast_expr_alloc_move(annotated_increment, &inc);
    }
    annotated_out->tag = AST_STATEMENT_FOR;
    ast_for_statement_init(&annotated_out->u.for_statement,
                           ast_meta_make_copy(&fs->meta),
                           fs->has_initializer, ini,
                           fs->has_condition, con,
                           fs->has_increment, inc,
                           annotated_body);

    break;
  for_fail_annotated_increment:
    if (fs->has_increment) {
      ast_expr_destroy(&annotated_increment);
    }
  for_fail_annotated_condition:
    if (fs->has_condition) {
      ast_expr_destroy(&annotated_condition);
    }
  for_fail_annotated_initializer:
    if (fs->has_initializer) {
      ast_statement_destroy(&annotated_initializer);
    }
  for_fail:
    goto fail;
  } break;
  case AST_STATEMENT_SWITCH: {
    struct ast_switch_statement *ss = &s->u.switch_statement;
    struct ast_typeexpr partial = ast_unknown_garbage();;
    struct ast_expr annotated_swartch;
    if (!check_expr(bs->es, ss->swartch, &partial, &annotated_swartch)) {
      ast_typeexpr_destroy(&partial);
      goto fail;
    }
    ast_typeexpr_destroy(&partial);

    struct ast_enumspec concrete_enumspec;
    if (!is_enum_type(bs->es->cs, ast_expr_type(&annotated_swartch), &concrete_enumspec)) {
      METERR(*ast_expr_ast_meta(ss->swartch),
             "Switching over non-enum type.%s", "\n");
      goto switch_fail_annotated_swartch;
    }

    fallthrough = FALLTHROUGH_NEVER;

    struct ast_cased_statement *annotated_cased_statements
      = malloc_mul(sizeof(*annotated_cased_statements), ss->cased_statements_count);

    size_t i = 0;
    for (size_t e = ss->cased_statements_count; i < e; i++) {
      struct ast_cased_statement *cas = &ss->cased_statements[i];
      for (size_t j = 0; j < i; j++) {
        if (ss->cased_statements[j].pattern.constructor_name.value
            == cas->pattern.constructor_name.value) {
          METERR(cas->meta, "Overlapping (duplicate) switch cases.%s", "\n");
          goto switch_fail_annotated_cased_statements;
        }
      }

      int constructor_found = 0;
      size_t constructor_num = SIZE_MAX;  /* Initialized to appease cl. */
      for (size_t j = 0, je = concrete_enumspec.enumfields_count; j < je; j++) {
        if (concrete_enumspec.enumfields[j].name.value
            == cas->pattern.constructor_name.value) {
          constructor_found = 1;
          constructor_num = j;
          break;
        }
      }

      if (!constructor_found) {
        METERR(cas->meta, "Unrecognized constructor in switch case.%s", "\n");
        goto switch_fail_annotated_cased_statements;
      }

      struct typeexpr_traits discard;
      if (!check_typeexpr_traits(bs->es->cs, &concrete_enumspec.enumfields[constructor_num].type, bs->es, &discard)) {
        goto switch_fail_annotated_cased_statements;
      }

      struct ast_typeexpr replaced_incomplete_type;
      replace_generics(bs->es, &cas->pattern.decl_.type, &replaced_incomplete_type);

      struct varnum varnum;
      struct ast_vardecl *replaced_decl;
      {
        if (!unify_directionally(&replaced_incomplete_type,
                                 &concrete_enumspec.enumfields[constructor_num].type)) {
          ast_typeexpr_destroy(&replaced_incomplete_type);
          METERR(cas->meta, "Switch case decl type mismatch.%s", "\n");
          goto switch_fail_annotated_cased_statements;
        }
        ast_typeexpr_destroy(&replaced_incomplete_type);

        struct ast_ident replaced_decl_name;
        ast_ident_init_copy(&replaced_decl_name, &cas->pattern.decl_.name);

        struct ast_typeexpr concrete_type_copy;
        ast_typeexpr_init_copy(&concrete_type_copy, &concrete_enumspec.enumfields[constructor_num].type);

        replaced_decl = malloc(sizeof(*replaced_decl));
        CHECK(replaced_decl);
        ast_vardecl_init(replaced_decl, ast_meta_make_copy(&cas->pattern.decl_.meta),
                         replaced_decl_name, concrete_type_copy);

        if (!exprscope_push_var(bs->es, replaced_decl, &varnum)) {
          free_ast_vardecl(&replaced_decl);
          goto switch_fail_annotated_cased_statements;
        }
      }

      enum fallthrough cas_fallthrough;
      struct ast_bracebody cas_annotated_body;
      if (!check_expr_bracebody(bs, &cas->body, &cas_annotated_body,
                                &cas_fallthrough)) {
        free_ast_vardecl(&replaced_decl);
        goto switch_fail_annotated_cased_statements;
      }

      fallthrough = max_fallthrough(fallthrough, cas_fallthrough);

      exprscope_pop_var(bs->es);
      free_ast_vardecl(&replaced_decl);

      struct ast_case_pattern pattern_copy;
      {
        struct ast_ident constructor_name;
        ast_ident_init_copy(&constructor_name, &cas->pattern.constructor_name);
        struct ast_vardecl decl;
        ast_vardecl_init_copy(&decl, &cas->pattern.decl_);
        {
          struct ast_typeexpr concrete_type_copy;
          ast_typeexpr_init_copy(&concrete_type_copy, &concrete_enumspec.enumfields[constructor_num].type);
          ast_var_info_specify(&decl.var_info, varnum, concrete_type_copy);
        }
        ast_case_pattern_init(&pattern_copy,
                              ast_meta_make_copy(&cas->pattern.meta),
                              constructor_name,
                              decl);
        ast_case_pattern_info_specify(&pattern_copy.info, constructor_num);
      }

      ast_cased_statement_init(&annotated_cased_statements[i],
                               ast_meta_make_copy(&cas->meta),
                               pattern_copy,
                               cas_annotated_body);
    }

    annotated_out->tag = AST_STATEMENT_SWITCH;
    ast_switch_statement_init(&annotated_out->u.switch_statement,
                              ast_meta_make_copy(&ss->meta),
                              annotated_swartch,
                              annotated_cased_statements,
                              ss->cased_statements_count);
    break;
  switch_fail_annotated_cased_statements:
    SLICE_FREE(annotated_cased_statements, i, ast_cased_statement_destroy);
    ast_enumspec_destroy(&concrete_enumspec);
  switch_fail_annotated_swartch:
    ast_expr_destroy(&annotated_swartch);
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
                         struct ast_bracebody *annotated_out,
                         enum fallthrough *fallthrough_out) {
  struct ast_vardecl **vardecls_pushed = NULL;
  size_t vardecls_pushed_count = 0;
  size_t vardecls_pushed_limit = 0;
  int ret = 0;

  enum fallthrough reachable = FALLTHROUGH_FROMTHETOP;

  struct ast_statement *annotated_statements
    = malloc_mul(sizeof(*annotated_statements), x->statements_count);

  size_t i = 0;
  for (size_t e = x->statements_count; i < e; i++) {
    struct ast_statement *s = &x->statements[i];
    enum fallthrough fallthrough;
    struct ast_vardecl *vardecl_to_push_or_null;
    if (!check_statement(bs, s, &annotated_statements[i], &fallthrough,
                         &vardecl_to_push_or_null)) {
      goto fail;
    }

    reachable = compose_fallthrough(reachable, fallthrough);
    if (vardecl_to_push_or_null) {
      SLICE_PUSH(vardecls_pushed, vardecls_pushed_count, vardecls_pushed_limit,
                 vardecl_to_push_or_null);
    }
  }

  ast_bracebody_init(annotated_out, ast_meta_make_copy(&x->meta),
                     annotated_statements, x->statements_count);
  *fallthrough_out = reachable;
  ret = 1;
 fail:
  if (!ret) {
    SLICE_FREE(annotated_statements, i, ast_statement_destroy);
  }
  for (size_t j = 0; j < vardecls_pushed_count; j++) {
    exprscope_pop_var(bs->es);
  }
  SLICE_FREE(vardecls_pushed, vardecls_pushed_count, free_ast_vardecl);
  return ret;
}

int check_expr_funcbody(struct exprscope *es,
                        struct ast_bracebody *x,
                        struct ast_typeexpr *partial_type,
                        struct ast_typeexpr *out,
                        struct ast_bracebody *annotated_out) {
  int ret = 0;

  struct bodystate bs;
  bodystate_init(&bs, es, partial_type);

  struct ast_bracebody annotated_bracebody;
  enum fallthrough fallthrough;
  if (!check_expr_bracebody(&bs, x, &annotated_bracebody, &fallthrough)) {
    goto fail;
  }

  ident_value void_ident = identmap_intern_c_str(es->cs->im, VOID_TYPE_NAME);
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
      METERR(x->meta, "not all control paths return a value.%s", "\n");
      goto fail_annotated_bracebody;
    }
  }

  /* We should have an exact return type because we didn't fallthrough
  (or the type was void and have_exact_return_type has been set). */
  CHECK(bs.have_exact_return_type);

  struct typeexpr_traits discard;
  if (!check_typeexpr_traits(es->cs, &bs.exact_return_type, es, &discard)) {
    goto fail_annotated_bracebody;
  }

  *out = bs.exact_return_type;
  bs.have_exact_return_type = 0;
  *annotated_out = annotated_bracebody;

  ret = 1;
 fail_annotated_bracebody:
  if (!ret) {
    ast_bracebody_destroy(&annotated_bracebody);
  }
 fail:
  bodystate_destroy(&bs);
  return ret;
}

int check_expr_lambda(struct exprscope *es,
                      struct ast_lambda *x,
                      struct ast_typeexpr *partial_type,
                      struct ast_typeexpr *out,
                      struct ast_lambda *annotated_out) {
  CHECK_DBG("check_expr_lambda\n");
  ident_value func_ident = identmap_intern_c_str(es->cs->im, FUNC_TYPE_NAME);
  size_t func_params_count = x->params_count;
  size_t args_count = size_add(func_params_count, 1);

  struct ast_typeexpr funcexpr;
  {
    struct ast_typeexpr *args = malloc_mul(sizeof(*args), args_count);
    size_t i;
    for (i = 0; i < func_params_count; i++) {
      for (size_t j = 0; j < i; j++) {
        if (x->params[i].name.value == x->params[j].name.value) {
          METERR(x->params[i].meta, "Duplicate lambda parameter name %.*s.\n",
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
                     make_ast_ident(func_ident), args, args_count);
  }

  if (!unify_directionally(partial_type, &funcexpr)) {
    METERR(x->meta, "lambda type does not match expression type.%s", "\n");
    goto fail_funcexpr;
  }

  /* Because lambdas can't capture variables, we make a fresh exprscope. */

  struct ast_vardecl *replaced_vardecls
    = malloc_mul(sizeof(*replaced_vardecls), func_params_count);
  size_t replaced_vardecls_size = func_params_count;

  for (size_t i = 0; i < func_params_count; i++) {
    struct ast_typeexpr type;
    ast_typeexpr_init_copy(&type, &funcexpr.u.app.params[i]);
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

  struct varnum *varnums = malloc_mul(sizeof(*varnums), func_params_count);

  for (size_t i = 0; i < func_params_count; i++) {
    int res = exprscope_push_var(&bb_es, &replaced_vardecls[i], &varnums[i]);
    /* Pushing the var should succeed, because we already called
    check_var_shadowing above. */
    CHECK(res);
  }

  struct ast_typeexpr computed_return_type;
  struct ast_bracebody annotated_bracebody;
  if (!check_expr_funcbody(
          &bb_es,
          &x->bracebody,
          &funcexpr.u.app.params[size_sub(funcexpr.u.app.params_count, 1)],
          &computed_return_type,
          &annotated_bracebody)) {
    CHECK_DBG("check_expr_funcbody fails\n");
    goto fail_bb_es;
  }

  ast_typeexpr_destroy(&computed_return_type);
  exprscope_destroy(&bb_es);
  SLICE_FREE(replaced_vardecls, replaced_vardecls_size, ast_vardecl_destroy);
  {
    struct ast_vardecl *params = malloc_mul(sizeof(*params), x->params_count);
    for (size_t i = 0, e = x->params_count; i < e; i++) {
      ast_vardecl_init_copy(&params[i], &x->params[i]);
      struct ast_typeexpr concrete_param_type;
      ast_typeexpr_init_copy(&concrete_param_type, &funcexpr.u.app.params[i]);
      ast_var_info_specify(&params[i].var_info, varnums[i], concrete_param_type);
    }
    free(varnums);
    struct ast_typeexpr return_type;
    ast_typeexpr_init_copy(&return_type, &x->return_type);

    ast_lambda_init(annotated_out, ast_meta_make_copy(&x->meta),
                    params, x->params_count,
                    return_type, annotated_bracebody);
  }
  *out = funcexpr;
  CHECK_DBG("check_expr_lambda succeeds\n");
  return 1;

 fail_bb_es:
  free(varnums);
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
                           int *is_lvalue_out,
                           struct ast_binop_expr *annotated_out) {
  int ret = 0;
  struct ast_typeexpr no_partial = ast_unknown_garbage();

  struct ast_expr annotated_lhs;
  if (!check_expr(es, x->lhs, &no_partial, &annotated_lhs)) {
    goto cleanup;
  }

  switch (x->operator) {
  case AST_BINOP_ASSIGN: {
    struct ast_expr annotated_rhs;
    if (!check_expr(es, x->rhs, ast_expr_type(&annotated_lhs), &annotated_rhs)) {
      goto cleanup_lhs;
    }
    if (es->computation == STATIC_COMPUTATION_YES) {
      METERR(x->meta, "Assignment within statically evaluated expression.%s", "\n");
      goto assign_cleanup_rhs;
    }
    if (!annotated_lhs.info.is_lvalue) {
      METERR(x->meta, "Trying to assign to non-lvalue.%s", "\n");
      goto assign_cleanup_rhs;
    }
    if (!exact_typeexprs_equal(ast_expr_type(&annotated_lhs),
                               ast_expr_type(&annotated_rhs))) {
      METERR(x->meta, "Assignment with non-matching types.%s", "\n");
      goto assign_cleanup_rhs;
    }

    if (!unify_directionally(partial_type,
                             ast_expr_type(&annotated_lhs))) {
      METERR(x->meta, "LHS type of assignment does not match contextual type.%s", "\n");
      goto assign_cleanup_rhs;
    }

    ast_typeexpr_init_copy(out, ast_expr_type(&annotated_lhs));
    *is_lvalue_out = 1;

    ast_binop_expr_init(annotated_out, ast_meta_make_copy(&x->meta),
                        x->operator, annotated_lhs, annotated_rhs);

    ret = 1;
    goto cleanup;
  assign_cleanup_rhs:
    ast_expr_destroy(&annotated_rhs);
    goto cleanup_lhs;
  } break;
  case AST_BINOP_LOGICAL_OR:
  case AST_BINOP_LOGICAL_AND: {
    struct ast_expr annotated_rhs;
    if (!check_expr(es, x->rhs, &no_partial, &annotated_rhs)) {
      goto cleanup_lhs;
    }
    struct ast_typeexpr boolean;
    init_name_type(&boolean, identmap_intern_c_str(es->cs->im, BOOL_TYPE_NAME));

    if (!unify_directionally(&boolean,
                             ast_expr_type(&annotated_lhs))) {
      METERR(x->meta, "LHS of and/or is non-boolean.%s", "\n");
      goto logical_cleanup_boolean;
    }

    if (!unify_directionally(&boolean,
                             ast_expr_type(&annotated_rhs))) {
      METERR(x->meta, "RHS of and/or is non-boolean.%s", "\n");
      goto logical_cleanup_boolean;
    }

    if (!unify_directionally(partial_type, &boolean)) {
      METERR(x->meta, "And/or expression in non-boolean context.%s", "\n");
      goto logical_cleanup_boolean;
    }

    *out = boolean;
    *is_lvalue_out = 0;

    ast_binop_expr_init(annotated_out, ast_meta_make_copy(&x->meta),
                        x->operator, annotated_lhs, annotated_rhs);

    ret = 1;
    goto cleanup;
  logical_cleanup_boolean:
    ast_typeexpr_destroy(&boolean);
    ast_expr_destroy(&annotated_rhs);
    goto cleanup_lhs;
  } break;
  default:
    UNREACHABLE();
  }

 cleanup_lhs:
  CHECK(!ret);
  ast_expr_destroy(&annotated_lhs);
 cleanup:
  ast_typeexpr_destroy(&no_partial);
  return ret;
}

int check_expr_binop(struct exprscope *es,
                     struct ast_binop_expr *x,
                     struct ast_typeexpr *partial_type,
                     struct ast_typeexpr *out,
                     int *is_lvalue_out,
                     struct ast_binop_expr *annotated_out) {
  CHECK(is_magic_binop(x->operator));
  return check_expr_magic_binop(es, x, partial_type, out, is_lvalue_out,
                                annotated_out);
}

int view_ptr_target(struct identmap *im,
                    struct ast_typeexpr *ptr_type,
                    struct ast_typeexpr **target_out) {
  if (ptr_type->tag != AST_TYPEEXPR_APP) {
    return 0;
  }

  ident_value ptr_ident = identmap_intern_c_str(im, PTR_TYPE_NAME);
  if (ptr_type->u.app.name.value != ptr_ident) {
    return 0;
  }

  if (ptr_type->u.app.params_count != 1) {
    return 0;
  }

  *target_out = &ptr_type->u.app.params[0];
  return 1;
}

void wrap_in_ptr(struct identmap *im,
                 struct ast_typeexpr *target,
                 struct ast_typeexpr *ptr_out) {
  ptr_out->tag = AST_TYPEEXPR_APP;
  struct ast_typeexpr *params = malloc_mul(sizeof(*params), 1);
  ast_typeexpr_init_copy(&params[0], target);
  ast_typeapp_init(&ptr_out->u.app, ast_meta_make_garbage(),
                   make_ast_ident(identmap_intern_c_str(im, PTR_TYPE_NAME)),
                   params, 1);
}

int check_expr_magic_unop(struct exprscope *es,
                          struct ast_unop_expr *x,
                          struct ast_typeexpr *partial_type,
                          struct ast_expr *annotated_out) {
  if (es->computation == STATIC_COMPUTATION_YES) {
    METERR(x->meta, "Magic unops not allowed in static expressions.%s", "\n");
    return 0;
  }

  int ret = 0;
  struct ast_typeexpr no_partial = ast_unknown_garbage();

  struct ast_expr annotated_rhs;
  if (!check_expr(es, x->rhs, &no_partial, &annotated_rhs)) {
    goto cleanup;
  }

  switch (x->operator) {
  case AST_UNOP_DEREFERENCE: {
    struct ast_typeexpr *rhs_target;
    if (!view_ptr_target(es->cs->im,
                         ast_expr_type(&annotated_rhs),
                         &rhs_target)) {
      METERR(x->meta, "Trying to dereference a non-pointer.%s", "\n");
      goto cleanup_rhs;
    }

    if (!unify_directionally(partial_type, rhs_target)) {
      METERR(x->meta, "Pointer dereference results in wrong type.%s", "\n");
      goto cleanup_rhs;
    }

    struct ast_typeexpr return_type;
    ast_typeexpr_init_copy(&return_type, rhs_target);
    ast_expr_partial_init(annotated_out, AST_EXPR_UNOP,
                          ast_expr_info_typechecked_no_temporary(1, return_type));
    ast_unop_expr_init(&annotated_out->u.unop_expr, ast_meta_make_copy(&x->meta),
                       x->operator, annotated_rhs);
    ret = 1;
  } break;
  case AST_UNOP_ADDRESSOF: {
    if (!annotated_rhs.info.is_lvalue) {
      METERR(x->meta, "Trying to take the address of a non-lvalue.%s", "\n");
      goto cleanup_rhs;
    }

    struct ast_typeexpr pointer_type;
    wrap_in_ptr(es->cs->im, ast_expr_type(&annotated_rhs),
                &pointer_type);

    if (!unify_directionally(partial_type, &pointer_type)) {
      METERR(x->meta, "Addressof results in wrong type.%s", "\n");
      ast_typeexpr_destroy(&pointer_type);
      goto cleanup_rhs;
    }

    /* addressof returns a pointer, which is a temporary though it is also trivial. */
    ast_expr_partial_init(annotated_out, AST_EXPR_UNOP,
                          ast_expr_info_typechecked_trivial_temporary(0, pointer_type));
    ast_unop_expr_init(&annotated_out->u.unop_expr, ast_meta_make_copy(&x->meta),
                       x->operator, annotated_rhs);
    ret = 1;
  } break;
  case AST_UNOP_NEGATE:
  case AST_UNOP_CONVERT:
  case AST_UNOP_LOGICAL_NOT:
  case AST_UNOP_BITWISE_NOT:
  default:
    UNREACHABLE();
  }

 cleanup_rhs:
  if (!ret) {
    ast_expr_destroy(&annotated_rhs);
  }
 cleanup:
  ast_typeexpr_destroy(&no_partial);
  return ret;
}

int check_expr_unop(struct exprscope *es,
                    struct ast_unop_expr *x,
                    struct ast_typeexpr *partial_type,
                    struct ast_expr *annotated_out) {
  CHECK(is_magic_unop(x->operator));
  return check_expr_magic_unop(es, x, partial_type, annotated_out);
}

int lookup_fields_field_type(struct ast_vardecl *fields,
                             size_t fields_count,
                             struct ast_ident *field_name,
                             struct ast_typeexpr *field_type_out) {
  for (size_t i = 0; i < fields_count; i++) {
    if (fields[i].name.value == field_name->value) {
      ast_typeexpr_init_copy(field_type_out, &fields[i].type);
      return 1;
    }
  }

  METERR(field_name->meta, "Field name not found.%s", "\n");
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
      CRASH("lookup_field_type sees an invalid type.");
    }

    if (ent->is_primitive) {
      METERR(fieldname->meta, "Looking up %sfield on primitive type %.*s.",
             fieldname->whole_field ? "whole " : "",
             IM_P(es->cs->im, type->u.name.value));
      return 0;
    }

    struct ast_deftype *deftype = ent->deftype;
    CHECK(!deftype->generics.has_type_params);
    if (!deftype_is_accessible(es, deftype)) {
      METERR(fieldname->meta, "Looking up field on inaccessible type.%s", "\n");
      return 0;
    }

    switch (deftype->rhs.tag) {
    case AST_DEFTYPE_RHS_TYPE: {
      if (fieldname->whole_field) {
        /* Stop here -- we don't recursively follow deftype chains on
        whole-field access.  Maybe we shouldn't, for named fields,
        too, but we do. */
        ast_typeexpr_init_copy(field_type_out, &deftype->rhs.u.type);
        return 1;
      } else {
        return lookup_field_type(es, &deftype->rhs.u.type, fieldname, field_type_out);
      }
    } break;
    case AST_DEFTYPE_RHS_ENUMSPEC:
      METERR(fieldname->meta, "Looking up %sfield on enumspec type.\n",
             fieldname->whole_field ? "whole " : "");
      return 0;
    default:
      UNREACHABLE();
    }
  } break;
  case AST_TYPEEXPR_APP: {
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(&es->cs->nt, type->u.app.name.value,
                                   param_list_arity(type->u.app.params_count),
                                   &ent)) {
      CRASH("lookup_field_type sees an invalid generic type.");
    }
    if (ent->is_primitive) {
      METERR(fieldname->meta, "Looking up %sfield on primitive type.\n",
             fieldname->whole_field ? "whole " : "");
      return 0;
    }

    struct ast_deftype *deftype = ent->deftype;
    CHECK(deftype->generics.has_type_params
          && deftype->generics.params_count == type->u.app.params_count);
    if (!deftype_is_accessible(es, deftype)) {
      METERR(fieldname->meta, "Looking up field on inaccessible type.%s", "\n");
      return 0;
    }

    switch (deftype->rhs.tag) {
    case AST_DEFTYPE_RHS_TYPE: {
      struct ast_typeexpr concrete_deftype_type;
      do_replace_generics(&deftype->generics,
                          type->u.app.params,
                          type->u.app.params_count,
                          &deftype->rhs.u.type,
                          &concrete_deftype_type);

      if (fieldname->whole_field) {
        /* Stop here -- we don't recursively follow deftype chains on
        whole-field access.  Maybe we shouldn't, for named fields,
        too, but we do. */
        *field_type_out = concrete_deftype_type;
        return 1;
      } else {
        int ret = lookup_field_type(es, &concrete_deftype_type, fieldname,
                                    field_type_out);
        ast_typeexpr_destroy(&concrete_deftype_type);
        return ret;
      }
    } break;
    case AST_DEFTYPE_RHS_ENUMSPEC:
      METERR(fieldname->meta, "Looking up %sfield on enumspec type.\n",
             fieldname->whole_field ? "whole " : "");
      return 0;
    default:
      UNREACHABLE();
    }
  } break;
  case AST_TYPEEXPR_STRUCTE: {
    if (fieldname->whole_field) {
      METERR(fieldname->meta, "Looking up whole field of a naked struct type.%s", "\n");
      return 0;
    }
    return lookup_fields_field_type(type->u.structe.fields,
                                    type->u.structe.fields_count,
                                    &fieldname->ident,
                                    field_type_out);
  } break;
  case AST_TYPEEXPR_UNIONE: {
    if (fieldname->whole_field) {
      METERR(fieldname->meta, "Looking up whole field of a struct type.%s", "\n");
      return 0;
    }
    return lookup_fields_field_type(type->u.unione.fields,
                                    type->u.unione.fields_count,
                                    &fieldname->ident,
                                    field_type_out);
  }
  case AST_TYPEEXPR_ARRAY: {
    METERR(fieldname->meta, "Looking up %sfield on array type.\n",
           fieldname->whole_field ? "whole " : "");
    return 0;
  } break;
  default:
    UNREACHABLE();
  }
}

int check_expr_local_field_access(
    struct exprscope *es,
    struct ast_local_field_access *x,
    struct ast_typeexpr *partial_type,
    struct ast_expr *annotated_out) {
  int ret = 0;
  struct ast_typeexpr lhs_partial_type = ast_unknown_garbage();

  struct ast_expr annotated_lhs;
  if (!check_expr(es, x->lhs, &lhs_partial_type, &annotated_lhs)) {
    goto cleanup;
  }

  if (ast_expr_type(&annotated_lhs)->tag == AST_TYPEEXPR_ARRAY
      && !x->fieldname.whole_field
      && x->fieldname.ident.value == identmap_intern_c_str(es->cs->im, ARRAY_LENGTH_FIELDNAME)) {
    struct ast_fieldname fieldname;
    ast_fieldname_init_copy(&fieldname, &x->fieldname);
    ast_local_field_access_init(&annotated_out->u.local_field_access,
                                ast_meta_make_copy(&x->meta),
                                annotated_lhs, fieldname);
    struct ast_typeexpr u32_type;
    init_name_type(&u32_type, identmap_intern_c_str(es->cs->im, U32_TYPE_NAME));
    ast_expr_partial_init(annotated_out,
                          AST_EXPR_LOCAL_FIELD_ACCESS,
                          ast_expr_info_typechecked_trivial_temporary(
                              0, u32_type));
    ret = 1;
    goto cleanup;
  }


  struct ast_typeexpr field_type;
  if (!lookup_field_type(es,
                         ast_expr_type(&annotated_lhs),
                         &x->fieldname,
                         &field_type)) {
    goto cleanup_lhs;
  }

  if (!unify_directionally(partial_type, &field_type)) {
    goto cleanup_field_type;
  }

  struct ast_fieldname fieldname;
  ast_fieldname_init_copy(&fieldname, &x->fieldname);
  ast_local_field_access_init(&annotated_out->u.local_field_access,
                              ast_meta_make_copy(&x->meta),
                              annotated_lhs, fieldname);
  ast_expr_partial_init(annotated_out,
                        AST_EXPR_LOCAL_FIELD_ACCESS,
                        expr_info_typechecked_subobject(
                            field_type,
                            &annotated_out->u.local_field_access.lhs->info));
  ret = 1;
  goto cleanup;

 cleanup_field_type:
  ast_typeexpr_destroy(&field_type);
 cleanup_lhs:
  ast_expr_destroy(&annotated_lhs);
 cleanup:
  ast_typeexpr_destroy(&lhs_partial_type);
  return ret;
}

int check_expr_deref_field_access(
    struct exprscope *es,
    struct ast_deref_field_access *x,
    struct ast_typeexpr *partial_type,
    struct ast_typeexpr *out,
    struct ast_deref_field_access *annotated_out) {
  if (es->computation == STATIC_COMPUTATION_YES) {
    METERR(x->meta, "Dereferencing field access disallowed in static computation.%s", "\n");
    return 0;
  }
  int ret = 0;
  /* Even though we know the lhs is supposed to be a ptr, we shouldn't
  put that info into the context when type checking it. */
  struct ast_typeexpr lhs_partial_type = ast_unknown_garbage();

  struct ast_expr annotated_lhs;
  if (!check_expr(es, x->lhs, &lhs_partial_type, &annotated_lhs)) {
    goto cleanup;
  }

  struct ast_typeexpr *ptr_target;
  if (!view_ptr_target(es->cs->im, ast_expr_type(&annotated_lhs),
                       &ptr_target)) {
    METERR(x->meta, "Dereferencing field access expects ptr type.%s", "\n");
    goto cleanup_lhs;
  }

  struct ast_typeexpr field_type;
  if (!lookup_field_type(es,
                         ptr_target,
                         &x->fieldname,
                         &field_type)) {
    goto cleanup_lhs;
  }

  if (!unify_directionally(partial_type, &field_type)) {
    METERR(x->meta, "Dereferencing field access results in wrong type.%s", "\n");
    goto cleanup_field_type;
  }

  *out = field_type;
  struct ast_fieldname fieldname;
  ast_fieldname_init_copy(&fieldname, &x->fieldname);
  ast_deref_field_access_init(annotated_out, ast_meta_make_copy(&x->meta),
                              annotated_lhs, fieldname);
  ret = 1;
  goto cleanup_lhs;

 cleanup_field_type:
  ast_typeexpr_destroy(&field_type);
 cleanup_lhs:
  if (!ret) {
    ast_expr_destroy(&annotated_lhs);
  }
 cleanup:
  ast_typeexpr_destroy(&lhs_partial_type);
  return ret;
}

int check_is_index_rhs_type(struct checkstate *cs, struct ast_meta *expr_meta,
                            struct ast_typeexpr *type) {
  if (!(type->tag == AST_TYPEEXPR_NAME
        && (type->u.name.value == identmap_intern_c_str(cs->im, I32_TYPE_NAME)
            || type->u.name.value == identmap_intern_c_str(cs->im, U32_TYPE_NAME)
            || type->u.name.value == identmap_intern_c_str(cs->im, SIZE_TYPE_NAME)))) {
    METERR(*expr_meta, "Invalid index expr rhs type.%s", "\n");
    return 0;
  }
  return 1;
}

int check_index_expr(struct exprscope *es,
                     struct ast_index_expr *a,
                     struct ast_typeexpr *partial_type,
                     struct ast_expr *annotated_out) {
  struct ast_typeexpr no_partial_type = ast_unknown_garbage();;

  struct ast_expr lhs_annotated;
  if (!check_expr(es, a->lhs, &no_partial_type, &lhs_annotated)) {
    goto fail;
  }

  struct ast_expr rhs_annotated;
  if (!check_expr(es, a->rhs, &no_partial_type, &rhs_annotated)) {
    goto fail_lhs_annotated;
  }

  if (!check_is_index_rhs_type(es->cs, &a->meta, ast_expr_type(&rhs_annotated))) {
    goto fail_rhs_annotated;
  }

  struct ast_typeexpr *lhs_type = ast_expr_type(&lhs_annotated);

  struct ast_typeexpr *lhs_target;
  if (!view_ptr_target(es->cs->im, lhs_type, &lhs_target)) {
    if (lhs_type->tag != AST_TYPEEXPR_ARRAY) {
      METERR(a->meta, "Indexing into a non-pointer, non-array type.%s", "\n");
      goto fail_rhs_annotated;
    }

    lhs_target = lhs_type->u.arraytype.param;
  }

  if (!unify_directionally(partial_type, lhs_target)) {
    METERR(a->meta, "Indexing returns wrong type.%s", "\n");
    goto fail_rhs_annotated;
  }

  struct ast_typeexpr return_type;
  ast_typeexpr_init_copy(&return_type, lhs_target);

  struct ast_expr_info expr_info;

  if (lhs_type->tag == AST_TYPEEXPR_ARRAY) {
    expr_info = expr_info_typechecked_subobject(return_type, &lhs_annotated.info);
  } else {
    expr_info = ast_expr_info_typechecked_no_temporary(1, return_type);
  }

  ast_typeexpr_destroy(&no_partial_type);

  ast_expr_partial_init(annotated_out, AST_EXPR_INDEX, expr_info);
  ast_index_expr_init(&annotated_out->u.index_expr,
                      ast_meta_make_copy(&a->meta),
                      lhs_annotated,
                      rhs_annotated);
  return 1;

 fail_rhs_annotated:
  ast_expr_destroy(&rhs_annotated);
 fail_lhs_annotated:
  ast_expr_destroy(&lhs_annotated);
 fail:
  ast_typeexpr_destroy(&no_partial_type);
  return 0;
}

void replace_name_expr_params(struct exprscope *es,
                              struct ast_name_expr *x,
                              struct ast_name_expr *out) {
  struct ast_name_expr copy;
  ast_name_expr_init_copy(&copy, x);

  if (copy.has_params) {
    for (size_t i = 0, e = copy.params_count; i < e; i++) {
      struct ast_typeexpr replaced;
      replace_generics(es, &copy.params[i], &replaced);
      ast_typeexpr_destroy(&copy.params[i]);
      copy.params[i] = replaced;
    }
  }

  *out = copy;
}

int check_expr_ai(struct exprscope *es,
                  enum allow_incomplete ai,
                  struct ast_expr *x,
                  struct ast_typeexpr *partial_type,
                  struct ast_expr *annotated_out) {
  (void)ai;  /* TODO: Use ai. */
  switch (x->tag) {
  case AST_EXPR_NAME: {
    struct ast_name_expr replaced_name;
    replace_name_expr_params(es, &x->u.name, &replaced_name);

    struct ast_typeexpr name_type;
    int is_lvalue;
    struct def_instantiation *inst_or_null;
    if (!exprscope_lookup_name(es, &replaced_name, partial_type,
                               &name_type, &is_lvalue, &inst_or_null)) {
      ast_name_expr_destroy(&replaced_name);
      return 0;
    }
    ast_expr_partial_init(annotated_out, AST_EXPR_NAME,
                          ast_expr_info_typechecked_no_temporary(is_lvalue, name_type));
    annotated_out->u.name = replaced_name;
    ast_name_expr_info_mark_inst(&annotated_out->u.name.info, inst_or_null);
    return 1;
  } break;
  case AST_EXPR_NUMERIC_LITERAL: {
    struct ast_typeexpr num_type;
    numeric_literal_type(es->cs->im, &x->u.numeric_literal, &num_type);
    if (!unify_directionally(partial_type, &num_type)) {
      METERR(x->u.numeric_literal.meta, "Numeric literal in bad place.%s", "\n");
      ast_typeexpr_destroy(&num_type);
      return 0;
    }
    ast_expr_partial_init(annotated_out, AST_EXPR_NUMERIC_LITERAL,
                          ast_expr_info_typechecked_no_temporary(0, num_type));
    ast_numeric_literal_init_copy(&annotated_out->u.numeric_literal,
                                  &x->u.numeric_literal);
    return 1;
  } break;
  case AST_EXPR_CHAR_LITERAL: {
    struct ast_typeexpr char_type;
    init_name_type(&char_type, identmap_intern_c_str(es->cs->im, CHAR_STANDIN_TYPE_NAME));
    if (!unify_directionally(partial_type, &char_type)) {
      METERR(x->u.char_literal.meta, "Character literal in bad place.%s", "\n");
      ast_typeexpr_destroy(&char_type);
      return 0;
    }
    ast_expr_partial_init(annotated_out, AST_EXPR_CHAR_LITERAL,
                          ast_expr_info_typechecked_no_temporary(0, char_type));
    ast_char_literal_init_copy(&annotated_out->u.char_literal,
                               &x->u.char_literal);
    return 1;
  } break;
  case AST_EXPR_STRING_LITERAL: {
    if (es->computation == STATIC_COMPUTATION_YES) {
      METERR(*ast_expr_ast_meta(x), "String literals disallowed in static computation.%s", "\n");
      return 0;
    }
    uint32_t array_size = size_to_uint32(x->u.string_literal.values_count);
    struct ast_typeexpr array_type;
    {
      struct ast_typeexpr char_type;
      init_name_type(&char_type, identmap_intern_c_str(es->cs->im, CHAR_STANDIN_TYPE_NAME));
      array_type.tag = AST_TYPEEXPR_ARRAY;
      ast_arraytype_init(&array_type.u.arraytype, ast_meta_make_garbage(), array_size, char_type);
    }
    if (!unify_directionally(partial_type, &array_type)) {
      METERR(x->u.string_literal.meta, "Character literal in bad place.%s", "\n");
      ast_typeexpr_destroy(&array_type);
      return 0;
    }

    ast_expr_partial_init(annotated_out, AST_EXPR_STRING_LITERAL,
                          ast_expr_info_typechecked_no_temporary(0, array_type));
    ast_string_literal_init_copy(&annotated_out->u.string_literal,
                                 &x->u.string_literal);
    return 1;
  } break;
  case AST_EXPR_FUNCALL: {
    return check_expr_funcall(es, &x->u.funcall, partial_type, annotated_out);
  } break;
  case AST_EXPR_INDEX: {
    return check_index_expr(es, &x->u.index_expr, partial_type, annotated_out);
  } break;
  case AST_EXPR_UNOP: {
    return check_expr_unop(es, &x->u.unop_expr, partial_type, annotated_out);
  } break;
  case AST_EXPR_BINOP: {
    struct ast_typeexpr return_type;
    int is_lvalue;
    if (!check_expr_binop(es, &x->u.binop_expr, partial_type,
                          &return_type, &is_lvalue, &annotated_out->u.binop_expr)) {
      return 0;
    }
    /* Assignment opers return no temporary, and logical opers return
    a trivial temporary. */
    ast_expr_partial_init(annotated_out, AST_EXPR_BINOP,
                          ast_expr_info_typechecked_no_or_trivial_temporary(is_lvalue, return_type));
    return 1;
  } break;
  case AST_EXPR_LAMBDA: {
    struct ast_typeexpr type;
    if (!check_expr_lambda(es, &x->u.lambda, partial_type,
                           &type, &annotated_out->u.lambda)) {
      return 0;
    }
    /* Function pointers (whenever they get supported) for C-style
    functions are trivial.*/
    ast_expr_partial_init(annotated_out, AST_EXPR_LAMBDA,
                          ast_expr_info_typechecked_trivial_temporary(0, type));
    return 1;
  } break;
  case AST_EXPR_LOCAL_FIELD_ACCESS: {
    return check_expr_local_field_access(es, &x->u.local_field_access,
                                         partial_type,
                                         annotated_out);
  } break;
  case AST_EXPR_DEREF_FIELD_ACCESS: {
    struct ast_typeexpr type;
    if (!check_expr_deref_field_access(es, &x->u.deref_field_access,
                                       partial_type, &type,
                                       &annotated_out->u.deref_field_access)) {
      return 0;
    }
    /* No temporary, because we're derefing a pointer. */
    ast_expr_partial_init(annotated_out,
                          AST_EXPR_DEREF_FIELD_ACCESS,
                          ast_expr_info_typechecked_no_temporary(1, type));
    return 1;
  } break;
  case AST_EXPR_TYPED: {
    struct ast_typeexpr replaced_partial_type;
    replace_generics(es, &x->u.typed_expr.type, &replaced_partial_type);

    struct ast_expr annotated_lhs;
    if (!check_expr(es, x->u.typed_expr.expr, &replaced_partial_type,
                    &annotated_lhs)) {
      ast_typeexpr_destroy(&replaced_partial_type);
      return 0;
    }
    ast_typeexpr_destroy(&replaced_partial_type);

    struct ast_typeexpr old_type_copy;
    ast_typeexpr_init_copy(&old_type_copy, &x->u.typed_expr.type);
    ast_expr_partial_init(annotated_out, AST_EXPR_TYPED,
                          ast_expr_info_typechecked_identical(
                              &annotated_lhs.info));
    ast_typed_expr_init(&annotated_out->u.typed_expr,
                        ast_meta_make_copy(&x->u.typed_expr.meta),
                        old_type_copy,
                        annotated_lhs);
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}

int check_expr(struct exprscope *es,
               struct ast_expr *x,
               struct ast_typeexpr *partial_type,
               struct ast_expr *annotated_out) {
  return check_expr_ai(es, ALLOW_INCOMPLETE_NO, x, partial_type, annotated_out);
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
    int zero_defs_discard;
    struct ast_typeexpr unified;
    struct def_entry *ent;
    struct def_instantiation *inst;
    int success = name_table_match_def(cs->im,
                                       &cs->nt,
                                       &a->name,
                                       NULL, 0, /* (no generics) */
                                       ast_def_typeexpr(a),
                                       &zero_defs_discard,
                                       &unified,
                                       &ent,
                                       &inst);
    CHECK(success);
    CHECK(exact_typeexprs_equal(&unified, ast_def_typeexpr(a)));
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
      ret = check_expr(&es, &a->rhs, ast_def_typeexpr(a), &annotated_rhs);
      if (ret) {
        di_set_annotated_rhs(inst, annotated_rhs);
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

int check_toplevel(struct checkstate *cs, struct ast_toplevel *a);

int check_toplevels(struct checkstate *cs,
                    struct ast_toplevel *toplevels,
                    size_t toplevels_count) {
  for (size_t i = 0; i < toplevels_count; i++) {
    if (!check_toplevel(cs, &toplevels[i])) {
      return 0;
    }
  }
  return 1;
}

int check_toplevel(struct checkstate *cs, struct ast_toplevel *a) {
  switch (a->tag) {
  case AST_TOPLEVEL_IMPORT:
    /* We already parsed and loaded the import. */
    return 1;
  case AST_TOPLEVEL_DEF:
    return check_def(cs, &a->u.def);
  case AST_TOPLEVEL_EXTERN_DEF:
    return check_extern_def(cs, &a->u.extern_def);
  case AST_TOPLEVEL_DEFTYPE:
    return check_deftype(cs, lookup_deftype(&cs->nt, &a->u.deftype));
  case AST_TOPLEVEL_ACCESS: {
    /* Check that the type the access block refers to actually exists, and is a class. */
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(&cs->nt, a->u.access.name.value, a->u.access.arity,
                                   &ent)) {
      METERR(a->u.access.meta, "access block refers to non-existant type.%s", "\n");
      return 0;
    }
    if (ent->is_primitive || ent->deftype->disposition == AST_DEFTYPE_NOT_CLASS) {
      METERR(a->u.access.meta, "access block refers to non-class type.%s", "\n");
      return 0;
    }
    return check_toplevels(cs, a->u.access.toplevels, a->u.access.toplevels_count);
  } break;
  default:
    UNREACHABLE();
  }
}

#define NUMERIC_LITERAL_OOR "Numeric literal out of range.\n"

int numeric_literal_to_u32(int8_t *digits, size_t digits_count,
                           uint32_t *out) {
  CHECK(digits_count > 0);
  uint32_t built_value = 0;
  for (size_t i = 0; i < digits_count; i++) {
    if (!try_uint32_mul(built_value, 10, &built_value)) {
      ERR_DBG(NUMERIC_LITERAL_OOR);
      return 0;
    }
    if (!try_uint32_add(built_value, digits[i], &built_value)) {
      ERR_DBG(NUMERIC_LITERAL_OOR);
      return 0;
    }
  }

  *out = built_value;
  return 1;
}

int numeric_literal_to_i32(int8_t *digits, size_t digits_count,
                           int32_t *out) {
  /* TODO: There's no way to plainly represent INT32_MIN.  We should
  get static evaluation of "arbitrary numeric constants"
  implemented. */
  uint32_t value;
  if (!numeric_literal_to_u32(digits, digits_count, &value)) {
    return 0;
  }
  if (value > 0x7FFFFFFFul) {
    ERR_DBG(NUMERIC_LITERAL_OOR);
    return 0;
  }
  CHECK(value <= INT32_MAX);
  *out = (int32_t)value;
  return 1;
}

int eval_static_numeric_literal(struct ast_numeric_literal *a,
                                struct static_value *out) {
  switch (a->numeric_type) {
  case AST_NUMERIC_TYPE_SIGNED: {
    int32_t value;
    if (!numeric_literal_to_i32(a->digits, a->digits_count, &value)) {
      return 0;
    }
    static_value_init_i32(out, value);
    return 1;
  } break;
  case AST_NUMERIC_TYPE_UNSIGNED: {
    uint32_t value;
    if (!numeric_literal_to_u32(a->digits, a->digits_count, &value)) {
      return 0;
    }
    static_value_init_u32(out, value);
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}

int eval_static_value(struct ast_expr *expr, struct static_value *out);

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
    default:
      UNREACHABLE();
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

int eval_static_funcall(struct ast_funcall *funcall,
                        struct static_value *out) {
  int ret = 0;
  struct static_value func_value;
  if (!eval_static_value(funcall->func, &func_value)) {
    goto cleanup;
  }

  size_t params_count = funcall->args_count;
  struct static_value *params = malloc_mul(sizeof(*params), params_count);
  size_t i = 0;
  for (; i < params_count; i++) {
    if (!eval_static_value(&funcall->args[i].expr, &params[i])) {
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
int eval_static_value(struct ast_expr *expr,
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
    return eval_static_numeric_literal(&expr->u.numeric_literal, out);
  case AST_EXPR_CHAR_LITERAL:
    static_value_init_u8(out, expr->u.char_literal.value);
    return 1;
  case AST_EXPR_STRING_LITERAL:
    CRASH("String literal static value evaluation is not supported.\n");
  case AST_EXPR_FUNCALL:
    return eval_static_funcall(&expr->u.funcall, out);
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
    return eval_static_value(expr->u.typed_expr.expr, out);
  default:
    UNREACHABLE();
  }
}

int compute_static_values(struct name_table *nt, struct def_entry *ent) {
  int is_primitive = ent->is_primitive;
  CHECK(is_primitive || ent->def != NULL);
  for (size_t i = 0, e = ent->instantiations_count; i < e; i++) {
    struct def_instantiation *inst = ent->instantiations[i];

    if (is_primitive) {
      switch (ent->primitive_op.tag) {
      case PRIMITIVE_OP_SIZEOF: {
        CHECK(inst->substitutions_count == 1);
        uint32_t size = kira_sizeof(nt, &inst->substitutions[0]);
        static_value_init_u32(di_value_for_set(inst), size);
      } break;
      case PRIMITIVE_OP_ALIGNOF: {
        CHECK(inst->substitutions_count == 1);
        uint32_t alignment = kira_alignof(nt, &inst->substitutions[0]);
        static_value_init_u32(di_value_for_set(inst), alignment);
      } break;
      default:
        static_value_init_primitive_op(di_value_for_set(inst), ent->primitive_op);
        break;
      }
    } else {
      struct static_value value;
      if (!eval_static_value(di_annotated_rhs(inst), &value)) {
        return 0;
      }
      static_value_init_move(di_value_for_set(inst), &value);
    }
  }

  return 1;
}

int chase_def_entry_acyclicity(struct name_table *nt, struct def_entry *ent) {
  if (ent->known_acyclic) {
    return 1;
  }
  if (ent->acyclicity_being_chased) {
    CHECK(ent->def);
    METERR(ent->def->meta, "Cyclic reference in static expressions.%s", "\n");
    return 0;
  }
  ent->acyclicity_being_chased = 1;
  for (size_t i = 0, e = ent->static_references_count; i < e; i++) {
    if (!chase_def_entry_acyclicity(nt, ent->static_references[i])) {
      return 0;
    }
  }
  CHECK(ent->acyclicity_being_chased == 1);
  ent->acyclicity_being_chased = 0;
  CHECK(ent->known_acyclic == 0);
  ent->known_acyclic = 1;

  if (!ent->is_extern) {
    if (!compute_static_values(nt, ent)) {
      return 0;
    }
  }

  return 1;
}

int check_def_acyclicity(struct checkstate *cs) {
  for (size_t i = 0, e = cs->nt.defs_count; i < e; i++) {
    struct def_entry *ent = cs->nt.defs[i];
    if (!chase_def_entry_acyclicity(&cs->nt, ent)) {
      return 0;
    }
  }
  return 1;
}

int chase_modules_and_typecheck(struct checkstate *cs,
                                module_loader *loader,
                                ident_value first_module) {
  checkstate_import_primitives(cs);

  int ret = 0;
  if (!chase_imports(cs, loader, first_module)) {
    goto fail;
  }

  for (size_t i = 0, e = cs->imports_count; i < e; i++) {
    struct ast_file *file = cs->imports[i].file;
    if (!check_toplevels(cs, file->toplevels, file->toplevels_count)) {
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

int test_check_module(struct identmap *im, module_loader *loader,
                      ident_value name) {
  struct checkstate cs;
  checkstate_init(&cs, im);

  int ret = chase_modules_and_typecheck(&cs, loader, name);

  checkstate_destroy(&cs);
  return ret;
}

int read_module_file(const uint8_t *module_name,
                     size_t module_name_count,
                     uint8_t **data_out,
                     size_t *data_size_out) {
  int ret = 0;
  char *filename;
  size_t filename_count;
  alloc_half_strcat(module_name, module_name_count,
                    ".ki",
                    &filename, &filename_count);

  if (!read_file(filename, data_out, data_size_out)) {
    ERR("Could not read file %.*s.\n",
        size_to_int(filename_count), filename);
  } else {
    ret = 1;
  }

  free(filename);
  return ret;
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

int check_file_test_1(const uint8_t *name, size_t name_count,
                      uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "import bar;\n"
                               "\n"
                               "def x i32 = 3;"
                               "deftype dword u32;\n" },
                             { "bar",
                               "import foo;\n"
                               "\n"
                               "def y u32 = 5u;\n" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}


int check_file_test_2(const uint8_t *name, size_t name_count,
                      uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;"
                               "deftype dword u32;\n"
                               "deftype blah dword;\n"
                               "deftype feh ptr[blah];\n"
                               "deftype quux ptr[quux];\n" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_3(const uint8_t *name, size_t name_count,
                      uint8_t **data_out, size_t *data_count_out) {
  /* An invalid file: bar and foo recursively hold each other. */
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;"
                               "deftype foo bar;\n"
                               "deftype bar foo;\n" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_4(const uint8_t *name, size_t name_count,
                      uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;"
                               "deftype foo struct { "
                               "x u32; y i32; z ptr[foo]; };\n" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_5(const uint8_t *name, size_t name_count,
                      uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;"
                               "deftype[T] foo T;" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_6(const uint8_t *name, size_t name_count,
                      uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;"
                               "deftype[T] foo struct { "
                               "count u32; p ptr[T]; };\n" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_7(const uint8_t *name, size_t name_count,
                      uint8_t **data_out, size_t *data_count_out) {
  /* This fails because bar recursively holds itself through a
  template parameter. */
  struct test_module a[] = { { "foo",
                               "deftype[T, U] foo struct { x ptr[T]; y U; };\n"
                               "deftype bar struct { z foo[u32, bar]; };\n" }
  };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_8(const uint8_t *name, size_t name_count,
                      uint8_t **data_out, size_t *data_count_out) {
  /* But here bar holds itself indirectly. */
  struct test_module a[] = { { "foo",
                               "deftype[T, U] foo struct { x ptr[T]; y U; };\n"
                               "deftype bar struct { z foo[bar, u32]; };\n" }
  };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_def_1(const uint8_t *name, size_t name_count,
                          uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;\n" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_def_2(const uint8_t *name, size_t name_count,
                          uint8_t **data_out, size_t *data_count_out) {
  /* Fails because numeric literals are dumb and have type i32. */
  struct test_module a[] = { { "foo",
                               "def x u32 = 3;\n" } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_def_3(const uint8_t *name, size_t name_count,
                          uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "def[] x i32 = 3;\n"
                               "def y i32 = x;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_1(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;\n"
                               "def y fn[i32, i32] = func(z i32)i32 {\n"
                               "  return x;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_4(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  /* Fails because k is a u32. */
  struct test_module a[] = { { "foo",
                               "def k u32 = k;\n"
                               "def x i32 = 3;\n"
                               "def y fn[i32, i32] = func(z i32)i32 {\n"
                               "  return k;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_5(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;\n"
                               "def y fn[i32, i32] = func(z i32)i32 {\n"
                               "  var x i32 = 4;\n"
                               "  return z;\n"
                               "};\n"
    } };
  /* Passes despite x shadowing a global, because that's allowed.. */

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_6(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  /* Fails because z shadows a local. */
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;\n"
                               "def y fn[i32, i32] = func(z i32)i32 {\n"
                               "  var z i32 = 4;\n"
                               "  return x;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_7(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;\n"
                               "def y fn[i32, i32] = func(z i32)i32 {\n"
                               "  return x + z + 5;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_8(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  /* Fails because x is a u32. */
  struct test_module a[] = { { "foo",
                               "def x u32 = x;\n"
                               "def y fn[i32, i32] = func(z i32)i32 {\n"
                               "  return x + z + 5;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_9(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;\n"
                               "def y fn[i32, i32] = func(z i32)i32 {\n"
                               "  return -x + z + -5;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_10(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  /* Fails because you can't negate a u32. */
  struct test_module a[] = { { "foo",
                               "def x u32 = 3;\n"
                               "def y fn[i32, i32] = func(z i32)i32 {\n"
                               "  return -x;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_11(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "deftype foo struct { x i32; y i32; };\n"
                               "def y fn[foo, i32] = func(z foo) i32 {\n"
                               "  return z.x;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_12(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  /* Fails because the field x has type u32. */
  struct test_module a[] = { { "foo",
                               "deftype foo struct { x u32; y i32; };\n"
                               "def y fn[foo, i32] = func(z foo) i32 {\n"
                               "  return z.x;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_13(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "deftype[T] foo struct { x T; y i32; };\n"
      "def y fn[foo[i32], i32] = func(z foo[i32]) i32 {\n"
      "  return z.x + z.y;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_14(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  /* Fails because z.x is a u32. */
  struct test_module a[] = { {
      "foo",
      "deftype[T] foo struct { x T; y i32; };\n"
      "def y fn[foo[u32], i32] = func(z foo[u32]) i32 {\n"
      "  return z.x + z.y;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_15(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "def y fn[i32, i32] = func(z i32) i32 {\n"
                               "  var k fn[i32, i32] = func(m i32) i32 {\n"
                               "    return m + m;\n"
                               "  };\n"
                               "  return k(z) + k(z);\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_16(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  /* Fails because the inner lambda tries to capture "z". */
  struct test_module a[] = { { "foo",
                               "def y fn[i32, i32] = func(z i32) i32 {\n"
                               "  var k fn[i32, i32] = func(m i32) i32 {\n"
                               "    return m + z;\n"
                               "  };\n"
                               "  return k(z) + k(z);\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_17(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "deftype foo struct { x i32; y i32; };\n"
      "def y fn[ptr[foo], i32] = func(z ptr[foo]) i32 {\n"
      "  return z->x;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_18(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "deftype[T] foo struct { x T; y i32; };\n"
      "def y fn[ptr[foo[i32]], i32] = func(z ptr[foo[i32]]) i32 {\n"
      "  return z->x + z->y;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_19(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "deftype[T] foo struct { x T; y i32; };\n"
      "def y fn[ptr[foo[i32]], i32] = func(z ptr[foo[i32]]) i32 {\n"
      "  return (*z).x + (&(*z))->y;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_20(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "deftype[T] foo struct { x T; y i32; };\n"
      "def y fn[ptr[foo[i32]], i32] = func(z ptr[foo[i32]]) i32 {\n"
      "  if (z->x < 3 && z->y > 19) {\n"
      "    z->x = (*z).y + 5;\n"
      "  }\n"
      "  return (*z).x + (&(*z))->y;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_21(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  /* Fails because assignment mismatches types. */
  struct test_module a[] = { {
      "foo",
      "deftype[T] foo struct { x T; y i32; };\n"
      "def y fn[ptr[foo[i32]], i32] = func(z ptr[foo[i32]]) i32 {\n"
      "  if (z->x < 3 && z->y > 19) {\n"
      "    z->x = z;\n"
      "  }\n"
      "  return (*z).x + (&(*z))->y;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_22(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def[T] foo fn[ptr[T], T] = func(x ptr[T]) T { return *x; };\n"
      "def bar fn[i32] = func() i32 {\n"
      "  var x i32 = 3;\n"
      "  return foo(&x);\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_23(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  /* Fails because the def does not match. */
  struct test_module a[] = { {
      "foo",
      "def[T] foo fn[ptr[T], T] = func(x ptr[T]) T { return *x; };\n"
      "def bar fn[i32] = func() i32 {\n"
      "  var x i32 = 3;\n"
      "  return foo(x);\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_24(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def[T] fac fn[T, T] = func(x T) T {\n"
      "  if (x == 0) {\n"
      "    return 1;\n"
      "  } else {\n"
      "    return x * fac(x - 1);\n"
      "  }\n"
      "};\n"
      "def bar fn[i32] = func() i32 {\n"
      "  var x i32 = 5;\n"
      "  return fac(x);\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_25(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  /* Fails because of recursive template instantiation. */
  struct test_module a[] = { {
      "foo",
      "deftype[T] foo struct { x i32; };\n"
      "def[T] biggefy fn[T, foo[T]] = func(x T) foo[T] {\n"
      "  return biggefy(x);\n"
      "};\n"
      "def[T] rec fn[T, i32] = func(x T) i32 {\n"
      "  return rec(biggefy(x));\n"
      "};\n"
      "def bar fn[i32] = func() i32 {\n"
      "  var x u32 = 5u;\n"
      "  return rec(x);\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_26(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def x i32 = 3;\n"
      "def y u32 = 3u + 4u;\n"
      "def z fn[i32, i32] = func(k i32) i32 { return k + 1; };\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_27(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  /* Fails because you can't evaluate z(3) statically. */
  struct test_module a[] = { {
      "foo",
      "def x i32 = z(3);\n"
      "def z fn[i32, i32] = func(k i32) i32 { return k + 1; };\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_28(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def y i32 = -x;\n"
      "def x i32 = -3;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_29(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  /* Fails because of cyclic reference. */
  struct test_module a[] = { {
      "foo",
      "def y i32 = -x;\n"
      "def x i32 = -y;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_extern_1(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "extern putchar fn[i32, i32];\n"
      "def foo fn[i32] = func()i32 {\n"
      "  putchar(65);\n"
      "  putchar(10);\n"
      "  return 1;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_extern_2(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  /* Fails because putchar is called with the wrong type. */
  struct test_module a[] = { {
      "foo",
      "extern putchar fn[i32, i32];\n"
      "def foo fn[i32] = func()i32 {\n"
      "  putchar(65u);\n"
      "  putchar(10);\n"
      "  return 1;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_extern_3(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  /* Fails because putchar has a nonsense return type. */
  struct test_module a[] = { {
      "foo",
      "extern putchar fn[i32, quack];\n"
      "def foo fn[i32] = func()i32 {\n"
      "  putchar(65);\n"
      "  putchar(10);\n"
      "  return 1;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_1(const uint8_t *name, size_t name_count,
                           uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def foo fn[u32, u32] = func(x u32) u32 {\n"
      "  return x + 4u;\n"
      "};"
      "def bar fn[i32, i32] = foo;\n"
      "def foo fn[i32, i32] = func(x i32) i32 {\n"
      "  return x + 3;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_2(const uint8_t *name, size_t name_count,
                           uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def foo fn[u32, u32] = func(x u32) u32 {\n"
      "  return x + 4u;\n"
      "};"
      "def bar fn[i32, i32] = foo;\n"
      "def[T] foo fn[T, i32] = func(x T) i32 {\n"
      "  return x + 3;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_3(const uint8_t *name, size_t name_count,
                           uint8_t **data_out, size_t *data_count_out) {
  /* Fails because foo's instatiation won't typecheck. */
  struct test_module a[] = { {
      "foo",
      "def foo fn[u32, u32] = func(x u32) u32 {\n"
      "  return x + 4u;\n"
      "};"
      "def bar fn[u32, i32] = foo;\n"
      "def[T] foo fn[T, i32] = func(x T) i32 {\n"
      "  return x + 3;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_4(const uint8_t *name, size_t name_count,
                           uint8_t **data_out, size_t *data_count_out) {
  /* Fails because foo lacks a return statement. */
  /* Unfortunately we don't check (yet) that _all_ paths return. */
  struct test_module a[] = { {
      "foo",
      "def foo fn[u32, u32] = func(x u32) u32 {\n"
      "  x + x;\n"
      "};"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_5(const uint8_t *name, size_t name_count,
                           uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def foo i32 = 7;\n"
      "def bar i32 = 5 << foo;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_6(const uint8_t *name, size_t name_count,
                           uint8_t **data_out, size_t *data_count_out) {
  /* Fails because shift overflows. */
  struct test_module a[] = { {
      "foo",
      "def foo i32 = 30;\n"
      "def bar i32 = 5 << foo;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_7(const uint8_t *name, size_t name_count,
                           uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def[T] foo fn[T, T] = func(x T) T {\n"
      "  var y T = x;\n"
      "  return y;\n"
      "};\n"
      "def bar fn[i32] = func() i32 {\n"
      "  return foo(3);\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_8(const uint8_t *name, size_t name_count,
                           uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def[T] add32 fn[i32, T, i32] = func(x i32, y T) i32 {\n"
      "  var z i32 = ~4;\n"
      "  return x + z;\n"
      "};\n"
      "def bar fn[i32] = func() i32 {\n"
      "  return add32(3, 4u);\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_9(const uint8_t *name, size_t name_count,
                           uint8_t **data_out, size_t *data_count_out) {
  /* Fails because return type in return expression is wrong. */
  struct test_module a[] = { {
      "foo",
      "def foo fn[i32] = func() i32 {\n"
      "  return 4u;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_10(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def x i32 = ~4u;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_11(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def[T] foo fn[ptr[T], i32, T] = func(p ptr[T], i i32) T {\n"
      "  var ret T = p[i];\n"
      "  p[i] = p[i + 1];\n"
      "  return ret;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_12(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails because vec3 and [3]u32 are different types. */
  struct test_module a[] = { {
      "foo",
      "deftype vec3 [3]u32;\n"
      "def foo fn[[3]u32, vec3] = func(arr [3]u32) vec3 {\n"
      "  var v vec3 = arr;\n"
      "  return v;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_13(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails because convert doesn't have a type in context.  (See
     check_file_test_more_14.) */
  struct test_module a[] = { {
      "foo",
      "def foo fn[i32] = func() i32 {\n"
      "  return 2 + ~3u;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_14(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def foo fn[i32] = func() i32 {\n"
      "  return 2 + @[i32](~3u);\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_15(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def foo fn[i32] = func() i32 {\n"
      "  var x i32 = 5;\n"
      "  while (x > 3) {\n"
      "    x = x + 1;\n"
      "  }\n"
      "  return x;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_16(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def foo fn[i32] = func() i32 {\n"
      "  var x i32 = 5;\n"
      "  while !(x > 3) {\n"
      "    x = x + 1;\n"
      "  }\n"
      "  return x;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_17(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails because some control paths don't return a value. */
  struct test_module a[] = { {
      "foo",
      "def foo fn[i32] = func() i32 {\n"
      "  var x i32 = 2;\n"
      "  if (x < 3) {\n"
      "    x = x + 1;\n"
      "  } else {\n"
      "    return x;\n"
      "  }\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_18(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def foo fn[i32] = func() i32 {\n"
      "  var acc u32 = 0u;\n"
      "  for var i u32 = 0u; i < 10u; i = i + 1u {\n"
      "    acc = acc + i;\n"
      "  }\n"
      "  return ~acc;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_19(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails because in baz, we can't figure out type of foo instantiation. */
  struct test_module a[] = { {
      "foo",
      "def[T] foo fn[i32, T] = func(x i32) T {\n"
      "  // Why not test '@[T]' works where T is generic.\n"
      "  var y T = @[T](~x);\n"
      "  return y;\n"
      "};\n"
      "def bar fn[fn[i32, i16], i32, i16] = func(x fn[i32, i16], y i32) i16 {\n"
      "  return x(y);\n"
      "};\n"
      "def baz fn[i16] = func() i16 {\n"
      "  return bar(foo, 4);\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_20(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def[T] foo fn[i32, T] = func(x i32) T {\n"
      "  // Why not test '@[T]' works where T is generic.\n"
      "  var y T = @[T]~x;\n"
      "  return y;\n"
      "};\n"
      "def bar fn[fn[i32, i16], i32, i16] = func(x fn[i32, i16], y i32) i16 {\n"
      "  return x(y);\n"
      "};\n"
      "def baz fn[i16] = func() i16 {\n"
      "  return bar(foo@[i16], 4);\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_21(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "deftype ty struct { x i32; y i32; };\n"
      "def foo fn[size] = func() size {\n"
      "  return sizeof@[ty];\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_22(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails because whatever is not the name of a defclass type. */
  struct test_module a[] = { {
      "foo",
      "defclass ty struct { x i32; y i32; };\n"
      "access whatever {\n"
      "def foo fn[*ty, i32] = func(t *ty) i32 {\n"
      "  return t->x;\n"
      "};\n"
      "}\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_23(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails because ty[] has bad arity. */
  struct test_module a[] = { {
      "foo",
      "defclass ty struct { x i32; y i32; };\n"
      "access ty[] {\n"
      "def foo fn[*ty, i32] = func(t *ty) i32 {\n"
      "  return t->x;\n"
      "};\n"
      "}\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_24(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails because ty[_, _] has bad arity. */
  struct test_module a[] = { {
      "foo",
      "defclass ty struct { x i32; y i32; };\n"
      "access ty[_, _] {\n"
      "def foo fn[*ty, i32] = func(t *ty) i32 {\n"
      "  return t->x;\n"
      "};\n"
      "}\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_25(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "defclass ty struct { x i32; y i32; };\n"
      "access ty {\n"
      "def foo fn[*ty, i32] = func(t *ty) i32 {\n"
      "  return t->x;\n"
      "};\n"
      "}\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_26(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "defclass[T] ty struct { x T; y T; };\n"
      "access ty[_] {\n"
      "def[T] foo fn[*ty[T], T] = func(t *ty[T]) T {\n"
      "  return t->x;\n"
      "};\n"
      "}\n"
      "def bar fn[*ty[i32], i32] = foo;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_27(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails because we try to access a field of a defclass type. */
  struct test_module a[] = { {
      "foo",
      "defclass[T] ty struct { x T; y T; };\n"
      "def[T] foo fn[*ty[T], T] = func(t *ty[T]) T {\n"
      "  return t->x;\n"
      "};\n"
      "def bar fn[*ty[i32], i32] = foo;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_28(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "deftype ty i32;\n"
      "def foo fn[ty, i32] = func(t ty) i32 {\n"
      "  return t.~;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_29(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails (unlike more_28) because ty is defclass, and the conversion
  operator is private. */
  struct test_module a[] = { {
      "foo",
      "defclass ty i32;\n"
      "def foo fn[ty, i32] = func(t ty) i32 {\n"
      "  return t.~;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_30a(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  /* Fails because the type ty lacks an explicit destructor. */
  struct test_module a[] = { {
      "foo",
      "defclass ty i32;\n"
      "access ty {\n"
      "def foo fn[ty, i32] = func(t ty) i32 {\n"
      "  return t.~;\n"
      "};\n"
      "}\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_30b(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "defclass copy ty i32;\n"
      "access ty {\n"
      "def foo fn[ty, i32] = func(t ty) i32 {\n"
      "  return t.~;\n"
      "};\n"
      "}\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_31(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails (like more_29) because ty is defclass, and the conversion
  operator is private. */
  struct test_module a[] = { {
      "foo",
      "defclass[T] ty i32;\n"
      "def[T] foo fn[ty[T], i32] = func(t ty[T]) i32 {\n"
      "  return t.~;\n"
      "};\n"
      "def bar fn[ty[u32], i32] = foo;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_32a(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  /* Fails because ty[u32] lacks an explicit destructor. */
  struct test_module a[] = { {
      "foo",
      "defclass[T] ty i32;\n"
      "access ty[_] {\n"
      "def[T] foo fn[ty[T], i32] = func(t ty[T]) i32 {\n"
      "  return t.~;\n"
      "};\n"
      "}\n"
      "def bar fn[ty[u32], i32] = foo;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_32b(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "defclass[T] copy ty i32;\n"
      "access ty[_] {\n"
      "def[T] foo fn[ty[T], i32] = func(t ty[T]) i32 {\n"
      "  return t.~;\n"
      "};\n"
      "}\n"
      "def bar fn[ty[u32], i32] = foo;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_33(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails (unlike more_28) because ty is defclass, and the conversion
  operator is private. */
  struct test_module a[] = { {
      "foo",
      "defclass ty i32;\n"
      "def foo fn[*ty, *i32] = func(t *ty) *i32 {\n"
      "  return &t->~;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_34(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "defclass ty i32;\n"
      "access ty {\n"
      "def foo fn[*ty, *i32] = func(t *ty) *i32 {\n"
      "  return &t->~;\n"
      "};\n"
      "}\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_35(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails (like more_29) because ty is defclass, and the conversion
  operator is private. */
  struct test_module a[] = { {
      "foo",
      "defclass[T] ty i32;\n"
      "def[T] foo fn[*ty[T], *i32] = func(t *ty[T]) *i32 {\n"
      "  return &t->~;\n"
      "};\n"
      "def bar fn[*ty[u32], *i32] = foo;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_36(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "defclass[T] ty i32;\n"
      "access ty[_] {\n"
      "def[T] foo fn[*ty[T], *i32] = func(t *ty[T]) *i32 {\n"
      "  return &t->~;\n"
      "};\n"
      "}\n"
      "def bar fn[*ty[u32], *i32] = foo;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_37(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "defclass copy ty i32;\n"
      "access ty {\n"
      "def do_init fn[*ty, void] = func(t *ty) void {\n"
      "  var ret void;\n"
      "  return ret;\n"
      "};\n"
      "}\n"
      "def foo fn[i32] = func() i32 {\n"
      "  var k ty;\n"
      "  return 1;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_38(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails because blah is not the name of a type. */
  struct test_module a[] = { {
      "foo",
      "def foo fn[i32] = func() i32 {\n"
      "  var x blah;\n"
      "  return 1;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_39(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails because k is not default-initializable. */
  struct test_module a[] = { {
      "foo",
      "defclass copy ty i32;\n"
      "def foo fn[i32] = func() i32 {\n"
      "  var k ty;\n"
      "  return 1;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_40(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "defclass copy ty i32;\n"
      "access ty {\n"
      "def do_init fn[*ty, void] = func(t *ty) void {\n"
      "  var ret void;\n"
      "  return ret;\n"
      "};\n"
      "}\n"
      "deftype ty2 struct { x i32; y ty; };\n"
      "def foo fn[i32] = func() i32 {\n"
      "  var k ty2;\n"
      "  return 1;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_41(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails because k is not default-initializable. */
  struct test_module a[] = { {
      "foo",
      "defclass copy ty i32;\n"
      "deftype ty2 struct { x i32; y ty; };\n"
      "def foo fn[i32] = func() i32 {\n"
      "  var k ty2;\n"
      "  return 1;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_42(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def x u8 = '\\x12';\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_43(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails because the character literal is of the wrong type. */
  struct test_module a[] = { {
      "foo",
      "def x i8 = '\\x12';\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_44(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails because string literals not allowed in static evaluation. */
  /* TODO: String literals should be allowed, this should pass. */
  struct test_module a[] = { {
      "foo",
      "def x [5]u8 = \"pq\\x12rs\";\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_45(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails because the array size is wrong. */
  struct test_module a[] = { {
      "foo",
      "def x [6]u8 = \"pq\\x12rs\";\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_46(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "defenum ty {\n"
      "  c1 void;\n"
      "  c2 struct { p i32; q i32; };\n"
      "};\n"
      "def foo fn[ty, ty] = func(x ty) ty {\n"
      "  var v void;\n"
      "  var y ty = c1(v);\n"
      "  var u struct { p i32; q i32; };\n"
      "  y = c2(u);\n"
      "  return y;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_47(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails because c2 passed wrong type. */
  struct test_module a[] = { {
      "foo",
      "defenum ty {\n"
      "  c1 void;\n"
      "  c2 struct { p i32; q i32; };\n"
      "};\n"
      "def foo fn[ty, ty] = func(x ty) ty {\n"
      "  var v void;\n"
      "  var y ty = c1(v);\n"
      "  var u struct { p i32; q u32; };\n"
      "  y = c2(u);\n"
      "  return y;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}


int check_file_test_more_48(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "defenum[T] ty {\n"
      "  c1 void;\n"
      "  c2 struct { p T; q T; };\n"
      "};\n"
      "def foo fn[ty[i32], ty[i32]] = func(x ty[i32]) ty[i32] {\n"
      "  var v void;\n"
      "  var y ty[i32] = c1(v);\n"
      "  var u struct { p i32; q i32; };\n"
      "  y = c2(u);\n"
      "  return y;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_49(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails because c2 returns wrong type. */
  struct test_module a[] = { {
      "foo",
      "defenum[T] ty {\n"
      "  c1 void;\n"
      "  c2 struct { p T; q T; };\n"
      "};\n"
      "def foo fn[ty[i32], ty[i32]] = func(x ty[i32]) ty[i32] {\n"
      "  var v void;\n"
      "  var y ty[i32] = c1(v);\n"
      "  var u struct { p u32; q u32; };\n"
      "  y = c2(u);\n"
      "  return y;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_50(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "defenum ty {\n"
      "  c1 void;\n"
      "  c2 struct { p i32; q i32; };\n"
      "};\n"
      "def foo fn[ty, i32] = func(x ty) i32 {\n"
      "  switch x {\n"
      "    case c1(v void): { return -1; }\n"
      "    case c2(s struct { p i32; q i32; }): {\n"
      "      return s.p + s.q;\n"
      "    }\n"
      "  }\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_51(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  /* Fails because a control path in the switch does not return a value. */
  struct test_module a[] = { {
      "foo",
      "defenum ty {\n"
      "  c1 void;\n"
      "  c2 struct { p i32; q i32; };\n"
      "};\n"
      "def foo fn[ty, i32] = func(x ty) i32 {\n"
      "  switch x {\n"
      "    case c1(v void): { return -1; }\n"
      "    case c2(s struct { p i32; q i32; }): {\n"
      "      s.p + s.q;\n"
      "    }\n"
      "  }\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_52(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def foo fn[i32, void] = func(x i32) void {\n"
      "  var p *_ = &x;\n"
      "  var q _ = *p;\n"
      "  var r = q;\n"
      "  var s i32 = r;\n"
      "  var ret void;\n"
      "  return ret;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_53(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "defenum ty {\n"
      "  c1 void;\n"
      "  c2 struct { p i32; q i32; };\n"
      "};\n"
      "def foo fn[ty, i32] = func(x ty) i32 {\n"
      "  switch x {\n"
      "    case c1(v void): { return -1; }\n"
      "    case c2(s): {\n"
      "      return s.p + s.q;\n"
      "    }\n"
      "  }\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_54(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def foo fn[i32, void] = func(x i32) void {\n"
      "  var p *_ = &x;\n"
      "  var q _ = *p;\n"
      "  if (x == 3) {\n"
      "    return;\n"
      "  }\n"
      "  var r = q;\n"
      "  var s i32 = r;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_55(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def foo = func(x i32, y u32) void {\n"
      "  1;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_56(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "func foo(x i32, y u32) void {\n"
      "  1;\n"
      "}\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_57(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "deftype notsize u32;\n"
      "func `~`(x u32) notsize {\n"
      "  var ret notsize;\n"
      "  ret.~ = x;\n"
      "  return ret;\n"
      "}\n"
      "func foo(x u32) notsize {\n"
      "  return ~(x + 1u);\n"
      "}\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}




int test_check_file(void) {
  int ret = 0;
  struct identmap im;
  identmap_init(&im);
  ident_value foo = identmap_intern_c_str(&im, "foo");

  DBG("test_check_file check_file_test_1...\n");
  if (!test_check_module(&im, &check_file_test_1, foo)) {
    DBG("check_file_test_1 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_2...\n");
  if (!test_check_module(&im, &check_file_test_2, foo)) {
    DBG("check_file_test_2 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_3...\n");
  if (!!test_check_module(&im, &check_file_test_3, foo)) {
    DBG("!check_file_test_3 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_4...\n");
  if (!test_check_module(&im, &check_file_test_4, foo)) {
    DBG("check_file_test_4 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_5...\n");
  if (!test_check_module(&im, &check_file_test_5, foo)) {
    DBG("check_file_test_5 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_6...\n");
  if (!test_check_module(&im, &check_file_test_6, foo)) {
    DBG("check_file_test_6 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_7...\n");
  if (!!test_check_module(&im, &check_file_test_7, foo)) {
    DBG("!check_file_test_7 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_8...\n");
  if (!test_check_module(&im, &check_file_test_8, foo)) {
    DBG("check_file_test_8 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_def_1...\n");
  if (!test_check_module(&im, &check_file_test_def_1, foo)) {
    DBG("check_file_test_def_1 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_def_2...\n");
  if (!!test_check_module(&im, &check_file_test_def_2, foo)) {
    DBG("check_file_test_def_2 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_def_3...\n");
  if (!test_check_module(&im, &check_file_test_def_3, foo)) {
    DBG("check_file_test_def_3 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_1...\n");
  if (!test_check_module(&im, &check_file_test_lambda_1, foo)) {
    DBG("check_file_test_lambda_1 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_4...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_4, foo)) {
    DBG("check_file_test_lambda_4 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_5...\n");
  if (!test_check_module(&im, &check_file_test_lambda_5, foo)) {
    DBG("check_file_test_lambda_5 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_6...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_6, foo)) {
    DBG("check_file_test_lambda_6 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_7...\n");
  if (!test_check_module(&im, &check_file_test_lambda_7, foo)) {
    DBG("check_file_test_lambda_7 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_8...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_8, foo)) {
    DBG("check_file_test_lambda_8 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_9...\n");
  if (!test_check_module(&im, &check_file_test_lambda_9, foo)) {
    DBG("check_file_test_lambda_9 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_10...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_10, foo)) {
    DBG("check_file_test_lambda_10 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_11...\n");
  if (!test_check_module(&im, &check_file_test_lambda_11, foo)) {
    DBG("check_file_test_lambda_11 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_12...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_12, foo)) {
    DBG("check_file_test_lambda_12 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_13...\n");
  if (!test_check_module(&im, &check_file_test_lambda_13, foo)) {
    DBG("check_file_test_lambda_13 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_14...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_14, foo)) {
    DBG("check_file_test_lambda_14 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_15...\n");
  if (!test_check_module(&im, &check_file_test_lambda_15, foo)) {
    DBG("check_file_test_lambda_15 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_16...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_16, foo)) {
    DBG("check_file_test_lambda_16 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_17...\n");
  if (!test_check_module(&im, &check_file_test_lambda_17, foo)) {
    DBG("check_file_test_lambda_17 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_18...\n");
  if (!test_check_module(&im, &check_file_test_lambda_18, foo)) {
    DBG("check_file_test_lambda_18 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_19...\n");
  if (!test_check_module(&im, &check_file_test_lambda_19, foo)) {
    DBG("check_file_test_lambda_19 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_20...\n");
  if (!test_check_module(&im, &check_file_test_lambda_20, foo)) {
    DBG("check_file_test_lambda_20 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_21...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_21, foo)) {
    DBG("check_file_test_lambda_21 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_22...\n");
  if (!test_check_module(&im, &check_file_test_lambda_22, foo)) {
    DBG("check_file_test_lambda_22 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_23...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_23, foo)) {
    DBG("check_file_test_lambda_23 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_24...\n");
  if (!test_check_module(&im, &check_file_test_lambda_24, foo)) {
    DBG("check_file_test_lambda_24 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_25...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_25, foo)) {
    DBG("check_file_test_lambda_25 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_26...\n");
  if (!test_check_module(&im, &check_file_test_lambda_26, foo)) {
    DBG("check_file_test_lambda_26 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_27...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_27, foo)) {
    DBG("check_file_test_lambda_27 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_lambda_28...\n");
  if (!test_check_module(&im, &check_file_test_lambda_28, foo)) {
    DBG("check_file_test_lambda_28 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_29...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_29, foo)) {
    DBG("check_file_test_lambda_29 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_extern_1...\n");
  if (!test_check_module(&im, &check_file_test_extern_1, foo)) {
    DBG("check_file_test_extern_1 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_extern_2...\n");
  if (!!test_check_module(&im, &check_file_test_extern_2, foo)) {
    DBG("check_file_test_extern_2 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_extern_3...\n");
  if (!!test_check_module(&im, &check_file_test_extern_3, foo)) {
    DBG("check_file_test_extern_3 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_1...\n");
  if (!test_check_module(&im, &check_file_test_more_1, foo)) {
    DBG("check_file_test_more_1 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_2...\n");
  if (!test_check_module(&im, &check_file_test_more_2, foo)) {
    DBG("check_file_test_more_2 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_3...\n");
  if (!!test_check_module(&im, &check_file_test_more_3, foo)) {
    DBG("check_file_test_more_3 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_4...\n");
  if (!!test_check_module(&im, &check_file_test_more_4, foo)) {
    DBG("check_file_test_more_4 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_5...\n");
  if (!test_check_module(&im, &check_file_test_more_5, foo)) {
    DBG("check_file_test_more_5 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_6...\n");
  if (!!test_check_module(&im, &check_file_test_more_6, foo)) {
    DBG("check_file_test_more_6 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_7...\n");
  if (!test_check_module(&im, &check_file_test_more_7, foo)) {
    DBG("check_file_test_more_7 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_8...\n");
  if (!test_check_module(&im, &check_file_test_more_8, foo)) {
    DBG("check_file_test_more_8 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_9...\n");
  if (!!test_check_module(&im, &check_file_test_more_9, foo)) {
    DBG("check_file_test_more_9 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_10...\n");
  if (!test_check_module(&im, &check_file_test_more_10, foo)) {
    DBG("check_file_test_more_10 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_11...\n");
  if (!test_check_module(&im, &check_file_test_more_11, foo)) {
    DBG("check_file_test_more_11 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_12...\n");
  if (!!test_check_module(&im, &check_file_test_more_12, foo)) {
    DBG("check_file_test_more_12 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_13...\n");
  if (!!test_check_module(&im, &check_file_test_more_13, foo)) {
    DBG("check_file_test_more_13 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_14...\n");
  if (!test_check_module(&im, &check_file_test_more_14, foo)) {
    DBG("check_file_test_more_14 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_15...\n");
  if (!test_check_module(&im, &check_file_test_more_15, foo)) {
    DBG("check_file_test_more_15 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_16...\n");
  if (!test_check_module(&im, &check_file_test_more_16, foo)) {
    DBG("check_file_test_more_16 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_17...\n");
  if (!!test_check_module(&im, &check_file_test_more_17, foo)) {
    DBG("check_file_test_more_17 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_18...\n");
  if (!test_check_module(&im, &check_file_test_more_18, foo)) {
    DBG("check_file_test_more_18 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_19...\n");
  if (!!test_check_module(&im, &check_file_test_more_19, foo)) {
    DBG("check_file_test_more_19 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_20...\n");
  if (!test_check_module(&im, &check_file_test_more_20, foo)) {
    DBG("check_file_test_more_20 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_21...\n");
  if (!test_check_module(&im, &check_file_test_more_21, foo)) {
    DBG("check_file_test_more_21 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_22...\n");
  if (!!test_check_module(&im, &check_file_test_more_22, foo)) {
    DBG("check_file_test_more_22 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_23...\n");
  if (!!test_check_module(&im, &check_file_test_more_23, foo)) {
    DBG("check_file_test_more_23 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_24...\n");
  if (!!test_check_module(&im, &check_file_test_more_24, foo)) {
    DBG("check_file_test_more_24 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_25...\n");
  if (!test_check_module(&im, &check_file_test_more_25, foo)) {
    DBG("check_file_test_more_25 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_26...\n");
  if (!test_check_module(&im, &check_file_test_more_26, foo)) {
    DBG("check_file_test_more_26 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_27...\n");
  if (!!test_check_module(&im, &check_file_test_more_27, foo)) {
    DBG("check_file_test_more_27 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_28...\n");
  if (!test_check_module(&im, &check_file_test_more_28, foo)) {
    DBG("check_file_test_more_28 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_29...\n");
  if (!!test_check_module(&im, &check_file_test_more_29, foo)) {
    DBG("check_file_test_more_29 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_30a...\n");
  if (!!test_check_module(&im, &check_file_test_more_30a, foo)) {
    DBG("check_file_test_more_30a fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_30b...\n");
  if (!test_check_module(&im, &check_file_test_more_30b, foo)) {
    DBG("check_file_test_more_30b fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_31...\n");
  if (!!test_check_module(&im, &check_file_test_more_31, foo)) {
    DBG("check_file_test_more_31 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_32a...\n");
  if (!!test_check_module(&im, &check_file_test_more_32a, foo)) {
    DBG("check_file_test_more_32a fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_32b...\n");
  if (!test_check_module(&im, &check_file_test_more_32b, foo)) {
    DBG("check_file_test_more_32b fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_33...\n");
  if (!!test_check_module(&im, &check_file_test_more_33, foo)) {
    DBG("check_file_test_more_33 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_34...\n");
  if (!test_check_module(&im, &check_file_test_more_34, foo)) {
    DBG("check_file_test_more_34 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_35...\n");
  if (!!test_check_module(&im, &check_file_test_more_35, foo)) {
    DBG("check_file_test_more_35 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_36...\n");
  if (!test_check_module(&im, &check_file_test_more_36, foo)) {
    DBG("check_file_test_more_36 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_37...\n");
  if (!test_check_module(&im, &check_file_test_more_37, foo)) {
    DBG("check_file_test_more_37 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_38...\n");
  if (!!test_check_module(&im, &check_file_test_more_38, foo)) {
    DBG("check_file_test_more_38 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_39...\n");
  if (!!test_check_module(&im, &check_file_test_more_39, foo)) {
    DBG("check_file_test_more_39 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_40...\n");
  if (!test_check_module(&im, &check_file_test_more_40, foo)) {
    DBG("check_file_test_more_40 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_41...\n");
  if (!!test_check_module(&im, &check_file_test_more_41, foo)) {
    DBG("check_file_test_more_41 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_42...\n");
  if (!test_check_module(&im, &check_file_test_more_42, foo)) {
    DBG("check_file_test_more_42 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_43...\n");
  if (!!test_check_module(&im, &check_file_test_more_43, foo)) {
    DBG("check_file_test_more_43 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_44...\n");
  if (!!test_check_module(&im, &check_file_test_more_44, foo)) {
    DBG("check_file_test_more_44 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_45...\n");
  if (!!test_check_module(&im, &check_file_test_more_45, foo)) {
    DBG("check_file_test_more_45 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_46...\n");
  if (!test_check_module(&im, &check_file_test_more_46, foo)) {
    DBG("check_file_test_more_46 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_47...\n");
  if (!!test_check_module(&im, &check_file_test_more_47, foo)) {
    DBG("check_file_test_more_47 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_48...\n");
  if (!test_check_module(&im, &check_file_test_more_48, foo)) {
    DBG("check_file_test_more_48 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_49...\n");
  if (!!test_check_module(&im, &check_file_test_more_49, foo)) {
    DBG("check_file_test_more_49 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_50...\n");
  if (!test_check_module(&im, &check_file_test_more_50, foo)) {
    DBG("check_file_test_more_50 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_51...\n");
  if (!!test_check_module(&im, &check_file_test_more_51, foo)) {
    DBG("check_file_test_more_51 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_52...\n");
  if (!test_check_module(&im, &check_file_test_more_52, foo)) {
    DBG("check_file_test_more_52 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_53...\n");
  if (!test_check_module(&im, &check_file_test_more_53, foo)) {
    DBG("check_file_test_more_53 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_54...\n");
  if (!test_check_module(&im, &check_file_test_more_54, foo)) {
    DBG("check_file_test_more_54 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_55...\n");
  if (!test_check_module(&im, &check_file_test_more_55, foo)) {
    DBG("check_file_test_more_55 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_56...\n");
  if (!test_check_module(&im, &check_file_test_more_56, foo)) {
    DBG("check_file_test_more_56 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_57...\n");
  if (!test_check_module(&im, &check_file_test_more_57, foo)) {
    DBG("check_file_test_more_57 fails\n");
    goto cleanup_identmap;
  }

  ret = 1;
 cleanup_identmap:
  identmap_destroy(&im);
  return ret;
}
