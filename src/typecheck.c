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

const int MAX_TEMPLATE_INSTANTIATION_RECURSION_DEPTH = 50;

uint32_t numeric_type_size(enum numeric_type t) {
  switch (t) {
  case NUMERIC_TYPE_U8: return 1;
  case NUMERIC_TYPE_I8: return 1;
  case NUMERIC_TYPE_U16: return 2;
  case NUMERIC_TYPE_I16: return 2;
  case NUMERIC_TYPE_I32: return 4;
  case NUMERIC_TYPE_U32: return 4;
  default: UNREACHABLE();
  }
}

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
  int res = name_table_add_primitive_type(&cs->nt, ident,
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

static const enum primitive_op binop_i32_primitive_ops[] = {
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

static const enum primitive_op binop_u32_primitive_ops[] = {
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

static const enum primitive_op binop_u8_primitive_ops[] = {
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

static const enum primitive_op binop_i8_primitive_ops[] = {
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

static const enum primitive_op binop_u16_primitive_ops[] = {
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

static const enum primitive_op binop_i16_primitive_ops[] = {
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




void intern_binop(struct checkstate *cs,
                  enum ast_binop binop,
                  const enum primitive_op *primop_array,
                  struct ast_generics *generics,
                  struct ast_typeexpr *type) {

  name_table_add_primitive_def(
      &cs->nt,
      identmap_intern_c_str(cs->im, binop_name(binop)),
      primop_array[binop],
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
  intern_primitive_type(cs, U8_TYPE_NAME, NULL, 0, 1, 1);
  intern_primitive_type(cs, I8_TYPE_NAME, NULL, 0, 1, 1);
  intern_primitive_type(cs, U16_TYPE_NAME, NULL, 0, 2, 2);
  intern_primitive_type(cs, I16_TYPE_NAME, NULL, 0, 2, 2);
  intern_primitive_type(cs, U32_TYPE_NAME, NULL, 0, 4, 4);
  intern_primitive_type(cs, I32_TYPE_NAME, NULL, 0, 4, 4);

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
  ident_value bool_name = identmap_intern_c_str(im, BOOLEAN_STANDIN_TYPE_NAME);
  ident_value names[3];
  names[0] = names[1] = name;
  names[2] = bool_name;
  init_func_type(a, im, names, 3);
}

void import_integer_binops(struct checkstate *cs,
                           const enum primitive_op *primop_array,
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

void import_integer_conversions(struct checkstate *cs) {
  ident_value types[6];
  types[0] = identmap_intern_c_str(cs->im, U8_TYPE_NAME);
  types[1] = identmap_intern_c_str(cs->im, I8_TYPE_NAME);
  types[2] = identmap_intern_c_str(cs->im, U16_TYPE_NAME);
  types[3] = identmap_intern_c_str(cs->im, I16_TYPE_NAME);
  types[4] = identmap_intern_c_str(cs->im, U32_TYPE_NAME);
  types[5] = identmap_intern_c_str(cs->im, I32_TYPE_NAME);

  ident_value convert = identmap_intern_c_str(cs->im, CONVERT_FUNCTION_NAME);

  struct ast_generics generics;
  ast_generics_init_no_params(&generics);

  enum primitive_op conversions[6][6] = {
    {
      PRIMITIVE_OP_CONVERT_U8_TO_U8,
      PRIMITIVE_OP_CONVERT_U8_TO_I8,
      PRIMITIVE_OP_CONVERT_U8_TO_U16,
      PRIMITIVE_OP_CONVERT_U8_TO_I16,
      PRIMITIVE_OP_CONVERT_U8_TO_U32,
      PRIMITIVE_OP_CONVERT_U8_TO_I32,
    }, {
      PRIMITIVE_OP_CONVERT_I8_TO_U8,
      PRIMITIVE_OP_CONVERT_I8_TO_I8,
      PRIMITIVE_OP_CONVERT_I8_TO_U16,
      PRIMITIVE_OP_CONVERT_I8_TO_I16,
      PRIMITIVE_OP_CONVERT_I8_TO_U32,
      PRIMITIVE_OP_CONVERT_I8_TO_I32,
    }, {
      PRIMITIVE_OP_CONVERT_U16_TO_U8,
      PRIMITIVE_OP_CONVERT_U16_TO_I8,
      PRIMITIVE_OP_CONVERT_U16_TO_U16,
      PRIMITIVE_OP_CONVERT_U16_TO_I16,
      PRIMITIVE_OP_CONVERT_U16_TO_U32,
      PRIMITIVE_OP_CONVERT_U16_TO_I32,
    }, {
      PRIMITIVE_OP_CONVERT_I16_TO_U8,
      PRIMITIVE_OP_CONVERT_I16_TO_I8,
      PRIMITIVE_OP_CONVERT_I16_TO_U16,
      PRIMITIVE_OP_CONVERT_I16_TO_I16,
      PRIMITIVE_OP_CONVERT_I16_TO_U32,
      PRIMITIVE_OP_CONVERT_I16_TO_I32,
    }, {
      PRIMITIVE_OP_CONVERT_U32_TO_U8,
      PRIMITIVE_OP_CONVERT_U32_TO_I8,
      PRIMITIVE_OP_CONVERT_U32_TO_U16,
      PRIMITIVE_OP_CONVERT_U32_TO_I16,
      PRIMITIVE_OP_CONVERT_U32_TO_U32,
      PRIMITIVE_OP_CONVERT_U32_TO_I32,
    }, {
      PRIMITIVE_OP_CONVERT_I32_TO_U8,
      PRIMITIVE_OP_CONVERT_I32_TO_I8,
      PRIMITIVE_OP_CONVERT_I32_TO_U16,
      PRIMITIVE_OP_CONVERT_I32_TO_I16,
      PRIMITIVE_OP_CONVERT_I32_TO_U32,
      PRIMITIVE_OP_CONVERT_I32_TO_I32,
    },
  };

  for (size_t i = 0; i < 6; i++) {
    for (size_t j = 0; j < 6; j++) {
      struct ast_typeexpr func_type;
      ident_value names[2];
      names[0] = types[i];
      names[1] = types[j];
      init_func_type(&func_type, cs->im, names, 2);
      name_table_add_primitive_def(&cs->nt,
                                   convert,
                                   conversions[i][j],
                                   &generics,
                                   &func_type);
      ast_typeexpr_destroy(&func_type);
    }
  }
}

void import_unop(struct checkstate *cs,
                 enum primitive_op primitive_op,
                 const char *op_name,
                 const char *type_name) {
  struct ast_generics generics;
  ast_generics_init_no_params(&generics);

  struct ast_typeexpr type;
  ident_value args[2];
  args[0] = args[1] = identmap_intern_c_str(cs->im, type_name);
  init_func_type(&type, cs->im, args, 2);
  name_table_add_primitive_def(
      &cs->nt,
      identmap_intern_c_str(cs->im, op_name),
      primitive_op,
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
  init_name_type(&type, identmap_intern_c_str(cs->im, SIZE_STANDIN_TYPE_NAME));

  name_table_add_primitive_def(
      &cs->nt,
      identmap_intern_c_str(cs->im, "sizeof"),
      PRIMITIVE_OP_SIZEOF,
      &generics,
      &type);

  name_table_add_primitive_def(
      &cs->nt,
      identmap_intern_c_str(cs->im, "alignof"),
      PRIMITIVE_OP_ALIGNOF,
      &generics,
      &type);

  ast_typeexpr_destroy(&type);
  ast_generics_destroy(&generics);
}

void checkstate_import_primitive_defs(struct checkstate *cs) {
  import_integer_binops(cs, binop_i32_primitive_ops, I32_TYPE_NAME);
  import_integer_binops(cs, binop_u32_primitive_ops, U32_TYPE_NAME);
  import_integer_binops(cs, binop_u8_primitive_ops, U8_TYPE_NAME);
  import_integer_binops(cs, binop_i8_primitive_ops, I8_TYPE_NAME);
  import_integer_binops(cs, binop_u16_primitive_ops, U16_TYPE_NAME);
  import_integer_binops(cs, binop_i16_primitive_ops, I16_TYPE_NAME);

  import_integer_conversions(cs);

  {
    /* TODO: String values duplicated with parse code, I guess. */
    import_unop(cs, PRIMITIVE_OP_NEGATE_I8, "-", I8_TYPE_NAME);
    import_unop(cs, PRIMITIVE_OP_NEGATE_I16, "-", I16_TYPE_NAME);
    import_unop(cs, PRIMITIVE_OP_NEGATE_I32, "-", I32_TYPE_NAME);

    import_unop(cs, PRIMITIVE_OP_LOGICAL_NOT, "!", BOOLEAN_STANDIN_TYPE_NAME);

    import_unop(cs, PRIMITIVE_OP_BIT_NOT_I8, "^", I8_TYPE_NAME);
    import_unop(cs, PRIMITIVE_OP_BIT_NOT_U8, "^", U8_TYPE_NAME);
    import_unop(cs, PRIMITIVE_OP_BIT_NOT_I16, "^", I16_TYPE_NAME);
    import_unop(cs, PRIMITIVE_OP_BIT_NOT_U16, "^", U16_TYPE_NAME);
    import_unop(cs, PRIMITIVE_OP_BIT_NOT_I32, "^", I32_TYPE_NAME);
    import_unop(cs, PRIMITIVE_OP_BIT_NOT_U32, "^", U32_TYPE_NAME);
  }

  import_sizeof_alignof(cs);
}

void checkstate_import_primitives(struct checkstate *cs) {
  checkstate_import_primitive_types(cs);
  checkstate_import_primitive_defs(cs);
}

void init_boolean_typeexpr(struct checkstate *cs, struct ast_typeexpr *a) {
  a->tag = AST_TYPEEXPR_NAME;
  a->u.name = make_ast_ident(identmap_intern_c_str(cs->im, BOOLEAN_STANDIN_TYPE_NAME));
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

int add_conversion_operator(struct checkstate *cs,
                            struct defclass_ident *private_to,
                            size_t private_to_count,
                            struct ast_generics *generics,
                            struct ast_typeexpr *arg_type,
                            struct ast_typeexpr *return_type) {
  /* This is actually a magical thing, not a function.  It gets
  treated specially by build.c Maybe we should implement this
  differently. */

  struct ast_typeexpr func_type;
  copy_make_unary_func_type(cs, arg_type, return_type, &func_type);

  int ret = name_table_add_private_primitive_def(
      &cs->nt,
      identmap_intern_c_str(cs->im, "~"),
      PRIMITIVE_OP_REINTERPRET,
      generics,
      &func_type,
      private_to,
      private_to_count);

  ast_typeexpr_destroy(&func_type);
  return ret;
}

/* TODO: pass im, not cs. */
int add_direct_and_ptr_conversion_operator(
    struct checkstate *cs,
    struct defclass_ident *private_to,
    size_t private_to_count,
    struct ast_generics *generics,
    struct ast_typeexpr *arg_type,
    struct ast_typeexpr *return_type) {
  if (!add_conversion_operator(cs, private_to, private_to_count, generics,
                               arg_type, return_type)) {
    return 0;
  }

  struct ast_typeexpr ptr_arg;
  wrap_in_ptr(cs->im, arg_type, &ptr_arg);
  struct ast_typeexpr ptr_return;
  wrap_in_ptr(cs->im, return_type, &ptr_return);
  if (!add_conversion_operator(cs, private_to, private_to_count, generics,
                               &ptr_arg, &ptr_return)) {
    return 0;
  }
  ast_typeexpr_destroy(&ptr_return);
  ast_typeexpr_destroy(&ptr_arg);
  return 1;
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
      if (!name_table_add_def(&cs->nt,
                              toplevel->u.def.name_.value,
                              &toplevel->u.def.generics_,
                              &toplevel->u.def.type_,
                              ics->accessible,
                              ics->accessible_count,
                              toplevel->u.def.is_export,
                              &toplevel->u.def)) {
        return 0;
      }
    } break;
    case AST_TOPLEVEL_EXTERN_DEF: {
      if (!name_table_add_extern_def(&cs->nt,
                                     toplevel->u.extern_def.name.value,
                                     &toplevel->u.extern_def.type)) {
        return 0;
      }
    } break;
    case AST_TOPLEVEL_DEFTYPE: {
      struct ast_deftype *dt = &toplevel->u.deftype;
      struct generics_arity arity = params_arity(&dt->generics);
      if (!name_table_add_deftype(&cs->nt, dt->name.value, arity, dt)) {
        return 0;
      }

      struct ast_typeexpr name_type;
      init_generics_type(&name_type, &dt->name, &dt->generics);

      struct defclass_ident *private_to = NULL;
      size_t private_to_count = 0;
      if (dt->disposition != AST_DEFTYPE_NOT_CLASS) {
        private_to = malloc_mul(sizeof(*private_to), 1);
        private_to_count = 1;
        private_to[0].name = dt->name.value;
        private_to[0].arity = arity;
      }

      int ret = 0;
      if (!add_direct_and_ptr_conversion_operator(cs, private_to, private_to_count,
                                                  &toplevel->u.deftype.generics,
                                                  &name_type, &toplevel->u.deftype.type)) {
        goto deftype_fail;
      }
      if (!add_direct_and_ptr_conversion_operator(cs, private_to, private_to_count,
                                                  &toplevel->u.deftype.generics,
                                                  &toplevel->u.deftype.type, &name_type)) {
        goto deftype_fail;
      }

      ret = 1;
    deftype_fail:
      free(private_to);
      ast_typeexpr_destroy(&name_type);
      if (ret == 0) {
        return 0;
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

    chase_through_toplevels(cs, &ics, heap_file->toplevels, heap_file->toplevels_count);

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
      QMETERR(a->meta, "Unrecognized type name %.*s.\n", IM_P(cs->im, name));
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
    QMETERR(a->meta, "Type lookup fail for %.*s, arity %"PRIz"\n",
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
        QMETERR(field->meta, "struct/union fields have duplicate name %.*s\n",
                IM_P(cs->im, field->name.value));
        return 0;
      }
    }

    {
      size_t which_generic;
      if (generics_lookup_name(generics, field->name.value, &which_generic)) {
        QMETERR(field->meta,
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

enum typeexpr_trait min_typeexpr_trait(enum typeexpr_trait a, enum typeexpr_trait b) {
  return a < b ? a : b;
}

/* TODO: This should really return a 3-way enum.  It's icky that the
caller assumes it only needs to check the destructor on
lookup_copy. */
void deftype_trait_strategies(enum ast_deftype_disposition disposition,
                              int *lookup_move_out,
                              int *lookup_copy_out) {
  switch (disposition) {
  case AST_DEFTYPE_NOT_CLASS:
  case AST_DEFTYPE_CLASS_DEFAULT_COPY_MOVE_DESTROY:
    *lookup_move_out = 0;
    *lookup_copy_out = 0;
    break;
  case AST_DEFTYPE_CLASS_DEFAULT_MOVE:
    *lookup_move_out = 0;
    *lookup_copy_out = 1;
    break;
  case AST_DEFTYPE_CLASS_NO_DEFAULTS:
    *lookup_move_out = 1;
    *lookup_copy_out = 1;
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
  params[count].tag = AST_TYPEEXPR_UNKNOWN;

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
                                 int *result_out) {
  struct ast_typeexpr func_type;
  make_pointee_func_lookup_type(cs, a, argdupes, &func_type);

  struct ast_ident func_name = make_ast_ident(identmap_intern_c_str(cs->im, name));

  /* TODO: Where do the copy constructors and the like (for a specific instantiation) get type-checked? */
  size_t matching_defs = name_table_count_matching_defs(&cs->nt,
                                                        &func_name,
                                                        NULL,
                                                        0,
                                                        &func_type);

  ast_ident_destroy(&func_name);
  ast_typeexpr_destroy(&func_type);

  if (matching_defs < 2) {
    *result_out = (matching_defs == 1);
    return 1;
  } else {
    METERR(*ast_typeexpr_meta(a), "Multiple matching %s definitions\n", name);
    return 0;
  }
}

int has_explicit_copy(struct checkstate *cs, struct ast_typeexpr *a, int *result_out) {
  return has_explicit_movecopydestroy(cs, a, 2, "copy", result_out);
}

int has_explicit_destroy(struct checkstate *cs, struct ast_typeexpr *a, int *result_out) {
  return has_explicit_movecopydestroy(cs, a, 1, "destroy", result_out);
}

int has_explicit_move(struct checkstate *cs, struct ast_typeexpr *a, int *result_out) {
  return has_explicit_movecopydestroy(cs, a, 2, "move", result_out);
}

int check_typeexpr_traits(struct checkstate *cs,
                          /* a is a concrete type. */
                          struct ast_typeexpr *a,
                          struct typeexpr_traits *out);

int finish_checking_name_traits(struct checkstate *cs,
                                /* The original concrete name/app type we are checking traits for. */
                                struct ast_typeexpr *a,
                                struct ast_meta *deftype_meta,
                                enum ast_deftype_disposition deftype_disposition,
                                struct ast_typeexpr *concrete_deftype_type,
                                struct typeexpr_traits *out) {
  int lookup_move;
  int lookup_copy;
  deftype_trait_strategies(deftype_disposition, &lookup_move, &lookup_copy);

  struct typeexpr_traits rhs_traits = { 0 };
  if (!(lookup_move && lookup_copy)) {
    if (!check_typeexpr_traits(cs, concrete_deftype_type, &rhs_traits)) {
      return 0;
    }
  }

  int explicit_move;
  if (!has_explicit_move(cs, a, &explicit_move)) {
    return 0;
  }

  int explicit_copy;
  if (!has_explicit_copy(cs, a, &explicit_copy)) {
    return 0;
  }

  int explicit_destroy;
  if (!has_explicit_destroy(cs, a, &explicit_destroy)) {
    return 0;
  }

  if (lookup_move) {
    rhs_traits.movable = explicit_move ? TRAIT_HAD : TRAIT_LACKED;
  } else {
    if (explicit_move) {
      METERR(*deftype_meta, "Type has both implicit and explicit move.%s", "\n");
      return 0;
    }
  }

  if (lookup_copy) {
    rhs_traits.copyable = explicit_copy ? TRAIT_HAD : TRAIT_LACKED;
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

  *out = rhs_traits;
  return 1;
}

int check_typeexpr_name_traits(struct checkstate *cs,
                               struct ast_typeexpr *a,
                               struct typeexpr_traits *out) {
  struct deftype_entry *ent;
  if (!name_table_lookup_deftype(&cs->nt, a->u.name.value,
                                 no_param_list_arity(),
                                 &ent)) {
    CRASH("an invalid type");
  }

  if (ent->is_primitive) {
    out->movable = TRAIT_TRIVIALLY_HAD;
    out->copyable = TRAIT_TRIVIALLY_HAD;
    return 1;
  }

  return finish_checking_name_traits(cs,
                                     a,
                                     &ent->deftype->meta,
                                     ent->deftype->disposition,
                                     &ent->deftype->type,
                                     out);
}

int check_typeexpr_app_traits(struct checkstate *cs,
                              struct ast_typeexpr *a,
                              struct typeexpr_traits *out) {
  struct deftype_entry *ent;
  if (!name_table_lookup_deftype(&cs->nt, a->u.app.name.value,
                                 param_list_arity(a->u.app.params_count),
                                 &ent)) {
    CRASH("an invalid generic type");
  }

  if (ent->is_primitive) {
    out->movable = TRAIT_TRIVIALLY_HAD;
    out->copyable = TRAIT_TRIVIALLY_HAD;
    return 1;
  }

  struct ast_deftype *deftype = ent->deftype;

  CHECK(deftype->generics.has_type_params
        && deftype->generics.params_count == a->u.app.params_count);

  struct ast_typeexpr concrete_deftype_type;
  do_replace_generics(&deftype->generics,
                      a->u.app.params,
                      &deftype->type,
                      &concrete_deftype_type);

  int res = finish_checking_name_traits(cs,
                                        a,
                                        &deftype->meta,
                                        deftype->disposition,
                                        &concrete_deftype_type,
                                        out);
  ast_typeexpr_destroy(&concrete_deftype_type);
  return res;
}

int check_typeexpr_structe_traits(struct checkstate *cs,
                                  struct ast_typeexpr *a,
                                  struct typeexpr_traits *out) {
  struct typeexpr_traits combined;
  combined.movable = TRAIT_TRIVIALLY_HAD;
  combined.copyable = TRAIT_TRIVIALLY_HAD;
  for (size_t i = 0, e = a->u.structe.fields_count; i < e; i++) {
    struct typeexpr_traits traits;
    if (!check_typeexpr_traits(cs, &a->u.structe.fields[i].type, &traits)) {
      return 0;
    }

    combined.movable = min_typeexpr_trait(combined.movable, traits.movable);
    combined.copyable = min_typeexpr_trait(combined.copyable, traits.copyable);
  }

  *out = combined;
  return 1;
}

int check_typeexpr_unione_traits(struct checkstate *cs,
                                 struct ast_typeexpr *a,
                                 struct typeexpr_traits *out) {
  for (size_t i = 0, e = a->u.unione.fields_count; i < e; i++) {
    struct typeexpr_traits traits;
    if (!check_typeexpr_traits(cs, &a->u.unione.fields[i].type, &traits)) {
      return 0;
    }

    if (traits.movable != TRAIT_TRIVIALLY_HAD) {
      METERR(a->u.unione.meta, "Union field %.*s is not trivially movable.\n",
             IM_P(cs->im, a->u.unione.fields[i].name.value));
      return 0;
    }

    if (traits.copyable != TRAIT_TRIVIALLY_HAD) {
      METERR(a->u.unione.meta, "Union field %.*s is not trivially copyable.\n",
             IM_P(cs->im, a->u.unione.fields[i].name.value));
      return 0;
    }
  }

  out->movable = TRAIT_TRIVIALLY_HAD;
  out->copyable = TRAIT_TRIVIALLY_HAD;
  return 1;
}


/* Returns false if the type is invalid, e.g. a union with fields that
are not trivially copyable and movable, or a class type that needs a
defined destructor, or a type that has multiply defined
move/copy/destroy operations. */
/* TODO: It's possible a templated copy/move/destroy operator won't
get typechecked -- we need some way to force that, someplace where
there is an exprscope. */
int check_typeexpr_traits(struct checkstate *cs,
                          /* a is a concrete type. */
                          struct ast_typeexpr *a,
                          struct typeexpr_traits *out) {
  switch (a->tag) {
  case AST_TYPEEXPR_NAME:
    return check_typeexpr_name_traits(cs, a, out);
  case AST_TYPEEXPR_APP:
    return check_typeexpr_app_traits(cs, a, out);
  case AST_TYPEEXPR_STRUCTE:
    return check_typeexpr_structe_traits(cs, a, out);
  case AST_TYPEEXPR_UNIONE:
    return check_typeexpr_unione_traits(cs, a, out);
  case AST_TYPEEXPR_ARRAY:
    return check_typeexpr_traits(cs, a->u.arraytype.param, out);
  default:
    UNREACHABLE();
  }
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
        QMETERR(a->meta, "duplicate param names %.*s within same generics list.\n",
                IM_P(cs->im, name));
        return 0;
      }
    }

    if (name_table_shadowed(&cs->nt, name)) {
      QMETERR(a->meta, "generics list shadows global name %.*s.\n",
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

  if (!check_typeexpr(cs, &a->generics, &a->type, ent)) {
    return 0;
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
  /* TODO: We could probably have a stack of varnums that are in scope instead. */
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

  if (name_table_shadowed(&es->cs->nt, name->value)) {
    METERR(name->meta, "Variable name %.*s shadows a global def or type.\n",
           IM_P(es->cs->im, name->value));
    return 0;
  }

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

int check_expr_with_type(struct exprscope *es,
                         struct ast_expr *x,
                         struct ast_typeexpr *type,
                         struct ast_expr *annotated_out);

int lookup_global_maybe_typecheck(struct exprscope *es,
                                  struct ast_name_expr *name,
                                  struct ast_typeexpr *partial_type,
                                  struct ast_typeexpr *out,
                                  int *is_lvalue_out,
                                  struct def_instantiation **inst_out) {
  struct ast_typeexpr unified;
  struct def_entry *ent;
  struct def_instantiation *inst;
  if (!name_table_match_def(&es->cs->nt,
                            &name->ident,
                            name->has_params ? name->params : NULL,
                            name->has_params ? name->params_count : 0,
                            partial_type,
                            &unified,
                            &ent,
                            &inst)) {
    return 0;
  }

  for (size_t i = 0, e = ent->private_to_count; i < e; i++) {
    if (!is_accessible(es, ent->private_to[i])) {
      METERR(name->meta, "Access denied.%s", "\n");
      goto fail_unified;
    }
  }

  exprscope_note_static_reference(es, ent);

  if (!ent->is_primitive && !ent->is_extern
      && ent->generics.has_type_params && !inst->typecheck_started) {
    CHECK(ent->def);
    CHECK(!inst->annotated_rhs_computed);
    if (es->cs->template_instantiation_recursion_depth
        == MAX_TEMPLATE_INSTANTIATION_RECURSION_DEPTH) {
      METERR(ent->def->meta, "Max template instantiation recursion depth exceeded.%s", "\n");
      goto fail_unified;
    }

    es->cs->template_instantiation_recursion_depth++;

    inst->typecheck_started = 1;
    struct exprscope scope;
    exprscope_init(&scope, es->cs,
                   &ent->def->generics_,
                   inst->substitutions,
                   inst->substitutions_count,
                   ent->accessible,
                   ent->accessible_count,
                   STATIC_COMPUTATION_YES,
                   ent);

    struct ast_expr annotated_rhs;
    if (!check_expr_with_type(&scope, &ent->def->rhs_, &unified,
                              &annotated_rhs)) {
      exprscope_destroy(&scope);
      es->cs->template_instantiation_recursion_depth--;
      goto fail_unified;
    }
    CHECK(!inst->annotated_rhs_computed);
    inst->annotated_rhs_computed = 1;
    inst->annotated_rhs = annotated_rhs;

    exprscope_destroy(&scope);
    es->cs->template_instantiation_recursion_depth--;
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
  return lookup_global_maybe_typecheck(es, name, partial_type,
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
                                   struct ast_vardecl *fields,
                                   size_t fields_count,
                                   struct ast_vardecl **fields_out,
                                   size_t *fields_count_out) {
  struct ast_vardecl *f = malloc_mul(sizeof(*f), fields_count);
  for (size_t i = 0; i < fields_count; i++) {
    struct ast_ident name;
    ast_ident_init_copy(&name, &fields[i].name);
    struct ast_typeexpr type;
    do_replace_generics(generics, generics_substitutions, &fields[i].type,
                        &type);
    ast_vardecl_init(&f[i], ast_meta_make_garbage(),
                     name, type);
  }
  *fields_out = f;
  *fields_count_out = fields_count;
}

void do_replace_generics(struct ast_generics *generics,
                         struct ast_typeexpr *generics_substitutions,
                         struct ast_typeexpr *a,
                         struct ast_typeexpr *out) {
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
                                  a->u.unione.fields, a->u.unione.fields_count,
                                  &fields, &fields_count);
    out->tag = AST_TYPEEXPR_UNIONE;
    ast_unione_init(&out->u.unione, ast_meta_make_copy(&a->u.unione.meta),
                    fields, fields_count);
  } break;
  case AST_TYPEEXPR_ARRAY: {
    struct ast_typeexpr param;
    do_replace_generics(generics, generics_substitutions, a->u.arraytype.param,
                        &param);
    out->tag = AST_TYPEEXPR_ARRAY;
    ast_arraytype_init(&out->u.arraytype, ast_meta_make_copy(&a->u.arraytype.meta),
                       a->u.arraytype.count, param);
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
    CHECK(es->generics->params_count == es->generics_substitutions_count);
    do_replace_generics(es->generics, es->generics_substitutions, a, out);
  }
}

struct ast_expr_info expr_info_typechecked_subobject(
    struct ast_typeexpr concrete_type,
    struct ast_expr_info *info) {
  CHECK(info->is_typechecked);
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

int check_expr(struct exprscope *es,
               struct ast_expr *x,
               struct ast_typeexpr *partial_type,
               struct ast_expr *annotated_out);

int check_expr_funcall(struct exprscope *es,
                       struct ast_funcall *x,
                       struct ast_typeexpr *partial_type,
                       struct ast_expr *annotated_out) {
  size_t args_count = x->args_count;
  size_t args_types_count = size_add(args_count, 1);
  struct ast_typeexpr *args_types = malloc_mul(sizeof(*args_types),
                                               args_types_count);
  struct ast_expr *args_annotated = malloc_mul(sizeof(*args_annotated),
                                               args_count);
  /* Used for the case where args_count == 1 and we have
  PRIMITIVE_OP_REINTERPRET.  FML. */
  size_t i;
  for (i = 0; i < args_count; i++) {
    struct ast_typeexpr local_partial;
    local_partial.tag = AST_TYPEEXPR_UNKNOWN;
    if (!check_expr(es, &x->args[i], &local_partial, &args_annotated[i])) {
      goto fail_cleanup_args_types_and_annotated;
    }
    ast_typeexpr_init_copy(&args_types[i],
                           ast_expr_type(&args_annotated[i]));
  }

  ast_typeexpr_init_copy(&args_types[args_count], partial_type);

  ident_value func_ident = identmap_intern_c_str(es->cs->im, FUNC_TYPE_NAME);

  struct ast_typeexpr funcexpr;
  funcexpr.tag = AST_TYPEEXPR_APP;
  ast_typeapp_init(&funcexpr.u.app, ast_meta_make_garbage(),
                   make_ast_ident(func_ident),
                   args_types, args_types_count);

  struct ast_expr annotated_func;
  if (!check_expr(es, x->func, &funcexpr, &annotated_func)) {
    goto fail_cleanup_funcexpr;
  }

  /* We need to analyze annotated_func to see if it's a
  PRIMITIVE_OP_REINTERPRET op, which affects the calculation of
  whether a temporary object happens. */

  int is_PRIMITIVE_OP_REINTERPRET = 0;
  if (annotated_func.tag == AST_EXPR_NAME) {
    CHECK(annotated_func.u.name.info.info_valid);
    if (annotated_func.u.name.info.inst_or_null
        && annotated_func.u.name.info.inst_or_null->owner->is_primitive
        && annotated_func.u.name.info.inst_or_null->owner->primitive_op == PRIMITIVE_OP_REINTERPRET) {
      is_PRIMITIVE_OP_REINTERPRET = 1;
    }
  }

  struct ast_typeexpr return_type;
  copy_func_return_type(es->cs->im, ast_expr_type(&annotated_func),
                        args_types_count, &return_type);

  struct ast_expr_info expr_info;
  if (is_PRIMITIVE_OP_REINTERPRET) {
    CHECK(args_count == 1);
    expr_info = expr_info_typechecked_subobject(return_type, &args_annotated[0].info);
  } else {
    struct ast_typeexpr temporary_type;
    ast_typeexpr_init_copy(&temporary_type, &return_type);
    expr_info = ast_expr_info_typechecked_temporary(
        0,
        return_type,
        temporary_type,
        1,
        exprscope_temptag(es));
  }

  ast_expr_partial_init(annotated_out, AST_EXPR_FUNCALL, expr_info);
  ast_funcall_init(&annotated_out->u.funcall, ast_meta_make_copy(&x->meta),
                   annotated_func, args_annotated, args_count);

  ast_typeexpr_destroy(&funcexpr);
  return 1;
  /* Don't fallthrough -- args_annotated was moved into annotated_out. */
 fail_cleanup_funcexpr:
  ast_typeexpr_destroy(&funcexpr);
  SLICE_FREE(args_annotated, args_count, ast_expr_destroy);
  return 0;
  /* Don't fallthrough -- args_types was moved into funcexpr. */
 fail_cleanup_args_types_and_annotated:
  SLICE_FREE(args_types, i, ast_typeexpr_destroy);
  SLICE_FREE(args_annotated, i, ast_expr_destroy);
  return 0;
}

struct label_info {
  /* The name of the label in question. */
  ident_value label_name;
  /* True if the label has been observed. */
  int is_label_observed;
  /* True if a goto has been observed. */
  int is_goto_observed;
};

void label_info_wipe(struct label_info *a) {
  a->label_name = IDENT_VALUE_INVALID;
  a->is_label_observed = 0;
  a->is_goto_observed = 0;
}

struct bodystate {
  struct exprscope *es;
  struct ast_typeexpr *partial_type;
  int have_exact_return_type;
  struct ast_typeexpr exact_return_type;

  struct label_info *label_infos;
  size_t label_infos_count;
  size_t label_infos_limit;
};

/* TODO: Just asking, do we typecheck var or temporary destruction (or
gosh, copying or moving?) at any point? */

void bodystate_init(struct bodystate *bs, struct exprscope *es,
                    struct ast_typeexpr *partial_type) {
  bs->es = es;
  bs->partial_type = partial_type;
  bs->have_exact_return_type = 0;
  bs->label_infos = NULL;
  bs->label_infos_count = 0;
  bs->label_infos_limit = 0;
}

void bodystate_destroy(struct bodystate *bs) {
  bs->es = NULL;
  if (bs->have_exact_return_type) {
    ast_typeexpr_destroy(&bs->exact_return_type);
    bs->have_exact_return_type = 0;
  }
  SLICE_FREE(bs->label_infos, bs->label_infos_count, label_info_wipe);
  bs->label_infos_limit = 0;
}

void bodystate_note_goto(struct bodystate *bs, ident_value target) {
  for (size_t i = 0, e = bs->label_infos_count; i < e; i++) {
    if (bs->label_infos[i].label_name == target) {
      bs->label_infos[i].is_goto_observed = 1;
      return;
    }
  }
  struct label_info info;
  info.label_name = target;
  info.is_label_observed = 0;
  info.is_goto_observed = 1;
  SLICE_PUSH(bs->label_infos, bs->label_infos_count, bs->label_infos_limit,
             info);
}

int bodystate_note_label(struct bodystate *bs, struct ast_ident *name) {
  for (size_t i = 0, e = bs->label_infos_count; i < e; i++) {
    if (bs->label_infos[i].label_name == name->value) {
      if (bs->label_infos[i].is_label_observed) {
        METERR(name->meta, "Duplicate label %.*s.\n", IM_P(bs->es->cs->im, name->value));
        return 0;
      }
      bs->label_infos[i].is_label_observed = 1;
      return 1;
    }
  }
  struct label_info info;
  info.label_name = name->value;
  info.is_label_observed = 1;
  info.is_goto_observed = 0;
  SLICE_PUSH(bs->label_infos, bs->label_infos_count, bs->label_infos_limit,
             info);
  return 1;
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
  /* Says a statement/bracebody has a non-local entrance -- it is or
  has a label statement such that it could exit "out the bottom"
  without entering "from the top." */
  FALLTHROUGH_NONLOCAL,
};

enum fallthrough max_fallthrough(enum fallthrough x, enum fallthrough y) {
  return x < y ? y : x;
}

enum fallthrough compose_fallthrough(enum fallthrough top_reachability,
                                     enum fallthrough statement_fallthrough) {
  switch (statement_fallthrough) {
  case FALLTHROUGH_NEVER:
    return FALLTHROUGH_NEVER;
  case FALLTHROUGH_FROMTHETOP:
    return top_reachability;
  case FALLTHROUGH_NONLOCAL:
    return FALLTHROUGH_NONLOCAL;
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
    struct ast_typeexpr anything;
    anything.tag = AST_TYPEEXPR_UNKNOWN;
    struct ast_expr annotated_expr;
    if (!check_expr(bs->es, s->u.expr, &anything, &annotated_expr)) {
      goto fail;
    }
    annotated_out->tag = AST_STATEMENT_EXPR;
    ast_expr_alloc_move(annotated_expr, &annotated_out->u.expr);
    fallthrough = FALLTHROUGH_FROMTHETOP;
  } break;
  case AST_STATEMENT_RETURN_EXPR: {
    struct ast_expr annotated_expr;
    if (!check_expr(bs->es, s->u.return_expr, bs->partial_type, &annotated_expr)) {
      goto fail;
    }
    if (!bs->have_exact_return_type) {
      bs->have_exact_return_type = 1;
      ast_typeexpr_init_copy(&bs->exact_return_type,
                             ast_expr_type(&annotated_expr));
    } else {
      if (!exact_typeexprs_equal(&bs->exact_return_type,
                                 ast_expr_type(&annotated_expr))) {
        METERR(*ast_expr_ast_meta(s->u.return_expr), "Return statements with conflicting return types.%s", "\n");
        ast_expr_destroy(&annotated_expr);
        goto fail;
      }
    }
    annotated_out->tag = AST_STATEMENT_RETURN_EXPR;
    ast_expr_alloc_move(annotated_expr, &annotated_out->u.return_expr);
    fallthrough = FALLTHROUGH_NEVER;
  } break;
  case AST_STATEMENT_VAR: {
    struct ast_typeexpr replaced_type;
    replace_generics(bs->es, &s->u.var_statement.decl.type, &replaced_type);


    int has_rhs = s->u.var_statement.has_rhs;
    struct ast_expr annotated_rhs = { 0 };  /* Initialize to appease cl. */
    if (has_rhs) {
      if (!check_expr(bs->es, s->u.var_statement.rhs, &replaced_type, &annotated_rhs)) {
        ast_typeexpr_destroy(&replaced_type);
        goto fail;
      }
    }

    struct ast_ident name;
    ast_ident_init_copy(&name, &s->u.var_statement.decl.name);

    struct ast_typeexpr replaced_type_copy;
    ast_typeexpr_init_copy(&replaced_type_copy, &replaced_type);

    struct ast_vardecl *replaced_decl = malloc(sizeof(*replaced_decl));
    CHECK(replaced_decl);
    ast_vardecl_init(replaced_decl, ast_meta_make_copy(&s->u.var_statement.decl.meta), name,
                     replaced_type_copy);

    struct varnum varnum;
    if (!exprscope_push_var(bs->es, replaced_decl, &varnum)) {
      free_ast_vardecl(&replaced_decl);
      ast_typeexpr_destroy(&replaced_type);
      if (has_rhs) {
        ast_expr_destroy(&annotated_rhs);
      }
      goto fail;
    }

    vardecl_to_push = replaced_decl;

    struct ast_vardecl decl;
    ast_vardecl_init_copy(&decl, &s->u.var_statement.decl);
    ast_var_info_specify_varnum(&decl.var_info, varnum);
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
    ast_var_statement_info_note_type(
        &annotated_out->u.var_statement.info,
        replaced_type);
    fallthrough = FALLTHROUGH_FROMTHETOP;
  } break;
  case AST_STATEMENT_GOTO: {
    bodystate_note_goto(bs, s->u.goto_statement.target.value);
    ast_statement_init_copy(annotated_out, s);

    size_t vars_in_scope_count = bs->es->vars_count;
    struct varnum *vars_in_scope = malloc_mul(sizeof(*vars_in_scope), vars_in_scope_count);
    for (size_t i = 0; i < vars_in_scope_count; i++) {
      vars_in_scope[i] = bs->es->vars[i].varnum;
    }

    ast_statement_info_set_vars_in_scope(&annotated_out->u.goto_statement.goto_info,
                                         vars_in_scope,
                                         vars_in_scope_count);

    fallthrough = FALLTHROUGH_NEVER;
  } break;
  case AST_STATEMENT_LABEL: {
    if (!bodystate_note_label(bs, &s->u.label_statement.label)) {
      goto fail;
    }
    ast_statement_init_copy(annotated_out, s);

    /* TODO: Dedup with goto case (and slice-copying in general?) */
    size_t vars_in_scope_count = bs->es->vars_count;
    struct varnum *vars_in_scope = malloc_mul(sizeof(*vars_in_scope), vars_in_scope_count);
    for (size_t i = 0; i < vars_in_scope_count; i++) {
      vars_in_scope[i] = bs->es->vars[i].varnum;
    }

    ast_statement_info_set_vars_in_scope(&annotated_out->u.label_statement.info,
                                         vars_in_scope,
                                         vars_in_scope_count);

    fallthrough = FALLTHROUGH_NONLOCAL;
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

    struct ast_bracebody annotated_thenbody;
    enum fallthrough thenbody_fallthrough;
    if (!check_expr_bracebody(bs, &s->u.ifthen_statement.thenbody,
                              &annotated_thenbody, &thenbody_fallthrough)) {
      ast_expr_destroy(&annotated_condition);
      goto fail;
    }

    annotated_out->tag = AST_STATEMENT_IFTHEN;
    ast_ifthen_statement_init(
        &annotated_out->u.ifthen_statement,
        ast_meta_make_copy(&s->u.ifthen_statement.meta),
        annotated_condition,
        annotated_thenbody);
    fallthrough = max_fallthrough(FALLTHROUGH_FROMTHETOP, thenbody_fallthrough);
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
      struct ast_typeexpr anything;
      anything.tag = AST_TYPEEXPR_UNKNOWN;
      if (!check_expr(bs->es, fs->increment, &anything, &annotated_increment)) {
        goto for_fail_annotated_condition;
      }
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
the end", i.e. it has a label without a subsequent goto or return
statement.  This is more conservative than "conservative" analysis,
because we assume that all labels are reachable. */
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

  if (fallthrough != FALLTHROUGH_NEVER) {
    METERR(x->meta, "not all control paths return a value.%s", "\n");
    goto fail_annotated_bracebody;
  }

  for (size_t i = 0; i < bs.label_infos_count; i++) {
    if (!bs.label_infos[i].is_label_observed) {
      METERR(x->meta, "goto without label for name %.*s.\n",
             IM_P(bs.es->cs->im, bs.label_infos[i].label_name));
      goto fail_annotated_bracebody;
    }
    if (!bs.label_infos[i].is_goto_observed) {
      METERR(x->meta, "label without goto for name %.*s.\n",
             IM_P(bs.es->cs->im, bs.label_infos[i].label_name));
      goto fail_annotated_bracebody;
    }
  }

  if (!bs.have_exact_return_type) {
    METERR(x->meta, "Missing a return statement.%s", "\n");
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
  *out = funcexpr;
  {
    struct ast_vardecl *params = malloc_mul(sizeof(*params), x->params_count);
    for (size_t i = 0, e = x->params_count; i < e; i++) {
      ast_vardecl_init_copy(&params[i], &x->params[i]);
      ast_var_info_specify_varnum(&params[i].var_info, varnums[i]);
    }
    free(varnums);
    struct ast_typeexpr return_type;
    ast_typeexpr_init_copy(&return_type, &x->return_type);

    ast_lambda_init(annotated_out, ast_meta_make_copy(&x->meta),
                    params, x->params_count,
                    return_type, annotated_bracebody);
  }
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
  struct ast_typeexpr no_partial;
  no_partial.tag = AST_TYPEEXPR_UNKNOWN;

  struct ast_expr annotated_lhs;
  if (!check_expr(es, x->lhs, &no_partial, &annotated_lhs)) {
    goto cleanup;
  }

  struct ast_expr annotated_rhs;
  if (!check_expr(es, x->rhs, &no_partial, &annotated_rhs)) {
    goto cleanup_lhs;
  }

  switch (x->operator) {
  case AST_BINOP_ASSIGN: {
    if (es->computation == STATIC_COMPUTATION_YES) {
      METERR(x->meta, "Assignment within statically evaluated expression.%s", "\n");
      goto cleanup_both;
    }
    if (!annotated_lhs.info.is_lvalue) {
      METERR(x->meta, "Trying to assign to non-lvalue.%s", "\n");
      goto cleanup_both;
    }
    if (!exact_typeexprs_equal(ast_expr_type(&annotated_lhs),
                               ast_expr_type(&annotated_rhs))) {
      METERR(x->meta, "Assignment with non-matching types.%s", "\n");
      goto cleanup_both;
    }

    if (!unify_directionally(partial_type,
                             ast_expr_type(&annotated_lhs))) {
      METERR(x->meta, "LHS type of assignment does not match contextual type.%s", "\n");
      goto cleanup_both;
    }

    ast_typeexpr_init_copy(out, ast_expr_type(&annotated_lhs));
    *is_lvalue_out = 1;

    ast_binop_expr_init(annotated_out, ast_meta_make_copy(&x->meta),
                        x->operator, annotated_lhs, annotated_rhs);

    ret = 1;
    goto cleanup_just_rhs;
  } break;
  case AST_BINOP_LOGICAL_OR:
  case AST_BINOP_LOGICAL_AND: {
    struct ast_typeexpr boolean;
    boolean.tag = AST_TYPEEXPR_NAME;
    boolean.u.name = make_ast_ident(
        identmap_intern_c_str(es->cs->im, BOOLEAN_STANDIN_TYPE_NAME));

    if (!unify_directionally(&boolean,
                             ast_expr_type(&annotated_lhs))) {
      METERR(x->meta, "LHS of and/or is non-boolean.%s", "\n");
      ast_typeexpr_destroy(&boolean);
      goto cleanup_both;
    }

    if (!unify_directionally(&boolean,
                             ast_expr_type(&annotated_rhs))) {
      METERR(x->meta, "RHS of and/or is non-boolean.%s", "\n");
      ast_typeexpr_destroy(&boolean);
      goto cleanup_both;
    }

    if (!unify_directionally(partial_type, &boolean)) {
      METERR(x->meta, "And/or expression in non-boolean context.%s", "\n");
      ast_typeexpr_destroy(&boolean);
      goto cleanup_both;
    }

    *out = boolean;
    *is_lvalue_out = 0;

    ast_binop_expr_init(annotated_out, ast_meta_make_copy(&x->meta),
                        x->operator, annotated_lhs, annotated_rhs);

    ret = 1;
    goto cleanup_both;
  } break;
  default:
    UNREACHABLE();
  }

 cleanup_just_rhs:
  if (!ret) {
    ast_expr_destroy(&annotated_rhs);
  }
  goto cleanup;
 cleanup_both:
  if (!ret) {
    ast_expr_destroy(&annotated_rhs);
  }
 cleanup_lhs:
  if (!ret) {
    ast_expr_destroy(&annotated_lhs);
  }
 cleanup:
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
  struct ast_typeexpr no_partial;
  no_partial.tag = AST_TYPEEXPR_UNKNOWN;

  struct ast_expr annotated_rhs;
  if (!check_expr(es, x->rhs, &no_partial, &annotated_rhs)) {
    return 0;
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
                      struct ast_ident *field_name,
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
      METERR(field_name->meta, "Looking up field on primitive type.%s", "\n");
      return 0;
    }

    struct ast_deftype *deftype = ent->deftype;
    CHECK(!deftype->generics.has_type_params);
    if (!deftype_is_accessible(es, deftype)) {
      METERR(field_name->meta, "Looking up field on inaccessible type.%s", "\n");
      return 0;
    }
    return lookup_field_type(es, &deftype->type, field_name, field_type_out);
  } break;
  case AST_TYPEEXPR_APP: {
    struct deftype_entry *ent;
    if (!name_table_lookup_deftype(&es->cs->nt, type->u.app.name.value,
                                   param_list_arity(type->u.app.params_count),
                                   &ent)) {
      CRASH("lookup_field_type sees an invalid generic type.");
    }
    if (ent->is_primitive) {
      METERR(field_name->meta, "Looking up field on primitive type.%s", "\n");
      return 0;
    }

    struct ast_deftype *deftype = ent->deftype;
    CHECK(deftype->generics.has_type_params
          && deftype->generics.params_count == type->u.app.params_count);
    if (!deftype_is_accessible(es, deftype)) {
      METERR(field_name->meta, "Looking up field on inaccessible type.%s", "\n");
      return 0;
    }

    struct ast_typeexpr concrete_deftype_type;
    do_replace_generics(&deftype->generics,
                        type->u.app.params,
                        &deftype->type,
                        &concrete_deftype_type);

    int ret = lookup_field_type(es, &concrete_deftype_type, field_name,
                                field_type_out);
    ast_typeexpr_destroy(&concrete_deftype_type);
    return ret;
  } break;
  case AST_TYPEEXPR_STRUCTE:
    return lookup_fields_field_type(type->u.structe.fields,
                                    type->u.structe.fields_count, field_name,
                                    field_type_out);
  case AST_TYPEEXPR_UNIONE:
    return lookup_fields_field_type(type->u.unione.fields,
                                    type->u.unione.fields_count, field_name,
                                    field_type_out);
  case AST_TYPEEXPR_ARRAY: {
    METERR(field_name->meta, "Looking up field on array type.%s", "\n");
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
    struct ast_typeexpr *out,
    struct ast_local_field_access *annotated_out) {
  int ret = 0;
  struct ast_typeexpr lhs_partial_type;
  lhs_partial_type.tag = AST_TYPEEXPR_UNKNOWN;

  struct ast_expr annotated_lhs;
  if (!check_expr(es, x->lhs, &lhs_partial_type, &annotated_lhs)) {
    goto cleanup;
  }

  struct ast_typeexpr field_type;
  if (!lookup_field_type(es, ast_expr_type(&annotated_lhs),
                         &x->fieldname, &field_type)) {
    goto cleanup_lhs;
  }

  if (!unify_directionally(partial_type, &field_type)) {
    goto cleanup_field_type;
  }

  *out = field_type;
  struct ast_ident fieldname;
  ast_ident_init_copy(&fieldname, &x->fieldname);
  ast_local_field_access_init(annotated_out, ast_meta_make_copy(&x->meta),
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
  struct ast_typeexpr lhs_partial_type;
  lhs_partial_type.tag = AST_TYPEEXPR_UNKNOWN;

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
  if (!lookup_field_type(es, ptr_target, &x->fieldname, &field_type)) {
    goto cleanup_lhs;
  }

  if (!unify_directionally(partial_type, &field_type)) {
    METERR(x->meta, "Dereferencing field access results in wrong type.%s", "\n");
    goto cleanup_field_type;
  }

  *out = field_type;
  struct ast_ident fieldname;
  ast_ident_init_copy(&fieldname, &x->fieldname);
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
  return ret;
}

int check_is_index_rhs_type(struct checkstate *cs, struct ast_typeexpr *type) {
  return type->tag == AST_TYPEEXPR_NAME
    && (type->u.name.value == identmap_intern_c_str(cs->im, I32_TYPE_NAME)
        || type->u.name.value == identmap_intern_c_str(cs->im, U32_TYPE_NAME));
}

int check_index_expr(struct exprscope *es,
                     struct ast_index_expr *a,
                     struct ast_typeexpr *partial_type,
                     struct ast_expr *annotated_out) {
  struct ast_typeexpr no_partial_type;
  no_partial_type.tag = AST_TYPEEXPR_UNKNOWN;

  struct ast_expr lhs_annotated;
  if (!check_expr(es, a->lhs, &no_partial_type, &lhs_annotated)) {
    goto fail;
  }

  struct ast_expr rhs_annotated;
  if (!check_expr(es, a->rhs, &no_partial_type, &rhs_annotated)) {
    goto fail_lhs_annotated;
  }

  if (!check_is_index_rhs_type(es->cs, ast_expr_type(&rhs_annotated))) {
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

int check_expr(struct exprscope *es,
               struct ast_expr *x,
               struct ast_typeexpr *partial_type,
               struct ast_expr *annotated_out) {
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
    struct ast_typeexpr type;
    if (!check_expr_local_field_access(es, &x->u.local_field_access,
                                       partial_type, &type,
                                       &annotated_out->u.local_field_access)) {
      return 0;
    }
    ast_expr_partial_init(annotated_out,
                          AST_EXPR_LOCAL_FIELD_ACCESS,
                          expr_info_typechecked_subobject(
                              type,
                              &annotated_out->u.local_field_access.lhs->info));
    return 1;
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
    struct ast_typeexpr replaced_type;
    replace_generics(es, &x->u.typed_expr.type, &replaced_type);

    struct ast_expr annotated_lhs;
    if (!check_expr(es, x->u.typed_expr.lhs, &replaced_type,
                    &annotated_lhs)) {
      ast_typeexpr_destroy(&replaced_type);
      return 0;
    }
    ast_typeexpr_destroy(&replaced_type);

    struct ast_typeexpr old_type_copy;
    ast_typeexpr_init_copy(&old_type_copy, &x->u.typed_expr.type);
    ast_expr_partial_init(annotated_out, AST_EXPR_TYPED,
                          ast_expr_info_typechecked_identical(
                              &annotated_lhs.info));
    ast_typed_expr_init(&annotated_out->u.typed_expr,
                        ast_meta_make_copy(&x->u.typed_expr.meta),
                        annotated_lhs,
                        old_type_copy);
    return 1;
  } break;
  default:
    UNREACHABLE();
  }
}

/* Checks an expr, given that we know the type of expr. */
int check_expr_with_type(struct exprscope *es,
                         struct ast_expr *x,
                         struct ast_typeexpr *type,
                         struct ast_expr *annotated_out) {
  CHECK_DBG("check_expr_with_type\n");
  int ret = check_expr(es, x, type, annotated_out);
  return ret;
}

int check_def(struct checkstate *cs, struct ast_def *a) {
  if (!check_generics_shadowing(cs, &a->generics_)) {
    return 0;
  }

  if (!check_typeexpr(cs, &a->generics_, &a->type_, NULL)) {
    return 0;
  }

  /* We can only typecheck the def by instantiating it -- so we check
  the ones with no template params. */
  if (!a->generics_.has_type_params) {
    struct ast_typeexpr unified;
    struct def_entry *ent;
    struct def_instantiation *inst;
    int success = name_table_match_def(&cs->nt,
                                       &a->name_,
                                       NULL, 0, /* (no generics) */
                                       &a->type_,
                                       &unified,
                                       &ent,
                                       &inst);
    CHECK(success);
    CHECK(exact_typeexprs_equal(&unified, &a->type_));
    CHECK(!ent->is_primitive);

    ast_typeexpr_destroy(&unified);

    int ret;
    if (!inst->typecheck_started) {
      CHECK(!inst->annotated_rhs_computed);
      inst->typecheck_started = 1;
      struct exprscope es;
      exprscope_init(&es, cs, &a->generics_, NULL, 0,
                     ent->accessible, ent->accessible_count,
                     STATIC_COMPUTATION_YES, ent);
      struct ast_expr annotated_rhs;
      ret = check_expr_with_type(&es, &a->rhs_, &a->type_, &annotated_rhs);
      if (ret) {
        CHECK(!inst->annotated_rhs_computed);
        inst->annotated_rhs_computed = 1;
        inst->annotated_rhs = annotated_rhs;
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
    switch (func->u.primitive_op) {
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
    switch (func->u.primitive_op) {
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
    if (!eval_static_value(&funcall->args[i], &params[i])) {
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
    CHECK(inst_or_null->value_computed);
    static_value_init_copy(out, &inst_or_null->value);
    return 1;
  } break;
  case AST_EXPR_NUMERIC_LITERAL:
    return eval_static_numeric_literal(&expr->u.numeric_literal, out);
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
    return eval_static_value(expr->u.typed_expr.lhs, out);
  default:
    UNREACHABLE();
  }
}

int compute_static_values(struct name_table *nt, struct def_entry *ent) {
  int is_primitive = ent->is_primitive;
  CHECK(is_primitive || ent->def != NULL);
  for (size_t i = 0, e = ent->instantiations_count; i < e; i++) {
    struct def_instantiation *inst = ent->instantiations[i];
    CHECK(!inst->value_computed);

    if (is_primitive) {
      switch (ent->primitive_op) {
      case PRIMITIVE_OP_SIZEOF: {
        CHECK(inst->substitutions_count == 1);
        uint32_t size = kira_sizeof(nt, &inst->substitutions[0]);
        static_value_init_u32(&inst->value, size);
      } break;
      case PRIMITIVE_OP_ALIGNOF: {
        CHECK(inst->substitutions_count == 1);
        uint32_t alignment = kira_alignof(nt, &inst->substitutions[0]);
        static_value_init_u32(&inst->value, alignment);
      } break;
      default:
        static_value_init_primitive_op(&inst->value, ent->primitive_op);
        break;
      }
      inst->value_computed = 1;
    } else {
      CHECK(inst->annotated_rhs_computed);
      if (!eval_static_value(&inst->annotated_rhs, &inst->value)) {
        return 0;
      }
      inst->value_computed = 1;
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
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
                               "  return x;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_2(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;\n"
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
                               "  if (~z) {\n"
                               "    goto foo;\n"
                               "    var k i32 = y(x);\n"
                               "  } else {\n"
                               "    return x;\n"
                               "  }\n"
                               "  label foo;\n"
                               "  return z;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_3(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  /* Fails because there's no label named foo. */
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;\n"
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
                               "  if (z) {\n"
                               "    y;\n"
                               "    goto foo;\n"
                               "  } else {\n"
                               "    return x;\n"
                               "  }\n"
                               "  return z;\n"
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
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
                               "  return k;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_5(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  /* Fails because x shadows a global. */
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;\n"
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
                               "  var x i32 = 4;\n"
                               "  return z;\n"
                               "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_6(const uint8_t *name, size_t name_count,
                             uint8_t **data_out, size_t *data_count_out) {
  /* Fails because z shadows a local. */
  struct test_module a[] = { { "foo",
                               "def x i32 = 3;\n"
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
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
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
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
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
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
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
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
                               "def y func[i32, i32] = fn(z i32)i32 {\n"
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
                               "def y func[foo, i32] = fn(z foo) i32 {\n"
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
                               "def y func[foo, i32] = fn(z foo) i32 {\n"
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
      "def y func[foo[i32], i32] = fn(z foo[i32]) i32 {\n"
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
      "def y func[foo[u32], i32] = fn(z foo[u32]) i32 {\n"
      "  return z.x + z.y;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_lambda_15(const uint8_t *name, size_t name_count,
                              uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { { "foo",
                               "def y func[i32, i32] = fn(z i32) i32 {\n"
                               "  var k func[i32, i32] = fn(m i32) i32 {\n"
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
                               "def y func[i32, i32] = fn(z i32) i32 {\n"
                               "  var k func[i32, i32] = fn(m i32) i32 {\n"
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
      "def y func[ptr[foo], i32] = fn(z ptr[foo]) i32 {\n"
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
      "def y func[ptr[foo[i32]], i32] = fn(z ptr[foo[i32]]) i32 {\n"
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
      "def y func[ptr[foo[i32]], i32] = fn(z ptr[foo[i32]]) i32 {\n"
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
      "def y func[ptr[foo[i32]], i32] = fn(z ptr[foo[i32]]) i32 {\n"
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
      "def y func[ptr[foo[i32]], i32] = fn(z ptr[foo[i32]]) i32 {\n"
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
      "def[T] foo func[ptr[T], T] = fn(x ptr[T]) T { return *x; };\n"
      "def bar func[i32] = fn() i32 {\n"
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
      "def[T] foo func[ptr[T], T] = fn(x ptr[T]) T { return *x; };\n"
      "def bar func[i32] = fn() i32 {\n"
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
      "def[T] fac func[T, T] = fn(x T) T {\n"
      "  if (x == 0) {\n"
      "    return 1;\n"
      "  } else {\n"
      "    return x * fac(x - 1);\n"
      "  }\n"
      "};\n"
      "def bar func[i32] = fn() i32 {\n"
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
      "def[T] biggefy func[T, foo[T]] = fn(x T) foo[T] {\n"
      "  return biggefy(x);\n"
      "};\n"
      "def[T] rec func[T, i32] = fn(x T) i32 {\n"
      "  return rec(biggefy(x));\n"
      "};\n"
      "def bar func[i32] = fn() i32 {\n"
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
      "def z func[i32, i32] = fn(k i32) i32 { return k + 1; };\n"
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
      "def z func[i32, i32] = fn(k i32) i32 { return k + 1; };\n"
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
      "extern putchar func[i32, i32];\n"
      "def foo func[i32] = fn()i32 {\n"
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
      "extern putchar func[i32, i32];\n"
      "def foo func[i32] = fn()i32 {\n"
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
      "extern putchar func[i32, quack];\n"
      "def foo func[i32] = fn()i32 {\n"
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
      "def foo func[u32, u32] = fn(x u32) u32 {\n"
      "  return x + 4u;\n"
      "};"
      "def bar func[i32, i32] = foo;\n"
      "def foo func[i32, i32] = fn(x i32) i32 {\n"
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
      "def foo func[u32, u32] = fn(x u32) u32 {\n"
      "  return x + 4u;\n"
      "};"
      "def bar func[i32, i32] = foo;\n"
      "def[T] foo func[T, i32] = fn(x T) i32 {\n"
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
      "def foo func[u32, u32] = fn(x u32) u32 {\n"
      "  return x + 4u;\n"
      "};"
      "def bar func[u32, i32] = foo;\n"
      "def[T] foo func[T, i32] = fn(x T) i32 {\n"
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
      "def foo func[u32, u32] = fn(x u32) u32 {\n"
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
      "def[T] foo func[T, T] = fn(x T) T {\n"
      "  var y T = x;\n"
      "  return y;\n"
      "};\n"
      "def bar func[i32] = fn() i32 {\n"
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
      "def[T] add32 func[i32, T, i32] = fn(x i32, y T) i32 {\n"
      "  var z i32 = ~4;\n"
      "  return x + z;\n"
      "};\n"
      "def bar func[i32] = fn() i32 {\n"
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
      "def foo func[i32] = fn() i32 {\n"
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
      "def[T] foo func[ptr[T], i32, T] = fn(p ptr[T], i i32) T {\n"
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
      "def foo func[[3]u32, vec3] = fn(arr [3]u32) vec3 {\n"
      "  var vec3 v = arr;\n"
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
      "def foo func[i32] = fn() i32 {\n"
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
      "def foo func[i32] = fn() i32 {\n"
      "  return 2 + ~3u :: i32;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_15(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "def foo func[i32] = fn() i32 {\n"
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
      "def foo func[i32] = fn() i32 {\n"
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
      "def foo func[i32] = fn() i32 {\n"
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
      "def foo func[i32] = fn() i32 {\n"
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
      "def[T] foo func[i32, T] = fn(x i32) T {\n"
      "  // Why not test ':: T' works where T is generic.\n"
      "  var y T = (~x :: T);\n"
      "  return y;\n"
      "};\n"
      "def bar func[func[i32, i16], i32, i16] = fn(x func[i32, i16], y i32) i16 {\n"
      "  return x(y);\n"
      "};\n"
      "def baz func[i16] = fn() i16 {\n"
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
      "def[T] foo func[i32, T] = fn(x i32) T {\n"
      "  // Why not test ':: T' works where T is generic.\n"
      "  var y T = (~x :: T);\n"
      "  return y;\n"
      "};\n"
      "def bar func[func[i32, i16], i32, i16] = fn(x func[i32, i16], y i32) i16 {\n"
      "  return x(y);\n"
      "};\n"
      "def baz func[i16] = fn() i16 {\n"
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
      "def foo func[u32] = fn() u32 {\n"
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
      "def foo func[*ty, i32] = fn(t *ty) i32 {\n"
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
      "def foo func[*ty, i32] = fn(t *ty) i32 {\n"
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
      "def foo func[*ty, i32] = fn(t *ty) i32 {\n"
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
      "def foo func[*ty, i32] = fn(t *ty) i32 {\n"
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
      "def[T] foo func[*ty[T], T] = fn(t *ty[T]) T {\n"
      "  return t->x;\n"
      "};\n"
      "}\n"
      "def bar func[*ty[i32], i32] = foo;\n"
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
      "def[T] foo func[*ty[T], T] = fn(t *ty[T]) T {\n"
      "  return t->x;\n"
      "};\n"
      "def bar func[*ty[i32], i32] = foo;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_28(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "deftype ty i32;\n"
      "def foo func[ty, i32] = fn(t ty) i32 {\n"
      "  return ~t;\n"
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
      "def foo func[ty, i32] = fn(t ty) i32 {\n"
      "  return ~t;\n"
      "};\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_30(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "defclass ty i32;\n"
      "access ty {\n"
      "def foo func[ty, i32] = fn(t ty) i32 {\n"
      "  return ~t;\n"
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
      "def[T] foo func[ty[T], i32] = fn(t ty[T]) i32 {\n"
      "  return ~t;\n"
      "};\n"
      "def bar func[ty[u32], i32] = foo;\n"
    } };

  return load_test_module(a, sizeof(a) / sizeof(a[0]),
                          name, name_count, data_out, data_count_out);
}

int check_file_test_more_32(const uint8_t *name, size_t name_count,
                            uint8_t **data_out, size_t *data_count_out) {
  struct test_module a[] = { {
      "foo",
      "defclass[T] ty i32;\n"
      "access ty[_] {\n"
      "def[T] foo func[ty[T], i32] = fn(t ty[T]) i32 {\n"
      "  return ~t;\n"
      "};\n"
      "}\n"
      "def bar func[ty[u32], i32] = foo;\n"
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
      "def foo func[*ty, *i32] = fn(t *ty) *i32 {\n"
      "  return ~t;\n"
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
      "def foo func[*ty, *i32] = fn(t *ty) *i32 {\n"
      "  return ~t;\n"
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
      "def[T] foo func[*ty[T], *i32] = fn(t *ty[T]) *i32 {\n"
      "  return ~t;\n"
      "};\n"
      "def bar func[*ty[u32], *i32] = foo;\n"
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
      "def[T] foo func[*ty[T], *i32] = fn(t *ty[T]) *i32 {\n"
      "  return ~t;\n"
      "};\n"
      "}\n"
      "def bar func[*ty[u32], *i32] = foo;\n"
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

  DBG("test_check_file check_file_test_lambda_2...\n");
  if (!test_check_module(&im, &check_file_test_lambda_2, foo)) {
    DBG("check_file_test_lambda_2 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_3...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_3, foo)) {
    DBG("check_file_test_lambda_3 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_4...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_4, foo)) {
    DBG("check_file_test_lambda_4 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_lambda_5...\n");
  if (!!test_check_module(&im, &check_file_test_lambda_5, foo)) {
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

  DBG("test_check_file check_file_test_more_30...\n");
  if (!test_check_module(&im, &check_file_test_more_30, foo)) {
    DBG("check_file_test_more_30 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file !check_file_test_more_31...\n");
  if (!!test_check_module(&im, &check_file_test_more_31, foo)) {
    DBG("check_file_test_more_31 fails\n");
    goto cleanup_identmap;
  }

  DBG("test_check_file check_file_test_more_32...\n");
  if (!test_check_module(&im, &check_file_test_more_32, foo)) {
    DBG("check_file_test_more_32 fails\n");
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

  ret = 1;
 cleanup_identmap:
  identmap_destroy(&im);
  return ret;
}
