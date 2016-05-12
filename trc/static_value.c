#include "static_value.h"

void static_value_init_i32(struct static_value *a, int32_t i32_value) {
  a->tag = STATIC_VALUE_I32;
  a->u.i32_value = i32_value;
}

void static_value_init_u32(struct static_value *a, uint32_t u32_value) {
  a->tag = STATIC_VALUE_U32;
  a->u.u32_value = u32_value;
}

void static_value_init_u8(struct static_value *a, uint8_t u8_value) {
  a->tag = STATIC_VALUE_U8;
  a->u.u8_value = u8_value;
}

void static_value_init_bool(struct static_value *a, int bool_value) {
  CHECK(bool_value == 0 || bool_value == 1);
  a->tag = STATIC_VALUE_BOOL;
  a->u.bool_value = bool_value;
}

void static_value_init_enumvoid(struct static_value *a, size_t enumconstruct_number, size_t enumsize) {
  a->tag = STATIC_VALUE_ENUMVOID;
  a->u.enumvoid_value.enumconstruct_number = enumconstruct_number;
  a->u.enumvoid_value.enumsize = enumsize;
}

void static_value_init_typechecked_lambda(struct static_value *a,
                                          struct ast_expr lambda) {
  CHECK(lambda.info.typechecked == AST_TYPECHECKED_YES);
  a->tag = STATIC_VALUE_LAMBDA;
  a->u.typechecked_lambda = lambda;
}

void static_value_init_primitive_op(struct static_value *a,
                                    struct primitive_op primitive_op) {
  a->tag = STATIC_VALUE_PRIMITIVE_OP;
  a->u.primitive_op = primitive_op;
}

void static_value_init_copy(struct static_value *a, struct static_value *c) {
  a->tag = c->tag;
  switch (c->tag) {
  case STATIC_VALUE_U32:
    a->u.u32_value = c->u.u32_value;
    break;
  case STATIC_VALUE_I32:
    a->u.i32_value = c->u.i32_value;
    break;
  case STATIC_VALUE_U8:
    a->u.u8_value = c->u.u8_value;
    break;
  case STATIC_VALUE_BOOL:
    a->u.bool_value = c->u.bool_value;
    break;
  case STATIC_VALUE_ENUMVOID:
    a->u.enumvoid_value = c->u.enumvoid_value;
    break;
  case STATIC_VALUE_LAMBDA:
    ast_expr_init_copy(&a->u.typechecked_lambda,
                       &c->u.typechecked_lambda);
    break;
  case STATIC_VALUE_PRIMITIVE_OP:
    a->u.primitive_op = c->u.primitive_op;
    break;
  default:
    UNREACHABLE();
  }
}

void static_value_init_move(struct static_value *a, struct static_value *m) {
  a->tag = m->tag;
  switch (m->tag) {
  case STATIC_VALUE_U32:
    a->u.u32_value = m->u.u32_value;
    break;
  case STATIC_VALUE_I32:
    a->u.i32_value = m->u.i32_value;
    break;
  case STATIC_VALUE_U8:
    a->u.u8_value = m->u.u8_value;
    break;
  case STATIC_VALUE_BOOL:
    a->u.bool_value = m->u.bool_value;
    break;
  case STATIC_VALUE_ENUMVOID:
    a->u.enumvoid_value = m->u.enumvoid_value;
    break;
  case STATIC_VALUE_LAMBDA:
    a->u.typechecked_lambda = m->u.typechecked_lambda;
    break;
  case STATIC_VALUE_PRIMITIVE_OP:
    a->u.primitive_op = m->u.primitive_op;
    break;
  default:
    UNREACHABLE();
  }
}

void static_value_destroy(struct static_value *sv) {
  switch (sv->tag) {
  case STATIC_VALUE_U32: /* fallthrough */
  case STATIC_VALUE_I32: /* fallthrough */
  case STATIC_VALUE_U8: /* fallthrough */
  case STATIC_VALUE_BOOL: /* fallthrough */
  case STATIC_VALUE_ENUMVOID:
    break;
  case STATIC_VALUE_LAMBDA:
    ast_expr_destroy(&sv->u.typechecked_lambda);
    break;
  case STATIC_VALUE_PRIMITIVE_OP:
    break;
  default:
    UNREACHABLE();
  }
  sv->tag = (enum static_value_tag)-1;
}
