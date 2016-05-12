#ifndef KIT_STATIC_VALUE_H_
#define KIT_STATIC_VALUE_H_

#include "ast.h"
#include "primitive.h"

enum static_value_tag {
  STATIC_VALUE_I32,
  STATIC_VALUE_U32,
  STATIC_VALUE_U8,
  STATIC_VALUE_BOOL,
  STATIC_VALUE_LAMBDA,
  STATIC_VALUE_ENUMVOID,
  STATIC_VALUE_PRIMITIVE_OP,
};

struct static_value_enum_void {
  size_t enumconstruct_number;
  /* The size of the enum type -- I'm not sure if this is necessary. */
  size_t enumsize;
};

struct static_value {
  enum static_value_tag tag;
  union {
    int32_t i32_value;
    uint32_t u32_value;
    uint8_t u8_value;
    int bool_value;
    struct static_value_enum_void enumvoid_value;
    /* An owned ref to the _typechecked_, annotated AST. */
    struct ast_expr typechecked_lambda;
    struct primitive_op primitive_op;
  } u;
};

void static_value_init_i32(struct static_value *a, int32_t i32_value);
void static_value_init_u32(struct static_value *a, uint32_t u32_value);
void static_value_init_u8(struct static_value *a, uint8_t u8_value);
void static_value_init_bool(struct static_value *a, int bool_value);
void static_value_init_enumvoid(struct static_value *a, size_t enumconstruct_number, size_t enumsize);
void static_value_init_typechecked_lambda(struct static_value *a,
                                          struct ast_expr lambda);
void static_value_init_primitive_op(struct static_value *a,
                                    struct primitive_op primitive_op);
void static_value_init_copy(struct static_value *a, struct static_value *c);
void static_value_init_move(struct static_value *a, struct static_value *m);
void static_value_destroy(struct static_value *a);


#endif /* KIT_STATIC_VALUE_H_ */
