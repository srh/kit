#ifndef KIRA_PRIMITIVE_H_
#define KIRA_PRIMITIVE_H_

#include <stddef.h>

enum primitive_op_tag {
  /* TODO: Implement (or change what primitive_ops generally are...) */
  PRIMITIVE_OP_ENUMCONSTRUCT,

  PRIMITIVE_OP_INIT,
  PRIMITIVE_OP_COPY,
  PRIMITIVE_OP_MOVE,
  PRIMITIVE_OP_DESTROY,

  PRIMITIVE_OP_CONVERT_U8_TO_U8,
  PRIMITIVE_OP_CONVERT_U8_TO_I8,
  PRIMITIVE_OP_CONVERT_U8_TO_U16,
  PRIMITIVE_OP_CONVERT_U8_TO_I16,
  PRIMITIVE_OP_CONVERT_U8_TO_U32,
  PRIMITIVE_OP_CONVERT_U8_TO_I32,
  PRIMITIVE_OP_CONVERT_U8_TO_SIZE,
  PRIMITIVE_OP_CONVERT_U8_TO_OSIZE,

  PRIMITIVE_OP_CONVERT_I8_TO_U8,
  PRIMITIVE_OP_CONVERT_I8_TO_I8,
  PRIMITIVE_OP_CONVERT_I8_TO_U16,
  PRIMITIVE_OP_CONVERT_I8_TO_I16,
  PRIMITIVE_OP_CONVERT_I8_TO_U32,
  PRIMITIVE_OP_CONVERT_I8_TO_I32,
  PRIMITIVE_OP_CONVERT_I8_TO_SIZE,
  PRIMITIVE_OP_CONVERT_I8_TO_OSIZE,

  PRIMITIVE_OP_CONVERT_U16_TO_U8,
  PRIMITIVE_OP_CONVERT_U16_TO_I8,
  PRIMITIVE_OP_CONVERT_U16_TO_U16,
  PRIMITIVE_OP_CONVERT_U16_TO_I16,
  PRIMITIVE_OP_CONVERT_U16_TO_U32,
  PRIMITIVE_OP_CONVERT_U16_TO_I32,
  PRIMITIVE_OP_CONVERT_U16_TO_SIZE,
  PRIMITIVE_OP_CONVERT_U16_TO_OSIZE,

  PRIMITIVE_OP_CONVERT_I16_TO_U8,
  PRIMITIVE_OP_CONVERT_I16_TO_I8,
  PRIMITIVE_OP_CONVERT_I16_TO_U16,
  PRIMITIVE_OP_CONVERT_I16_TO_I16,
  PRIMITIVE_OP_CONVERT_I16_TO_U32,
  PRIMITIVE_OP_CONVERT_I16_TO_I32,
  PRIMITIVE_OP_CONVERT_I16_TO_SIZE,
  PRIMITIVE_OP_CONVERT_I16_TO_OSIZE,

  PRIMITIVE_OP_CONVERT_U32_TO_U8,
  PRIMITIVE_OP_CONVERT_U32_TO_I8,
  PRIMITIVE_OP_CONVERT_U32_TO_U16,
  PRIMITIVE_OP_CONVERT_U32_TO_I16,
  PRIMITIVE_OP_CONVERT_U32_TO_U32,
  PRIMITIVE_OP_CONVERT_U32_TO_I32,
  PRIMITIVE_OP_CONVERT_U32_TO_SIZE,
  PRIMITIVE_OP_CONVERT_U32_TO_OSIZE,

  PRIMITIVE_OP_CONVERT_I32_TO_U8,
  PRIMITIVE_OP_CONVERT_I32_TO_I8,
  PRIMITIVE_OP_CONVERT_I32_TO_U16,
  PRIMITIVE_OP_CONVERT_I32_TO_I16,
  PRIMITIVE_OP_CONVERT_I32_TO_U32,
  PRIMITIVE_OP_CONVERT_I32_TO_I32,
  PRIMITIVE_OP_CONVERT_I32_TO_SIZE,
  PRIMITIVE_OP_CONVERT_I32_TO_OSIZE,

  PRIMITIVE_OP_CONVERT_SIZE_TO_U8,
  PRIMITIVE_OP_CONVERT_SIZE_TO_I8,
  PRIMITIVE_OP_CONVERT_SIZE_TO_U16,
  PRIMITIVE_OP_CONVERT_SIZE_TO_I16,
  PRIMITIVE_OP_CONVERT_SIZE_TO_U32,
  PRIMITIVE_OP_CONVERT_SIZE_TO_I32,
  PRIMITIVE_OP_CONVERT_SIZE_TO_SIZE,
  PRIMITIVE_OP_CONVERT_SIZE_TO_OSIZE,

  PRIMITIVE_OP_CONVERT_OSIZE_TO_U8,
  PRIMITIVE_OP_CONVERT_OSIZE_TO_I8,
  PRIMITIVE_OP_CONVERT_OSIZE_TO_U16,
  PRIMITIVE_OP_CONVERT_OSIZE_TO_I16,
  PRIMITIVE_OP_CONVERT_OSIZE_TO_U32,
  PRIMITIVE_OP_CONVERT_OSIZE_TO_I32,
  PRIMITIVE_OP_CONVERT_OSIZE_TO_SIZE,
  PRIMITIVE_OP_CONVERT_OSIZE_TO_OSIZE,

  PRIMITIVE_OP_NEGATE_I8,
  PRIMITIVE_OP_NEGATE_I16,
  PRIMITIVE_OP_NEGATE_I32,

  PRIMITIVE_OP_LOGICAL_NOT,

  /* TODO: Idk about this. */
  PRIMITIVE_OP_SIZEOF,
  PRIMITIVE_OP_ALIGNOF,

  PRIMITIVE_OP_BIT_NOT_I8,
  PRIMITIVE_OP_BIT_NOT_U8,
  PRIMITIVE_OP_BIT_NOT_I16,
  PRIMITIVE_OP_BIT_NOT_U16,
  PRIMITIVE_OP_BIT_NOT_I32,
  PRIMITIVE_OP_BIT_NOT_U32,
  PRIMITIVE_OP_BIT_NOT_SIZE,
  PRIMITIVE_OP_BIT_NOT_OSIZE,

  PRIMITIVE_OP_EQ_PTR,
  PRIMITIVE_OP_NE_PTR,

  PRIMITIVE_OP_LT_BOOL,
  PRIMITIVE_OP_LE_BOOL,
  PRIMITIVE_OP_GT_BOOL,
  PRIMITIVE_OP_GE_BOOL,
  PRIMITIVE_OP_EQ_BOOL,
  PRIMITIVE_OP_NE_BOOL,
  PRIMITIVE_OP_BIT_XOR_BOOL,
  PRIMITIVE_OP_BIT_OR_BOOL,
  PRIMITIVE_OP_BIT_AND_BOOL,

  PRIMITIVE_OP_ADD_U8,
  PRIMITIVE_OP_SUB_U8,
  PRIMITIVE_OP_MUL_U8,
  PRIMITIVE_OP_DIV_U8,
  PRIMITIVE_OP_MOD_U8,
  PRIMITIVE_OP_LT_U8,
  PRIMITIVE_OP_LE_U8,
  PRIMITIVE_OP_GT_U8,
  PRIMITIVE_OP_GE_U8,
  PRIMITIVE_OP_EQ_U8,
  PRIMITIVE_OP_NE_U8,
  PRIMITIVE_OP_BIT_XOR_U8,
  PRIMITIVE_OP_BIT_OR_U8,
  PRIMITIVE_OP_BIT_AND_U8,
  PRIMITIVE_OP_BIT_LEFTSHIFT_U8,
  PRIMITIVE_OP_BIT_RIGHTSHIFT_U8,

  PRIMITIVE_OP_ADD_I8,
  PRIMITIVE_OP_SUB_I8,
  PRIMITIVE_OP_MUL_I8,
  PRIMITIVE_OP_DIV_I8,
  PRIMITIVE_OP_MOD_I8,
  PRIMITIVE_OP_LT_I8,
  PRIMITIVE_OP_LE_I8,
  PRIMITIVE_OP_GT_I8,
  PRIMITIVE_OP_GE_I8,
  PRIMITIVE_OP_EQ_I8,
  PRIMITIVE_OP_NE_I8,
  PRIMITIVE_OP_BIT_XOR_I8,
  PRIMITIVE_OP_BIT_OR_I8,
  PRIMITIVE_OP_BIT_AND_I8,
  PRIMITIVE_OP_BIT_LEFTSHIFT_I8,
  PRIMITIVE_OP_BIT_RIGHTSHIFT_I8,

  PRIMITIVE_OP_ADD_U16,
  PRIMITIVE_OP_SUB_U16,
  PRIMITIVE_OP_MUL_U16,
  PRIMITIVE_OP_DIV_U16,
  PRIMITIVE_OP_MOD_U16,
  PRIMITIVE_OP_LT_U16,
  PRIMITIVE_OP_LE_U16,
  PRIMITIVE_OP_GT_U16,
  PRIMITIVE_OP_GE_U16,
  PRIMITIVE_OP_EQ_U16,
  PRIMITIVE_OP_NE_U16,
  PRIMITIVE_OP_BIT_XOR_U16,
  PRIMITIVE_OP_BIT_OR_U16,
  PRIMITIVE_OP_BIT_AND_U16,
  PRIMITIVE_OP_BIT_LEFTSHIFT_U16,
  PRIMITIVE_OP_BIT_RIGHTSHIFT_U16,

  PRIMITIVE_OP_ADD_I16,
  PRIMITIVE_OP_SUB_I16,
  PRIMITIVE_OP_MUL_I16,
  PRIMITIVE_OP_DIV_I16,
  PRIMITIVE_OP_MOD_I16,
  PRIMITIVE_OP_LT_I16,
  PRIMITIVE_OP_LE_I16,
  PRIMITIVE_OP_GT_I16,
  PRIMITIVE_OP_GE_I16,
  PRIMITIVE_OP_EQ_I16,
  PRIMITIVE_OP_NE_I16,
  PRIMITIVE_OP_BIT_XOR_I16,
  PRIMITIVE_OP_BIT_OR_I16,
  PRIMITIVE_OP_BIT_AND_I16,
  PRIMITIVE_OP_BIT_LEFTSHIFT_I16,
  PRIMITIVE_OP_BIT_RIGHTSHIFT_I16,

  PRIMITIVE_OP_ADD_U32,
  PRIMITIVE_OP_SUB_U32,
  PRIMITIVE_OP_MUL_U32,
  PRIMITIVE_OP_DIV_U32,
  PRIMITIVE_OP_MOD_U32,
  PRIMITIVE_OP_LT_U32,
  PRIMITIVE_OP_LE_U32,
  PRIMITIVE_OP_GT_U32,
  PRIMITIVE_OP_GE_U32,
  PRIMITIVE_OP_EQ_U32,
  PRIMITIVE_OP_NE_U32,
  PRIMITIVE_OP_BIT_XOR_U32,
  PRIMITIVE_OP_BIT_OR_U32,
  PRIMITIVE_OP_BIT_AND_U32,
  PRIMITIVE_OP_BIT_LEFTSHIFT_U32,
  PRIMITIVE_OP_BIT_RIGHTSHIFT_U32,

  PRIMITIVE_OP_ADD_I32,
  PRIMITIVE_OP_SUB_I32,
  PRIMITIVE_OP_MUL_I32,
  PRIMITIVE_OP_DIV_I32,
  PRIMITIVE_OP_MOD_I32,
  PRIMITIVE_OP_LT_I32,
  PRIMITIVE_OP_LE_I32,
  PRIMITIVE_OP_GT_I32,
  PRIMITIVE_OP_GE_I32,
  PRIMITIVE_OP_EQ_I32,
  PRIMITIVE_OP_NE_I32,
  PRIMITIVE_OP_BIT_XOR_I32,
  PRIMITIVE_OP_BIT_OR_I32,
  PRIMITIVE_OP_BIT_AND_I32,
  PRIMITIVE_OP_BIT_LEFTSHIFT_I32,
  PRIMITIVE_OP_BIT_RIGHTSHIFT_I32,

  PRIMITIVE_OP_ADD_SIZE,
  PRIMITIVE_OP_SUB_SIZE,
  PRIMITIVE_OP_MUL_SIZE,
  PRIMITIVE_OP_DIV_SIZE,
  PRIMITIVE_OP_MOD_SIZE,
  PRIMITIVE_OP_LT_SIZE,
  PRIMITIVE_OP_LE_SIZE,
  PRIMITIVE_OP_GT_SIZE,
  PRIMITIVE_OP_GE_SIZE,
  PRIMITIVE_OP_EQ_SIZE,
  PRIMITIVE_OP_NE_SIZE,
  PRIMITIVE_OP_BIT_XOR_SIZE,
  PRIMITIVE_OP_BIT_OR_SIZE,
  PRIMITIVE_OP_BIT_AND_SIZE,
  PRIMITIVE_OP_BIT_LEFTSHIFT_SIZE,
  PRIMITIVE_OP_BIT_RIGHTSHIFT_SIZE,

  PRIMITIVE_OP_ADD_OSIZE,
  PRIMITIVE_OP_SUB_OSIZE,
  PRIMITIVE_OP_MUL_OSIZE,
  PRIMITIVE_OP_DIV_OSIZE,
  PRIMITIVE_OP_MOD_OSIZE,
  PRIMITIVE_OP_LT_OSIZE,
  PRIMITIVE_OP_LE_OSIZE,
  PRIMITIVE_OP_GT_OSIZE,
  PRIMITIVE_OP_GE_OSIZE,
  PRIMITIVE_OP_EQ_OSIZE,
  PRIMITIVE_OP_NE_OSIZE,
  PRIMITIVE_OP_BIT_XOR_OSIZE,
  PRIMITIVE_OP_BIT_OR_OSIZE,
  PRIMITIVE_OP_BIT_AND_OSIZE,
  PRIMITIVE_OP_BIT_LEFTSHIFT_OSIZE,
  PRIMITIVE_OP_BIT_RIGHTSHIFT_OSIZE,
};

#define PRIMITIVE_OP_INVALID ((enum primitive_op_tag)-1)

/* This is a POD-like native-copyable type. */
struct primitive_op {
  enum primitive_op_tag tag;
  union {
    size_t enumconstruct_number;
  } u;
};

struct primitive_op make_primop(enum primitive_op_tag tag);
struct primitive_op make_enumconstruct_op(size_t enumconstruct_number);

#endif /* KIRA_PRIMITIVE_H_ */
