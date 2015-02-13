#include "primitive.h"

#include "util.h"

/* Not good for every kind of primop, just the ones that only need a
tag. */
struct primitive_op make_primop(enum primitive_op_tag tag) {
  CHECK(tag != PRIMITIVE_OP_ENUMCONSTRUCT);
  struct primitive_op ret;
  ret.tag = tag;
  return ret;
}

struct primitive_op make_enumconstruct_op(size_t enumconstruct_number) {
  struct primitive_op ret;
  ret.tag = PRIMITIVE_OP_ENUMCONSTRUCT;
  ret.u.enumconstruct_number = enumconstruct_number;
  return ret;
}
