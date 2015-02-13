#include "primitive.h"

#include "util.h"

struct primitive_op make_primop(enum primitive_op_tag tag) {
  CHECK(tag != PRIMITIVE_OP_ENUMCONSTRUCT);
  struct primitive_op ret;
  ret.tag = tag;
  return ret;
}
