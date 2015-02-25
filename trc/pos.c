#include "pos.h"

struct pos make_pos(size_t offset, size_t line, size_t column,
                    ident_value filename) {
  struct pos ret;
  ret.offset = offset;
  ret.line = line;
  ret.column = column;
  ret.filename = filename;
  return ret;
}

