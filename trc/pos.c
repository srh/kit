#include "pos.h"

struct pos make_pos(size_t global_offset, size_t line, size_t column,
                    ident_value filename) {
  struct pos ret;
  ret.global_offset = global_offset;
  ret.line = line;
  ret.column = column;
  ret.filename = filename;
  return ret;
}

