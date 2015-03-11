#include "pos.h"

#include "arith.h"

struct pos make_pos(size_t global_offset) {
  struct pos ret;
  ret.global_offset = global_offset;
  return ret;
}

size_t compute_line(const uint8_t *buf, size_t offset) {
  size_t newlines = 0;
  for (size_t i = 0; i < offset; i++) {
    newlines += (buf[i] == '\n');
  }

  return size_add(newlines, 1);
}

size_t compute_column(const uint8_t *buf, size_t offset) {
  /* This could be faster -- we could seek backwards from offset to
  the nearest newline, instead of forwards from 0. */
  size_t column = 0;
  for (size_t i = 0; i < offset; i++) {
    uint8_t ch = buf[i];
    if (ch == '\n') {
      column = 0;
    } else if (ch == '\t') {
      column = (column | 7) + 1;
    } else {
      column++;
    }
  }
  return column;
}
