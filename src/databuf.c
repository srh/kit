#include "databuf.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "arith.h"
#include "util.h"

void databuf_init(struct databuf *b) {
  b->buf = NULL;
  b->count = 0;
  b->limit = 0;
}

void databuf_destroy(struct databuf *b) {
  free(b->buf);
  databuf_init(b);
}

void databuf_move_destroy(struct databuf *b,
                          void **buf_out, size_t *count_out) {
  void *buf = realloc(b->buf, b->count);
  CHECK(buf || b->count == 0);
  *buf_out = buf;
  *count_out = b->count;
  databuf_init(b);
}

void databuf_grow(struct databuf *b, size_t accomodated_count) {
  size_t limit = b->limit;
  while (limit < accomodated_count) {
    limit = limit ? size_mul(limit, 2) : 64;
  }
  b->buf = realloc(b->buf, limit);
  CHECK(b->buf || limit == 0);
  b->limit = limit;
}

void databuf_append(struct databuf *b, const void *p, size_t count) {
  if (b->limit - b->count < count) {
    databuf_grow(b, size_add(b->count, count));
  }
  ok_memcpy(b->buf + b->count, p, count);
  b->count = size_add(b->count, count);
}

void databuf_overwrite(struct databuf *b, size_t offset,
                       const void *p, size_t count) {
  CHECK(size_add(offset, count) <= b->count);
  ok_memcpy(b->buf + offset, p, count);
}
