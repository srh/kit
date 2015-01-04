#include "util.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "arith.h"

void report_and_abort(const char *file, int line,
                      const char *msg1, const char *msg2) {
  fprintf(stderr, "Fatal error at %s:%d: %s%s\n",
          file, line, msg1, msg2);
  fflush(stderr);
  abort();
}

void *malloc_mul(size_t a, size_t b) {
  size_t n = size_mul(a, b);
  void *ret = malloc(n ? n : 1);
  CHECK(ret);
  return ret;
}

void alloc_memcat(const void *lbuf, size_t lcount,
                  const void *rbuf, size_t rcount,
                  char **buf_ptr_out, size_t *count_out) {
  size_t count = size_add(lcount, rcount);
  char *buf = malloc_mul(1, size_add(1, count));
  memcpy(buf, lbuf, lcount);
  memcpy(buf + lcount, rbuf, rcount);
  buf[lcount + rcount] = '\0';
  *buf_ptr_out = buf;
  *count_out = count;
}

void alloc_half_strcat(const void *lbuf, size_t lcount,
                       const char *r,
                       char **buf_ptr_out, size_t *count_out) {
  alloc_memcat(lbuf, lcount, r, strlen(r),
               buf_ptr_out, count_out);
}
