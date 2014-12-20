#include "util.h"

#include <stdio.h>
#include <stdlib.h>

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
  void *ret = malloc(n);
  CHECK(ret || n == 0);
  return ret;
}
