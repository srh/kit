#include "arith.h"

#include <stdint.h>

#include "util.h"

size_t size_mul(size_t x, size_t y) {
  CHECK(y == 0 || x <= SIZE_MAX / y);
  return x * y;
}

size_t size_add(size_t x, size_t y) {
  CHECK(x <= SIZE_MAX - y);
  return x + y;
}

size_t size_sub(size_t x, size_t y) {
  CHECK(x >= y);
  return x - y;
}
