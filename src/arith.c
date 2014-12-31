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

uint32_t uint32_mul(uint32_t x, uint32_t y) {
  CHECK(y == 0 || x <= UINT32_MAX / y);
  return x * y;
}

uint32_t uint32_add(uint32_t x, uint32_t y) {
  CHECK(x <= UINT32_MAX - y);
  return x + y;
}

uint32_t uint32_ceil_aligned(uint32_t x, uint32_t m) {
  CHECK(m != 0);
  uint32_t y = x % m;
  if (y == 0) {
    return x;
  } else {
    return uint32_add(x, m - y);
  }
}

uint32_t size_to_uint32(size_t x) {
  CHECK(x <= UINT32_MAX);
  return x;
}
