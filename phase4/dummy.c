#include <stdio.h>
#include <inttypes.h>

FILE *Stdout() {
  return stdout;
}
FILE *Stdin() {
  return stdin;
}
FILE *Stderr() {
  return stderr;
}

int32_t __kit_x86_i64_mul(int64_t x, int64_t y, int64_t *out) {
  /* This assumes x86 division behavior because it's an x86-specific
  implementation. */
  /* This is a brute, stupid, slow, implementation. */
  if (x > 0) {
    if (y >= 0) {
      if (y > INT64_MAX / x) {
        return 1;
      }
    } else {
      if (y < INT64_MIN / x) {
        return 1;
      }
    }
  } else if (x < 0) {
    if (y > 0) {
      if (x < INT64_MIN / y) {
        return 1;
      }
    } else if (y < 0) {
      if (x == INT64_MIN) {
        return 1;
      }
      if (-x > INT64_MIN / y) {
        return 1;
      }
    }
  }
  *out = x * y;
  return 0;
}

/* These are supposed to trap on divide by zero or INT64_MIN / -1 (which they do). */
int64_t __kit_x86_i64_div(int64_t numer, int64_t denom) {
  return numer / denom;
}

int64_t __kit_x86_i64_mod(int64_t numer, int64_t denom) {
  return numer % denom;
}

uint64_t __kit_x86_u64_div(uint64_t numer, uint64_t denom) {
  return numer / denom;
}

uint64_t __kit_x86_u64_mod(uint64_t numer, uint64_t denom) {
  return numer % denom;
}
