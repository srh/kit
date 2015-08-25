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
  ok_memcpy(buf, lbuf, lcount);
  ok_memcpy(buf + lcount, rbuf, rcount);
  buf[count] = '\0';
  *buf_ptr_out = buf;
  *count_out = count;
}

void alloc_half_strcat(const void *lbuf, size_t lcount,
                       const char *r,
                       char **buf_ptr_out, size_t *count_out) {
  alloc_memcat(lbuf, lcount, r, strlen(r),
               buf_ptr_out, count_out);
}

void ok_memcpy(void *dest, const void *src, size_t n) {
  if (n == 0) {
    return;
  }
  memcpy(dest, src, n);
}

void write_le_u32(void *dest, uint32_t x) {
  uint8_t *d = dest;
  d[0] = x & 0xFF;
  d[1] = (x >> 8) & 0xFF;
  d[2] = (x >> 16) & 0xFF;
  d[3] = (x >> 24);
}

uint32_t read_le_u32(const void *src) {
  const uint8_t *s = src;
  uint32_t ret = s[0];
  ret += (((uint32_t)(s[1])) << 8);
  ret += (((uint32_t)(s[2])) << 16);
  ret += (((uint32_t)(s[3])) << 24);
  return ret;
}

void write_le_u16(void *dest, uint16_t x) {
  uint8_t *d = dest;
  d[0] = x & 0xFF;
  d[1] = (x >> 8) & 0xFF;
}

uint16_t read_le_u16(const void *src) {
  const uint8_t *s = src;
  uint16_t ret = s[0];
  ret += (((uint32_t)(s[1])) << 8);
  return ret;
}

struct le_u32 to_le_u32(uint32_t x) {
  struct le_u32 ret;
  write_le_u32(ret.bytes, x);
  return ret;
}
uint32_t from_le_u32(struct le_u32 x) {
  return read_le_u32(x.bytes);
}

struct le_u16 to_le_u16(uint16_t x) {
  struct le_u16 ret;
  write_le_u16(ret.bytes, x);
  return ret;
}
uint16_t from_le_u16(struct le_u16 x) {
  return read_le_u16(x.bytes);
}
