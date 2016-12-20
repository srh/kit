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

void write_le_u64(void *dest, uint64_t x) {
  uint8_t *d = dest;
  d[0] = x & 0xFF;
  d[1] = (x >> 8) & 0xFF;
  d[2] = (x >> 16) & 0xFF;
  d[3] = (x >> 24) & 0xFF;
  d[4] = (x >> 32) & 0xFF;
  d[5] = (x >> 40) & 0xFF;
  d[6] = (x >> 48) & 0xFF;
  d[7] = (x >> 56);
}

uint64_t read_le_u64(const void *src) {
  const uint8_t *s = src;
  uint64_t ret = s[0];
  ret += (((uint64_t)(s[1])) << 8);
  ret += (((uint64_t)(s[2])) << 16);
  ret += (((uint64_t)(s[3])) << 24);
  ret += (((uint64_t)(s[4])) << 32);
  ret += (((uint64_t)(s[5])) << 40);
  ret += (((uint64_t)(s[6])) << 48);
  ret += (((uint64_t)(s[7])) << 56);
  return ret;
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

struct le_u64 to_le_u64(uint64_t x) {
  struct le_u64 ret;
  write_le_u64(ret.bytes, x);
  return ret;
}
uint64_t from_le_u64(struct le_u64 x) {
  return read_le_u64(x.bytes);
}

struct le_i64 to_le_i64(int64_t x) {
  /* We convert to uint64_t, adding 2**64 to x if it's negative. */
  uint64_t xu = x;
  struct le_i64 ret;
  write_le_u64(ret.bytes, xu);
  return ret;
}
int64_t from_le_i64(struct le_i64 x) {
  /* We can't just convert unsigned->signed, it's
  implementation-defined. */
  uint64_t xu = read_le_u64(x.bytes);
  if (xu > INT64_MAX) {
    /* We subtract 2**64 from the numerical value, by subtracting (1 +
    INT64_MAX) once and (1 + INT64_MAX / 2) twice. */
    uint64_t s = 1 + (uint64_t)INT64_MAX;
    /* First, xu > INT64_MAX, so we can subtract without underflow. */
    xu -= s;
    /* Now 0 <= xu <= INT64_MAX, we can safely convert. */
    int64_t xi = (int64_t)xu;

    int64_t hs = 1 + INT64_MAX / 2;
    /* Now, subtract (1 + INT64_MAX / 2) twice. */
    xi -= hs;
    xi -= hs;
    return xi;
  } else {
    /* xu <= INT64_MAX, we can safely convert. */
    int64_t xi = (int32_t)xu;
    return xi;
  }
}

struct le_u32 to_le_u32(uint32_t x) {
  struct le_u32 ret;
  write_le_u32(ret.bytes, x);
  return ret;
}
uint32_t from_le_u32(struct le_u32 x) {
  return read_le_u32(x.bytes);
}

struct le_i32 to_le_i32(int32_t x) {
  /* We convert to uint32_t, adding 2**32 to x if it's negative. */
  uint32_t xu = x;
  struct le_i32 ret;
  write_le_u32(ret.bytes, xu);
  return ret;
}
int32_t from_le_i32(struct le_i32 x) {
  /* We can't just convert unsigned->signed, it's
  implementation-defined.  We use int64_t to safely subtract 2**32. */
  uint32_t retu = read_le_u32(x.bytes);
  int64_t reti64 = retu;
  if (reti64 > INT32_MAX) {
    reti64 -= 1 + (int64_t)UINT32_MAX;
  }
  int32_t reti32 = (int32_t)reti64;
  return reti32;
}

struct le_u16 to_le_u16(uint16_t x) {
  struct le_u16 ret;
  write_le_u16(ret.bytes, x);
  return ret;
}
uint16_t from_le_u16(struct le_u16 x) {
  return read_le_u16(x.bytes);
}

struct le_i16 to_le_i16(int16_t x) {
  /* We convert to uint64_t, adding 2**16 to x if it's negative. */
  uint16_t xu = x;
  struct le_i16 ret;
  write_le_u16(ret.bytes, xu);
  return ret;
}

int16_t from_le_i16(struct le_i16 x) {
  /* We can't convert unsigned->signed, just like in from_le_i32. */
  uint16_t retu = read_le_u16(x.bytes);
  int32_t reti32 = retu;
  if (reti32 > INT16_MAX) {
    reti32 -= 1 + (int32_t)UINT16_MAX;
  }
  int16_t reti16 = (int16_t)reti32;
  return reti16;
}
