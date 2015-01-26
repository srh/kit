#ifndef KIRA_ARITH_H_
#define KIRA_ARITH_H_

#include <stddef.h>
#include <stdint.h>

size_t size_mul(size_t x, size_t y);
size_t size_add(size_t x, size_t y);
size_t size_sub(size_t x, size_t y);

int32_t size_to_int32(size_t x);

int try_uint32_mul(uint32_t x, uint32_t y, uint32_t *out);
uint32_t uint32_mul(uint32_t x, uint32_t y);
int try_uint32_add(uint32_t x, uint32_t y, uint32_t *out);
uint32_t uint32_add(uint32_t x, uint32_t y);
int try_uint32_sub(uint32_t x, uint32_t y, uint32_t *out);
int try_uint32_div(uint32_t x, uint32_t y, uint32_t *out);
int try_uint32_mod(uint32_t x, uint32_t y, uint32_t *out);
uint32_t uint32_ceil_aligned(uint32_t x, uint32_t m);

uint32_t size_to_uint32(size_t x);

int try_int32_add(int32_t x, int32_t y, int32_t *out);
int try_int32_sub(int32_t x, int32_t y, int32_t *out);
int32_t int32_sub(int32_t x, int32_t y);
int try_int32_mul(int32_t x, int32_t y, int32_t *out);
int32_t int32_add(int32_t x, int32_t y);
int32_t int32_div(int32_t x, int32_t y);
int32_t int32_positive_mod(int32_t x, int32_t y);

int32_t uint32_to_int32(uint32_t x);

#endif /* KIRA_ARITH_H_ */
