#ifndef KIRA_ARITH_H_
#define KIRA_ARITH_H_

#include <stddef.h>
#include <stdint.h>

size_t size_mul(size_t x, size_t y);
size_t size_add(size_t x, size_t y);
size_t size_sub(size_t x, size_t y);

uint32_t uint32_mul(uint32_t x, uint32_t y);
uint32_t uint32_add(uint32_t x, uint32_t y);
uint32_t uint32_ceil_aligned(uint32_t x, uint32_t m);

uint32_t size_to_uint32(size_t x);


#endif /* KIRA_ARITH_H_ */
