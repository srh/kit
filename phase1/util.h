#ifndef KIT_UTIL_H_
#define KIT_UTIL_H_

#include <stddef.h>
#include <stdint.h>

#ifdef _WIN32
#define PRIz "Iu"
#define NORETURN __declspec(noreturn)
#define ALIGNOF(x) __alignof(x)
#define PACK_PUSH __pragma(pack(push, 1))
#define PACK_POP __pragma(pack(pop))
#define PACK_ATTRIBUTE
#else /* _WIN32 */
#define PRIz "zu"
#define NORETURN __attribute__((__noreturn__))
#define ALIGNOF(x) __alignof__(x)
#define PACK_PUSH
#define PACK_POP
#define PACK_ATTRIBUTE __attribute__((__packed__))
#endif /* _WIN32 */

#define STATIC_CHECK(x) do { enum { assertion = 1/!!(x) }; } while (0)


NORETURN void report_and_abort(const char *file, int line,
                               const char *msg1, const char *msg2);

#define CHECK(x) do { \
    if (!(x)) { \
      report_and_abort(__FILE__, __LINE__, "CHECK failed: ", #x); \
    } \
  } while (0)

#define CRASH(msg) do { \
    report_and_abort(__FILE__, __LINE__, "CRASH: ", msg); \
  } while (0)

#define UNREACHABLE() CRASH("Unreachable.")

#define DBG(...) do { \
    fprintf(stderr, __VA_ARGS__); \
    fflush(stderr); \
  } while (0)

#define TODO_IMPLEMENT CRASH("Unimplemented.")

/* TODO: Every use of ERR_DBG is a bad error message.  And this is in
the wrong place. */
#define ERR_DBG(...) DBG(__VA_ARGS__)

/* Uses of ERR are better error messages than what we had before. */
#define ERR(...) DBG(__VA_ARGS__)

/* Never returns a null pointer.  (Checks if a * b overflows, and
   calls malloc(1) instead of malloc(0).) */
void *malloc_mul(size_t a, size_t b);

/* Returns a pointer to a buffer of size *count_out + 1... so that
there's a null terminator. */
void alloc_memcat(const void *lbuf, size_t lcount,
                  const void *rbuf, size_t rcount,
                  char **buf_ptr_out, size_t *count_out);

/* r is a null-terminated string. */
void alloc_half_strcat(const void *lbuf, size_t lcount,
                       const char *r,
                       char **buf_ptr_out, size_t *count_out);

/* memcpy, only if n == 0 and dest or src is NULL, you don't have
undefined behavior. */
void ok_memcpy(void *dest, const void *src, size_t n);

/* Writes/reads a uint64_t in little-endian. */
void write_le_u64(void *dest, uint64_t x);
uint64_t read_le_u64(const void *src);

#define write_le_i64(dest, x) write_le_u64((dest), (uint64_t)(x))

/* Writes/reads a uint32_t in little-endian. */
void write_le_u32(void *dest, uint32_t x);
uint32_t read_le_u32(const void *src);

#define write_le_i32(dest, x) write_le_u32((dest), (uint32_t)(x))

/* Writes/reads a uint16_t in little-endian. */
void write_le_u16(void *dest, uint16_t x);
uint16_t read_le_u16(const void *src);

#define write_le_i16(dest, x) write_le_u16((dest), (uint32_t)(x));

struct le_u64 {
  char bytes[8];
};

struct le_u64 to_le_u64(uint64_t x);
uint64_t from_le_u64(struct le_u64 x);

struct le_i64 {
  char bytes[8];
};

struct le_i64 to_le_i64(int64_t x);
int64_t from_le_i64(struct le_i64 x);

struct le_u32 {
  char bytes[4];
};

struct le_u32 to_le_u32(uint32_t x);
uint32_t from_le_u32(struct le_u32 x);

/* This is always two's complement (of course). */
struct le_i32 {
  char bytes[4];
};

struct le_i32 to_le_i32(int32_t x);
int32_t from_le_i32(struct le_i32 x);

struct le_u16 {
  char bytes[2];
};

struct le_u16 to_le_u16(uint16_t x);
uint16_t from_le_u16(struct le_u16 x);

/* This is always two's complement (of course). */
struct le_i16 {
  char bytes[2];
};

struct le_i16 to_le_i16(int16_t x);
int16_t from_le_i16(struct le_i16 x);


#endif /* KIT_UTIL_H_ */
