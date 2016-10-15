#ifndef KIT_SLICE_H_
#define KIT_SLICE_H_

#include <stdlib.h>

#include "arith.h"
#include "util.h"

/* Generates a slice type and utility func declarations. */
#define GEN_SLICE_HDR(name, typ) \
  struct name##_slice { \
    typ *ptr; \
    size_t count; \
    size_t limit; \
  }; \
  struct name##_slice name##_slice_initializer(void); \
  void name##_slice_push(struct name##_slice *slice, typ value); \
  void name##_slice_pop(struct name##_slice *slice, void (*destructor)(typ *)); \
  void name##_slice_destroy(struct name##_slice *slice, void (*destructor)(typ *)); \
  void name##_slice_destroy_prim(struct name##_slice *slice); \
  typedef int GEN_SLICE_HDR_##name##_force_semicolon

/* Generates utility func implementations for a corresponding GEN_SLICE_HDR. */
#define GEN_SLICE_IMPL(name, typ) \
  struct name##_slice name##_slice_initializer(void) { \
    struct name##_slice ret = SLICE_INITIALIZER; \
    return ret; \
  } \
  void name##_slice_push(struct name##_slice *slice, typ value) { \
    size_t count = slice->count; \
    size_t limit = slice->limit; \
    CHECK(count <= limit); \
    if (count == limit) { \
      size_t new_limit = limit ? size_mul(limit, 2) : 4; \
      slice->ptr = realloc(slice->ptr, size_mul(sizeof(*slice->ptr), new_limit)); \
      CHECK(slice->ptr); \
      slice->limit = new_limit; \
    } \
    slice->ptr[count] = value; \
    slice->count = count + 1; \
  } \
  void name##_slice_pop(struct name##_slice *slice, void (*destructor)(typ *)) { \
    CHECK(slice->count > 0); \
    (*destructor)(&slice->ptr[--slice->count]); \
  } \
  void name##_slice_destroy(struct name##_slice *slice, void (*destructor)(typ *)) { \
    SLICE_FREE(slice->ptr, slice->count, *destructor); \
    slice->limit = 0; \
  } \
  void name##_slice_destroy_prim(struct name##_slice *slice) { \
    free(slice->ptr); \
    slice->ptr = NULL; \
    slice->count = 0; \
    slice->limit = 0; \
  } \
  typedef int GEN_SLICE_IMPL_##name##_force_semicolon

#define SLICE_INITIALIZER {NULL, 0, 0}

GEN_SLICE_HDR(int8, int8_t);
GEN_SLICE_HDR(uint8, uint8_t);

#define SLICE_FREE(ptr, count, destructor) do { \
    for (size_t SLICE_FREE_i = (count); SLICE_FREE_i-- > 0; ) { \
      (destructor)(&(ptr)[SLICE_FREE_i]); \
    } \
    free(ptr); \
    (ptr) = NULL; \
    (count) = 0; \
  } while (0)

#define SLICE_INIT_COPY(dest_ptr, dest_count, src_ptr, src_count, copier) do { \
    size_t SLICE_INIT_COPY_count = (src_count); \
    (dest_ptr) = malloc(size_mul(sizeof(*(dest_ptr)), SLICE_INIT_COPY_count)); \
    CHECK(dest_ptr); \
    (dest_count) = SLICE_INIT_COPY_count; \
    for (size_t SLICE_INIT_COPY_i = 0; \
         SLICE_INIT_COPY_i < SLICE_INIT_COPY_count; \
         SLICE_INIT_COPY_i++) { \
      (copier)(&(dest_ptr)[SLICE_INIT_COPY_i], &(src_ptr)[SLICE_INIT_COPY_i]); \
    } \
  } while (0)

#define SLICE_INIT_COPY_PRIM(dest_ptr, dest_count, src_ptr, src_count) do { \
    size_t SLICE_INIT_COPY_count = (src_count); \
    (dest_ptr) = malloc(size_mul(sizeof(*(dest_ptr)), SLICE_INIT_COPY_count)); \
    CHECK(dest_ptr); \
    (dest_count) = SLICE_INIT_COPY_count; \
    for (size_t SLICE_INIT_COPY_i = 0; \
         SLICE_INIT_COPY_i < SLICE_INIT_COPY_count; \
         SLICE_INIT_COPY_i++) { \
      (dest_ptr)[SLICE_INIT_COPY_i] = (src_ptr)[SLICE_INIT_COPY_i]; \
    } \
  } while (0)

#endif /* KIT_SLICE_H_ */
