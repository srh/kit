#ifndef KIT_SLICE_H_
#define KIT_SLICE_H_

#include <stdlib.h>

#include "arith.h"
#include "util.h"

/* Generates a "slice" type (growable array) and utility func declarations. */
#define GEN_SLICE_HDR(name, typ) \
  struct name##_slice { \
    typ *ptr; \
    size_t count; \
    size_t limit; \
  }; \
  struct name##_slice name##_slice_initializer(void); \
  void name##_slice_push(struct name##_slice *slice, typ value); \
  void name##_slice_pop(struct name##_slice *slice); \
  void name##_slice_destroy(struct name##_slice *slice); \
  typedef int GEN_SLICE_HDR_##name##_force_semicolon

/* Generates utility func implementations for a corresponding GEN_SLICE_HDR. */
#define GEN_SLICE_IMPL_COMMON(name, typ) \
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
  typedef int GEN_SLICE_IMPL_COMMON_##name##_force_semicolon

#define GEN_SLICE_IMPL_PRIM(name, typ) \
  GEN_SLICE_IMPL_COMMON(name, typ); \
  void name##_slice_pop(struct name##_slice *slice) { \
    CHECK(slice->count > 0); \
    --slice->count; \
  } \
  void name##_slice_destroy(struct name##_slice *slice) { \
    free(slice->ptr); \
    slice->ptr = NULL; \
    slice->count = 0; \
    slice->limit = 0; \
  } \
  typedef int GEN_SLICE_IMPL_##name##_force_semicolon

#define GEN_SLICE_IMPL(name, typ, dtor) \
  GEN_SLICE_IMPL_COMMON(name, typ); \
  void name##_slice_pop(struct name##_slice *slice) { \
    CHECK(slice->count > 0); \
    dtor(&slice->ptr[--slice->count]); \
  } \
  void name##_slice_destroy(struct name##_slice *slice) { \
    SLICE_FREE(slice->ptr, slice->count, dtor); \
    slice->limit = 0; \
  } \
  typedef int GEN_SLICE_IMPL_##name##_force_semicolon

#define SLICE_INITIALIZER {NULL, 0, 0}

/* Generates non-growable "array" type and utility func declarations. */
#define GEN_ARRAY_HDR(name, typ) \
  struct name##_array { \
    typ *ptr; \
    size_t count; \
    size_t limit; \
  }; \
  struct name##_array name##_array_make(typ *ptr, size_t count); \
  struct name##_array name##_array_malloc(size_t count); \
  void name##_array_init_copy(struct name##_array *a, \
                              struct name##_array *c); \
  void name##_array_destroy(struct name##_array *arr); \
  void name##_array_destroy_prim(struct name##_array *arr); \
  typedef int GEN_ARRAY_HDR_##name##_force_semicolon

/* Generates utility func implementations for a corresponding GEN_ARRAY_HDR. */
#define GEN_ARRAY_IMPL_COMMON(name, typ) \
  struct name##_array name##_array_make(typ *ptr, size_t count) { \
    struct name##_array ret; \
    ret.ptr = ptr; \
    ret.count = count; \
    return ret; \
  } \
  struct name##_array name##_array_malloc(size_t count) { \
    return name##_array_make(malloc_mul(sizeof(typ), count), count);  \
  } \
  typedef int GEN_ARRAY_IMPL_COMMON_##name##_force_semicolon

#define GEN_ARRAY_IMPL(name, typ, dtor) \
  GEN_ARRAY_IMPL_COMMON(name, typ); \
  void name##_array_destroy(struct name##_array *arr) { \
    SLICE_FREE(arr->ptr, arr->count, dtor); \
  } \
  typedef int GEN_ARRAY_IMPL_##name##_force_semicolon

#define GEN_ARRAY_IMPL_PRIM(name, typ) \
  GEN_ARRAY_IMPL_COMMON(name, typ); \
  void name##_array_destroy(struct name##_array *arr) { \
    free(arr->ptr); \
    arr->ptr = NULL; \
    arr->count = 0; \
  } \
  typedef int GEN_ARRAY_IMPL_##name##_force_semicolon

/* For types for which we use both slices, define both with a conversion. */
#define GEN_SLICE_AND_ARRAY_HDR(name, typ) \
  GEN_SLICE_HDR(name, typ); \
  GEN_ARRAY_HDR(name, typ); \
  struct name##_array name##_array_from_slice(struct name##_slice *slice); \
  typedef int GEN_SLICE_AND_ARRAY_HDR_##name##_force_semicolon

#define GEN_SLICE_AND_ARRAY_IMPL_PRIM(name, typ) \
  GEN_SLICE_IMPL_PRIM(name, typ); \
  GEN_ARRAY_IMPL_PRIM(name, typ); \
  struct name##_array name##_array_from_slice(struct name##_slice *slice) { \
    struct name##_array ret = name##_array_make(slice->ptr, slice->count); \
    slice->ptr = NULL; \
    slice->count = 0; \
    slice->limit = 0; \
    return ret; \
  } \
  typedef int GEN_SLICE_AND_ARRAY_IMPL_##name##_force_semicolon

#define GEN_SLICE_AND_ARRAY_IMPL(name, typ, dtor) \
  GEN_SLICE_IMPL(name, typ, dtor); \
  GEN_ARRAY_IMPL(name, typ, dtor); \
  struct name##_array name##_array_from_slice(struct name##_slice *slice) { \
    struct name##_array ret = name##_array_make(slice->ptr, slice->count); \
    slice->ptr = NULL; \
    slice->count = 0; \
    slice->limit = 0; \
    return ret; \
  } \
  typedef int GEN_SLICE_AND_ARRAY_IMPL_##name##_force_semicolon


GEN_SLICE_AND_ARRAY_HDR(int8, int8_t);
GEN_SLICE_AND_ARRAY_HDR(uint8, uint8_t);


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
