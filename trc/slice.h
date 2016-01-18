#ifndef KIT_SLICE_H_
#define KIT_SLICE_H_

#include <stdlib.h>

#include "arith.h"
#include "util.h"

#define SLICE_PUSH(ptr, count, limit, value) do { \
    CHECK((count) <= (limit)); \
    if ((count) == (limit)) { \
      size_t SLICE_PUSH_new_limit = (limit) ? size_mul((limit), 2) : 4; \
      (ptr) = realloc((ptr), size_mul(sizeof(*ptr), SLICE_PUSH_new_limit)); \
      CHECK(ptr); \
      (limit) = SLICE_PUSH_new_limit; \
    } \
    (ptr)[(count)++] = (value); \
  } while (0)

#define SLICE_POP(ptr, count, destructor) do { \
    CHECK((count) > 0); \
    (destructor)(&(ptr)[--(count)]); \
  } while (0)

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
