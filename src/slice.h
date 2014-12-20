#ifndef KIRA_SLICE_H_
#define KIRA_SLICE_H_

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

#define SLICE_FREE(ptr, count, destructor) do { \
    for (size_t SLICE_FREE_i = (count); SLICE_FREE_i-- > 0; ) { \
      (destructor)(&(ptr)[SLICE_FREE_i]); \
    } \
    free(ptr); \
    (ptr) = NULL; \
    (count) = 0; \
  } while (0)

#endif /* KIRA_SLICE_H_ */
