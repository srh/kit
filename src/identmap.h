#ifndef KIRA_IDENTMAP_H_
#define KIRA_IDENTMAP_H_

#include <stddef.h>

#include "util.h"

struct identmap_data;

typedef size_t ident_value;
#define IDENT_VALUE_INVALID SIZE_MAX
#define PRIident_value PRIz

struct identmap {
  ident_value *table;
  /* count is less than limit / 2, unless limit is 0. */
  size_t count;
  /* limit is either 0 or a power of 2 that is at least 8. */
  size_t limit;

  /* datas is of length limit / 2. */
  struct identmap_data *datas;
};

void identmap_init(struct identmap *m);
void identmap_init_move(struct identmap *m, struct identmap *movee);
void identmap_destroy(struct identmap *m);

ident_value identmap_intern(struct identmap *m,
                            const void *buf,
                            size_t count);

ident_value identmap_intern_c_str(struct identmap *m,
                                  const char *s);

void identmap_lookup(struct identmap *m, ident_value ident,
                     const void **buf_out, size_t *count_out);

void identmap_set_user_value(struct identmap *m, ident_value ident,
                             void *user_value);
void *identmap_get_user_value(struct identmap *m, ident_value ident);


#endif /* KIRA_IDENTMAP_H_ */
