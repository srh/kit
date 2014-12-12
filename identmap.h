#ifndef KIRA_IDENTMAP_H_
#define KIRA_IDENTMAP_H_

#include <stddef.h>

#include "util.h"

struct ident_map_entry;

typedef size_t ident_value;
#define IDENT_VALUE_MAX SIZE_MAX
#define IDENT_VALUE_INVALID 0
#define PRIident_value PRIz

struct ident_map {
  struct ident_map_entry *table;
  /* count is less than limit / 2, unless limit is 0. */
  size_t count;
  /* limit is either 0 or a power of 2 that is at least 8. */
  size_t limit;
  /* prev_value is initially zero, which means no ident can be zero. */
  ident_value prev_value;

  /* Holds all the identifier values, concatenated. */
  char *strings;
  size_t strings_size;
  size_t strings_limit;
};

void ident_map_init(struct ident_map *m);
void ident_map_init_move(struct ident_map *m, struct ident_map *movee);
void ident_map_destroy(struct ident_map *m);

ident_value ident_map_intern(struct ident_map *m,
			     const void *buf,
			     size_t count);



#endif /* KIRA_IDENTMAP_H_ */
