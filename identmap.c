#include "identmap.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <inttypes.h>

#include "arith.h"
#include "util.h"

#define IDENTMAP_DBG(...) do { } while (0)

struct ident_map_entry {
  ident_value ident;
  /* The offset into m->strings. */
  size_t strings_offset;
  size_t count;
};

void ident_map_init(struct ident_map *m) {
  m->table = NULL;
  m->count = 0;
  m->limit = 0;
  m->prev_value = 0;
  m->strings = NULL;
  m->strings_size = 0;
  m->strings_limit = 0;
}

void ident_map_init_move(struct ident_map *m, struct ident_map *movee) {
  *m = *movee;
  ident_map_init(movee);
}

void ident_map_destroy(struct ident_map *m) {
  free(m->table);
  free(m->strings);
  ident_map_init(m);
}

size_t ident_map_hash(const void *buf, size_t count) {
  const uint8_t *p = (const uint8_t *)buf;
  size_t accum = 0x12345679;
  for (size_t i = 0; i < count; i++) {
    accum = (accum * 33) ^ p[i];
  }
  return accum;
}

void ident_map_rebuild(struct ident_map *m,
		       size_t new_limit) {
  IDENTMAP_DBG("ident_map_rebuild, new_limit=%"PRIz"\n", new_limit);
  /* The limit must always be a power of two. */
  CHECK(0 == (new_limit & (new_limit - 1)));
  CHECK(m->count < new_limit / 2);

  IDENTMAP_DBG("ident_map_rebuild, survived checks\n");
  size_t malloc_size = size_mul(new_limit, sizeof(struct ident_map_entry));
  IDENTMAP_DBG("computed malloc_size of %"PRIz"\n", malloc_size);
  struct ident_map_entry *new_table = malloc(malloc_size);
  IDENTMAP_DBG("malloc returned\n");
  CHECK(new_table);

  IDENTMAP_DBG("ident_map_rebuild, malloced new_table\n");
  for (size_t i = 0; i < new_limit; i++) {
    new_table[i].ident = IDENT_VALUE_INVALID;
    new_table[i].strings_offset = 0;
    new_table[i].count = 0;
  }

  IDENTMAP_DBG("ident_map_rebuild initialized new buf\n");

  for (size_t i = 0; i < m->limit; i++) {
    if (m->table[i].ident == IDENT_VALUE_INVALID) {
      continue;
    }

    size_t hash = ident_map_hash(m->strings + m->table[i].strings_offset,
				 m->table[i].count);
    size_t offset = hash & (new_limit - 1);

    size_t step = 1;
    while (new_table[offset].ident != IDENT_VALUE_INVALID) {
      offset = size_add(offset, step) & (new_limit - 1);
      step++;
    }

    IDENTMAP_DBG("ident_map_rebuild moving from index %"PRIz" to %"PRIz"\n", i, offset);
    new_table[offset] = m->table[i];
  }

  free(m->table);
  m->table = new_table;
  m->limit = new_limit;
}

/* Returns the added string's offset into m->strings. */
size_t ident_map_add_string(struct ident_map *m, const void *buf,
			    size_t count) {
  IDENTMAP_DBG("ident_map_add_string\n");
  size_t new_size = size_add(m->strings_size, count);
  if (new_size > m->strings_limit) {
    size_t new_limit = m->strings_limit;
    do {
      new_limit = new_limit ? size_mul(new_limit, 2) : 32;
    } while (new_size > new_limit);

    char *new_strings = realloc(m->strings, new_limit);
    CHECK(new_strings);
    m->strings = new_strings;
    m->strings_limit = new_limit;
  }

  STATIC_CHECK(sizeof(uint8_t) == sizeof(char));
  memcpy(m->strings + m->strings_size, buf, count);
  size_t ret = m->strings_size;
  m->strings_size = new_size;
  CHECK(m->strings_size <= m->strings_limit);
  IDENTMAP_DBG("ident_map_add_string returning\n");
  return ret;
}

ident_value ident_map_intern(struct ident_map *m,
			     const void *buf,
			     size_t count) {
  size_t limit = m->limit;
  IDENTMAP_DBG("ident_map_intern count=%"PRIz", with limit %"PRIz"\n", count, limit);
  if (limit == 0) {
    ident_map_rebuild(m, 8);
    limit = m->limit;
    IDENTMAP_DBG("ident_map_intern rebuilt the map, its count and limit are %"PRIz" and %"PRIz"\n",
		 m->count, m->limit);
  }
  size_t offset = ident_map_hash(buf, count) & (limit - 1);
  size_t step = 1;
  ident_value v;
  while ((v = m->table[offset].ident), v != IDENT_VALUE_INVALID) {
    IDENTMAP_DBG("a collision at offset %"PRIz"\n", offset);
    if (m->table[offset].count == count
	&& 0 == memcmp(m->strings + m->table[offset].strings_offset, buf, count)) {
      return v;
    }

    offset = size_add(offset, step) & (limit - 1);
    step++;
  }

  size_t strings_offset = ident_map_add_string(m, buf, count);
  CHECK(m->prev_value != IDENT_VALUE_MAX);
  m->prev_value++;
  v = m->prev_value;
  m->table[offset].ident = v;
  m->table[offset].strings_offset = strings_offset;
  m->table[offset].count = count;
  m->count++;

  if (count >= m->limit / 2) {
    IDENTMAP_DBG("ident_map_intern rebuilding bigger map\n");
    ident_map_rebuild(m, size_mul(limit, 2));
  }

  IDENTMAP_DBG("ident_map_intern succeeded, value %" PRIident_value "\n", v);
  return v;
}

void ident_map_lookup(struct ident_map *m, ident_value ident,
		      const void **buf_out, size_t *count_out) {
  CHECK(ident != IDENT_VALUE_INVALID);
  /* TODO: We need a more efficient reverse lookup process. */
  for (size_t i = 0, e = m->limit; i < e; i++) {
    if (m->table[i].ident == ident) {
      *buf_out = m->strings + m->table[i].strings_offset;
      *count_out = m->table[i].count;
      return;
    }
  }
  CRASH("ident_map missing ident_value\n");
}
