#include "identmap.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <inttypes.h>

#include "arith.h"
#include "util.h"

#define IDENTMAP_DBG(...) do { } while (0)

struct ident_map_data {
  void *data;
  size_t data_count;
};

void ident_map_init(struct ident_map *m) {
  m->table = NULL;
  m->count = 0;
  m->limit = 0;
  m->datas = NULL;
}

void ident_map_init_move(struct ident_map *m, struct ident_map *movee) {
  *m = *movee;
  ident_map_init(movee);
}

void ident_map_destroy(struct ident_map *m) {
  free(m->table);
  for (size_t i = 0, e = m->count; i < e; i++) {
    free(m->datas[i].data);
    m->datas[i].data = NULL;
    m->datas[i].data_count = 0;
  }
  free(m->datas);
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
  IDENTMAP_DBG("ident_map_rebuild, m->count=%"PRIz", new_limit=%"PRIz"\n",
               m->count, new_limit);
  /* The limit must always be a power of two. */
  CHECK(0 == (new_limit & (new_limit - 1)));
  CHECK(m->count < new_limit / 2);

  ident_value *new_table = malloc_mul(new_limit, sizeof(*new_table));

  IDENTMAP_DBG("ident_map_rebuild, malloced new_table\n");
  for (size_t i = 0; i < new_limit; i++) {
    new_table[i] = IDENT_VALUE_INVALID;
  }

  IDENTMAP_DBG("ident_map_rebuild initialized new buf\n");

  for (size_t i = 0; i < m->limit; i++) {
    ident_value id = m->table[i];
    if (id == IDENT_VALUE_INVALID) {
      continue;
    }

    size_t hash = ident_map_hash(m->datas[id].data, m->datas[id].data_count);
    size_t offset = hash & (new_limit - 1);

    size_t step = 1;
    while (new_table[offset] != IDENT_VALUE_INVALID) {
      offset = size_add(offset, step) & (new_limit - 1);
      step++;
    }

    IDENTMAP_DBG("ident_map_rebuild moving from index %"PRIz" to %"PRIz"\n",
                 i, offset);
    new_table[offset] = m->table[i];
  }

  m->datas = realloc(m->datas, size_mul(sizeof(*m->datas), new_limit / 2));
  CHECK(m->datas);

  free(m->table);
  m->table = new_table;
  m->limit = new_limit;
}

ident_value ident_map_intern(struct ident_map *m,
                             const void *buf,
                             size_t count) {
  size_t limit = m->limit;
  IDENTMAP_DBG("ident_map_intern count=%"PRIz", with limit %"PRIz"\n",
               count, limit);
  if (limit == 0) {
    ident_map_rebuild(m, 8);
    limit = m->limit;
    IDENTMAP_DBG("ident_map_intern rebuilt the map, its count and limit "
                 "are %"PRIz" and %"PRIz"\n",
                 m->count, m->limit);
  }
  size_t offset = ident_map_hash(buf, count) & (limit - 1);
  size_t step = 1;
  ident_value v;
  while ((v = m->table[offset]), v != IDENT_VALUE_INVALID) {
    IDENTMAP_DBG("a collision at offset %"PRIz"\n", offset);
    if (m->datas[v].data_count == count
        && 0 == memcmp(m->datas[v].data, buf, count)) {
      return v;
    }

    offset = size_add(offset, step) & (limit - 1);
    step++;
  }

  void *data_copy = malloc_mul(count, 1);
  memcpy(data_copy, buf, count);

  STATIC_CHECK(IDENT_VALUE_INVALID == SIZE_MAX);
  CHECK(m->count != IDENT_VALUE_INVALID);
  v = m->count;

  m->table[offset] = v;
  /* Since datas is of length m->limit / 2, and since m->count <
     m->limit / 2, we can write to m->datas[m->count]. */
  m->datas[m->count].data = data_copy;
  m->datas[m->count].data_count = count;
  m->count++;

  if (m->count >= m->limit / 2) {
    IDENTMAP_DBG("ident_map_intern rebuilding bigger map\n");
    ident_map_rebuild(m, size_mul(limit, 2));
  }

  IDENTMAP_DBG("ident_map_intern succeeded, value %" PRIident_value "\n", v);
  return v;
}

ident_value ident_map_intern_c_str(struct ident_map *m,
                                   const char *s) {
  return ident_map_intern(m, s, strlen(s));
}

void ident_map_lookup(struct ident_map *m, ident_value ident,
                      const void **buf_out, size_t *count_out) {
  CHECK(ident != IDENT_VALUE_INVALID);
  CHECK(ident < m->count);
  *buf_out = m->datas[ident].data;
  *count_out = m->datas[ident].data_count;
}
