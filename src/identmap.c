#include "identmap.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <inttypes.h>

#include "arith.h"
#include "util.h"

#define IDENTMAP_DBG(...) do { } while (0)

struct identmap_data {
  void *data;
  size_t data_count;
  void *user_value;
};

void identmap_init_personal(struct identmap *m) {
  m->table = NULL;
  m->count = 0;
  m->limit = 0;
  m->datas = NULL;
}

void identmap_init(struct identmap *m) {
  identmap_init_personal(m);
  arena_init(&m->string_arena);
}

void identmap_init_move(struct identmap *m, struct identmap *movee) {
  m->table = movee->table;
  m->count = movee->count;
  m->limit = movee->limit;
  m->datas = movee->datas;
  arena_init_move(&m->string_arena, &movee->string_arena);
  identmap_init_personal(movee);
}

void identmap_destroy(struct identmap *m) {
  free(m->table);
  for (size_t i = 0, e = m->count; i < e; i++) {
    m->datas[i].data = NULL;
    m->datas[i].data_count = 0;
    m->datas[i].user_value = NULL;
  }
  free(m->datas);
  arena_destroy(&m->string_arena);
  identmap_init_personal(m);
}

size_t identmap_hash(const void *buf, size_t count) {
  const uint8_t *p = (const uint8_t *)buf;
  size_t accum = 0x12345679;
  for (size_t i = 0; i < count; i++) {
    accum = (accum * 33) ^ p[i];
  }
  return accum;
}

void identmap_rebuild(struct identmap *m,
                      size_t new_limit) {
  IDENTMAP_DBG("identmap_rebuild, m->count=%"PRIz", new_limit=%"PRIz"\n",
               m->count, new_limit);
  /* The limit must always be a power of two. */
  CHECK(0 == (new_limit & (new_limit - 1)));
  CHECK(m->count < new_limit / 2);

  ident_value *new_table = malloc_mul(new_limit, sizeof(*new_table));

  IDENTMAP_DBG("identmap_rebuild, malloced new_table\n");
  for (size_t i = 0; i < new_limit; i++) {
    new_table[i] = IDENT_VALUE_INVALID;
  }

  IDENTMAP_DBG("identmap_rebuild initialized new buf\n");

  for (size_t i = 0; i < m->limit; i++) {
    ident_value id = m->table[i];
    if (id == IDENT_VALUE_INVALID) {
      continue;
    }

    size_t hash = identmap_hash(m->datas[id].data, m->datas[id].data_count);
    size_t offset = hash & (new_limit - 1);

    size_t step = 1;
    while (new_table[offset] != IDENT_VALUE_INVALID) {
      offset = size_add(offset, step) & (new_limit - 1);
      step++;
    }

    IDENTMAP_DBG("identmap_rebuild moving from index %"PRIz" to %"PRIz"\n",
                 i, offset);
    new_table[offset] = m->table[i];
  }

  m->datas = realloc(m->datas, size_mul(sizeof(*m->datas), new_limit / 2));
  CHECK(m->datas);

  free(m->table);
  m->table = new_table;
  m->limit = new_limit;
}

ident_value identmap_help_intern(struct identmap *m,
                                 const void *buf,
                                 size_t count,
                                 size_t *offset_out) {
  size_t limit = m->limit;
  IDENTMAP_DBG("identmap_intern count=%"PRIz", with limit %"PRIz"\n",
               count, limit);
  if (limit == 0) {
    identmap_rebuild(m, 8);
    limit = m->limit;
    IDENTMAP_DBG("identmap_intern rebuilt the map, its count and limit "
                 "are %"PRIz" and %"PRIz"\n",
                 m->count, m->limit);
  }
  size_t offset = identmap_hash(buf, count) & (limit - 1);
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

  *offset_out = offset;
  return IDENT_VALUE_INVALID;
}

int identmap_is_interned(struct identmap *m,
                         const void *buf,
                         size_t count,
                         ident_value *out) {
  size_t offset;
  ident_value v = identmap_help_intern(m, buf, count, &offset);
  *out = v;
  return v != IDENT_VALUE_INVALID;
}

ident_value identmap_intern(struct identmap *m,
                            const void *buf,
                            size_t count) {
  size_t offset;
  ident_value v = identmap_help_intern(m, buf, count, &offset);
  if (v != IDENT_VALUE_INVALID) {
    return v;
  }

  void *data_copy = arena_unaligned(&m->string_arena, count);
  memcpy(data_copy, buf, count);

  STATIC_CHECK(IDENT_VALUE_INVALID == SIZE_MAX);
  CHECK(m->count != IDENT_VALUE_INVALID);
  v = m->count;

  m->table[offset] = v;
  /* Since datas is of length m->limit / 2, and since m->count <
     m->limit / 2, we can write to m->datas[m->count]. */
  m->datas[m->count].data = data_copy;
  m->datas[m->count].data_count = count;
  m->datas[m->count].user_value = NULL;
  m->count++;

  if (m->count >= m->limit / 2) {
    IDENTMAP_DBG("identmap_intern rebuilding bigger map\n");
    identmap_rebuild(m, size_mul(m->limit, 2));
  }

  IDENTMAP_DBG("identmap_intern succeeded, value %" PRIident_value "\n", v);
  return v;
}

ident_value identmap_intern_c_str(struct identmap *m,
                                  const char *s) {
  return identmap_intern(m, s, strlen(s));
}

void identmap_lookup(struct identmap *m, ident_value ident,
                     const void **buf_out, size_t *count_out) {
  CHECK(ident != IDENT_VALUE_INVALID);
  CHECK(ident < m->count);
  *buf_out = m->datas[ident].data;
  *count_out = m->datas[ident].data_count;
}

void identmap_set_user_value(struct identmap *m, ident_value ident,
                             void *user_value) {
  CHECK(ident != IDENT_VALUE_INVALID);
  CHECK(ident < m->count);
  m->datas[ident].user_value = user_value;
}

void *identmap_get_user_value(struct identmap *m, ident_value ident) {
  CHECK(ident != IDENT_VALUE_INVALID);
  CHECK(ident < m->count);
  return m->datas[ident].user_value;
}
