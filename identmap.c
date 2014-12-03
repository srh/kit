#include "identmap.h"

#include <stdlib.h>
#include <string.h>

#include "arith.h"
#include "util.h"

struct ident_map_entry {
  ident_value ident;
  const uint8_t *buf;
  size_t count;
};

void ident_map_init(struct ident_map *m) {
  m->table = NULL;
  m->count = 0;
  m->limit = 0;
  m->prev_value = 0;
}

void ident_map_destroy(struct ident_map *m) {
  free(m->table);
  ident_map_init(m);
}

size_t ident_map_hash(const uint8_t *buf, size_t count) {
  size_t accum = 0x12345679;
  for (size_t i = 0; i < count; i++) {
    accum = (accum * 33) ^ buf[i];
  }
  return accum;
}

void ident_map_rebuild(struct ident_map *m,
		       size_t new_limit) {
  /* The limit must always be a power of two. */
  CHECK(0 == (new_limit & (new_limit - 1)));
  CHECK(m->count < new_limit / 2);

  struct ident_map_entry *new_table = malloc(size_mul(new_limit, sizeof(*new_table)));
  CHECK(new_table);

  for (size_t i = 0; i < new_limit; i++) {
    new_table[i].ident = 0;
    new_table[i].buf = NULL;
    new_table[i].count = 0;
  }

  for (size_t i = 0; i < m->limit; i++) {
    if (!m->table[i].ident) {
      continue;
    }

    size_t offset = ident_map_hash(m->table[i].buf, m->table[i].count) & (new_limit - 1);
    size_t step = 1;
    while (new_table[offset].ident) {
      offset = size_add(offset, step) & (new_limit - 1);
      step++;
    }

    new_table[offset] = m->table[i];
  }

  free(m->table);
  m->table = new_table;
  m->limit = new_limit;
}



ident_value ident_map_intern(struct ident_map *m,
			     const uint8_t *buf,
			     size_t count) {
  size_t limit = m->limit;
  if (limit == 0) {
    ident_map_rebuild(m, 8);
  }
  size_t offset = ident_map_hash(buf, count) & (limit - 1);
  size_t step = 1;
  ident_value v;
  while ((v = m->table[offset].ident), v) {
    if (m->table[offset].count == count
	&& 0 == memcmp(m->table[offset].buf, buf, count)) {
      return v;
    }

    offset = size_add(offset, step) & (limit - 1);
    step++;
  }

  CHECK(m->prev_value != IDENT_VALUE_MAX);
  m->prev_value++;
  v = m->prev_value;
  m->table[offset].ident = v;
  m->table[offset].buf = buf;
  m->table[offset].count = count;
  m->count++;

  if (count >= m->limit / 2) {
    ident_map_rebuild(m, size_mul(limit, 2));
  }

  return v;
}
