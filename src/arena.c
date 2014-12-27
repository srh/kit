#include "arena.h"

#include <stdlib.h>

#include "util.h"

#define ONE_OFF_ALLOCATION_THRESHOLD 256
#define SEGMENT_SIZE 1024

struct arena_segment {
  void *buf;
  struct arena_segment *next_segment;
};

void arena_init(struct arena *a) {
  a->buf = NULL;
  a->offset = 0;
  a->old_segments = NULL;
}

void arena_init_move(struct arena *a, struct arena *movee) {
  a->buf = movee->buf;
  a->offset = movee->offset;
  a->old_segments = movee->old_segments;
  arena_init(movee);
}

void arena_destroy(struct arena *a) {
  free(a->buf);
  struct arena_segment *seg = a->old_segments;

  a->buf = NULL;
  a->offset = 0;
  a->old_segments = NULL;

  while (seg) {
    free(seg->buf);
    struct arena_segment *tmp = seg->next_segment;
    seg->buf = NULL;
    seg->next_segment = NULL;
    free(seg);
    seg = tmp;
  }
}

void *arena_unaligned(struct arena *a, size_t count) {
  if (a->offset < count) {
    if (count >= ONE_OFF_ALLOCATION_THRESHOLD) {
      void *buf = malloc(count);
      CHECK(buf);
      struct arena_segment *seg = malloc(sizeof(*seg));
      CHECK(seg);
      seg->buf = buf;
      seg->next_segment = a->old_segments;
      a->old_segments = seg;
      return buf;
    }

    if (a->buf) {
      struct arena_segment *seg = malloc(sizeof(*seg));
      CHECK(seg);
      seg->buf = a->buf;
      seg->next_segment = a->old_segments;
      a->old_segments = seg;
    }

    a->buf = malloc(SEGMENT_SIZE);
    CHECK(a->buf);
    a->offset = SEGMENT_SIZE;
  }

  a->offset -= count;
  return a->buf + a->offset;
}

void *arena_small_aligned(struct arena *a, size_t count, size_t alignment) {
  CHECK(alignment > 0 && (alignment & (alignment - 1)) == 0);
  CHECK((count & (alignment - 1)) == 0);
  CHECK(count < ONE_OFF_ALLOCATION_THRESHOLD);
  a->offset &= ~(alignment - 1);
  return arena_unaligned(a, count);
}
