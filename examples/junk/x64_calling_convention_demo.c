#include <stdint.h>
#include <stdio.h>

struct bi {
  uint32_t x, y, z, w;
};

struct uni {
  uint32_t x, y;
};

void bibi(struct bi x, struct bi y) {
  printf("%p %p\n", &x, &y);
}

void bibibi(struct bi x, struct bi y, struct bi z) {
  printf("%p %p %p\n", &x, &y, &z);
}

void unibibi(struct uni x, struct bi y, struct bi z) {
  printf("%p %p %p\n", &x, &y, &z);
}

void unibibiuni(struct uni x, struct bi y, struct bi z, struct uni t) {
  printf("%p %p %p %p\n", &x, &y, &z, &t);
}

void unibibibi(struct uni x, struct bi y, struct bi z, struct bi t, struct uni w) {
  printf("%p %p %p %p %p\n", &x, &y, &z, &t, &w);
}

