#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

struct ty {
  int32_t x;
  int32_t y;
};

struct ty showfacs(struct ty *t, uint16_t *num);

int32_t flushstdout(void) {
  fflush(stdout);
  return 0;
}

int main(void) {
  struct ty blah[2];
  uint16_t num;
  struct ty res = showfacs(blah, &num);
  printf("num = %"PRIu16"\n", num);
  printf("blah[0].x = %"PRIi32", .y = %"PRIi32"\n", blah[0].x, blah[0].y);
  printf("blah[1].x = %"PRIi32", .y = %"PRIi32"\n", blah[1].x, blah[1].y);
  printf("res.x = %"PRIi32", .y = %"PRIi32"\n", res.x, res.y);

  return 0;
}
