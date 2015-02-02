#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>

struct ty {
  int32_t x;
  int32_t y;
};

extern int32_t kira_foo1;
struct ty kira_showfacs3(struct ty *t, uint16_t *num);

int32_t flushstdout(void) {
  fflush(stdout);
  return 0;
}

int main(void) {
  printf("foo value: %" PRIu32 "\n", kira_foo1);

  struct ty blah[2];
  uint16_t num;
  struct ty res = kira_showfacs3(blah, &num);
  printf("num = %"PRIu16"\n", num);
  printf("blah[0].x = %"PRIi32", .y = %"PRIi32"\n", blah[0].x, blah[0].y);
  printf("blah[1].x = %"PRIi32", .y = %"PRIi32"\n", blah[1].x, blah[1].y);
  printf("res.x = %"PRIi32", .y = %"PRIi32"\n", res.x, res.y);

  return 0;
}
