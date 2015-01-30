#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>

struct ty {
  int32_t x;
  int32_t y;
};

extern int32_t kira_foo1;
struct ty kira_showfacs3(struct ty *t);

int32_t flushstdout(void) {
  fflush(stdout);
  return 0;
}

int main(void) {
  printf("foo value: %" PRIu32 "\n", kira_foo1);

  struct ty blah = { 0, 0 };
  struct ty res = kira_showfacs3(&blah);
  printf("blah.x = %"PRIi32", .y = %"PRIi32"\n", blah.x, blah.y);
  printf("res.x = %"PRIi32", .y = %"PRIi32"\n", res.x, res.y);

  return 0;
}
