#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>

struct ty {
  int32_t x;
  int32_t y;
};

extern int32_t kira_foo1;
int32_t kira_showfacs3(struct ty t);

int32_t flushstdout(void) {
  fflush(stdout);
  return 0;
}

int main(void) {
  printf("foo value: %" PRIu32 "\n", kira_foo1);

  struct ty blah = { 0, 0 };
  kira_showfacs3(blah);
  putchar('\n');

  return 0;
}
