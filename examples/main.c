#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>

extern int32_t kira_foo1;
int32_t kira_showfacs4(void);

int main(void) {
  printf("foo value: %" PRIu32 "\n", kira_foo1);

  kira_showfacs4();
  putchar('\n');

  return 0;
}
