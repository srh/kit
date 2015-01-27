#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>

extern int32_t kira_foo1;
int32_t kira_goo2(int32_t);

int main(void) {
  printf("foo value: %" PRIu32 "\n", kira_foo1);

  kira_goo2('H');
  kira_goo2('A');
  kira_goo2('L');
  kira_goo2('\n');

  return 0;
}
