#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>

extern int32_t kira_foo1;
int32_t kira_goo2(int32_t);
int32_t kira_ioo4(int32_t);

int main(void) {
  printf("foo value: %" PRIu32 "\n", kira_foo1);

  kira_goo2('H');
  kira_goo2('A');
  kira_goo2('L');
  putchar('\n');
  kira_ioo4('I');
  kira_ioo4('B');
  kira_ioo4('M');
  putchar('\n');

  return 0;
}
