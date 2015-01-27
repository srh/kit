#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>

extern int32_t kira_foo1;
int32_t kira_ioo2(int32_t);

int main(void) {
  printf("foo value: %" PRIu32 "\n", kira_foo1);

  kira_ioo2('I');
  kira_ioo2('B');
  kira_ioo2('M');
  putchar('\n');

  return 0;
}
