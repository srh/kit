#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>

extern int32_t kira_foo1;
int32_t kira_ioo2(int32_t);

int main(void) {
  printf("foo value: %" PRIu32 "\n", kira_foo1);

  kira_ioo2('H');
  kira_ioo2('A');
  kira_ioo2('L');
  putchar('\n');

  return 0;
}
