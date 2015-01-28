#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>

extern int32_t kira_foo1;
int32_t kira_ioo2(int32_t);

int main(void) {
  printf("foo value: %" PRIu32 "\n", kira_foo1);

  kira_ioo2('@');
  kira_ioo2('A');
  kira_ioo2('B');
  kira_ioo2('C');
  kira_ioo2('D');
  kira_ioo2('E');
  putchar('\n');

  return 0;
}
