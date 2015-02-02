#include <stdio.h>
#include <stdint.h>
#include <time.h>

int32_t kira_runloop1(void);

int main(void) {
  clock_t c = clock();

#if 0
  size_t acc = kira_runloop1();
#else
  size_t acc = 0;
  for (size_t i = 0; i < 1000000000; i++) {
    acc += i;
  }
#endif

  clock_t d = clock();

  printf("%u %f\n", acc, (d - c) * (1.0 / CLOCKS_PER_SEC));
  return 0;
}
