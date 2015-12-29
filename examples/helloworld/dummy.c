#include <stdio.h>

/* stdout needs a wrapper to be used portably, because its definition
can be a macro. */
FILE *Stdout() {
  return stdout;
}
