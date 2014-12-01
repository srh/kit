#include "util.h"

#include <stdio.h>
#include <stdlib.h>

void report_and_abort(const char *file, int line,
		      const char *msg1, const char *msg2) {
  fprintf(stderr, "Fatal error at %s:%d: %s%s\n",
	  file, line, msg1, msg2);
  abort();
}
