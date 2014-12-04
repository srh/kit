#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "io.h"
#include "parse.h"
#include "util.h"

int run_tests(void) {
  DBG("Running parse_test...\n");
  int ret = parse_test();
  DBG("parse test: %s\n", ret ? "PASS" : "FAIL");
  return ret;
}

int main(int argc, char **argv) {
  if (argc != 2) {
    fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
    return EXIT_FAILURE;
  }

  const char *path = argv[1];

  if (0 == strcmp(path, "--test")) {
    return run_tests();
  }

  uint8_t *data;
  size_t size;
  if (!read_file(path, &data, &size)) {
    fprintf(stderr, "Could not read %s\n", path);
    return EXIT_FAILURE;
  }

  free(data);

  return 0;
}

