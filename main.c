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
    fflush(stderr);
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
    fflush(stderr);
    return EXIT_FAILURE;
  }

  size_t leafcount;
  size_t error_pos;
  int ret;
  if (count_parse_buf(data, size, &leafcount, &error_pos)) {
    fprintf(stderr, "Parse succeeded.  Leaf count is %"PRIz".\n", leafcount);
    fflush(stderr);
    ret = EXIT_SUCCESS;
  } else {
    fprintf(stderr, "Parse failed, at %"PRIz".\n", error_pos);
    ret = EXIT_FAILURE;
  }

  free(data);
  return ret;
}

