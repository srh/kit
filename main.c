#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "identmap.h"
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

  int ret;
  struct ident_map im;
  ident_map_init(&im);
  struct ast_file file;
  size_t error_pos;
  if (!parse_buf_file(&im, data, size, &file, &error_pos)) {
    fprintf(stderr, "Parse failed, at %"PRIz".\n", error_pos);
    ret = EXIT_FAILURE;
    goto cleanup_im;
  }

  fprintf(stderr, "Parse succeeded.\n");
  fflush(stderr);
  ret = EXIT_SUCCESS;

 cleanup_im:
  ident_map_destroy(&im);
  free(data);
  return ret;
}

