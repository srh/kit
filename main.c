#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "arith.h"
#include "ast.h"
#include "identmap.h"
#include "io.h"
#include "parse.h"
#include "typecheck.h"
#include "util.h"

int run_tests(void) {
  DBG("Running parse_test...\n");
  int ret = parse_test();
  DBG("parse test: %s\n", ret ? "PASS" : "FAIL");
  int ret2 = test_check_file();
  DBG("test_check_file: %s\n", ret2 ? "PASS" : "FAIL");
  return ret && ret2;
}

int main(int argc, char **argv) {
  if (argc != 2) {
    fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
    fflush(stderr);
    return EXIT_FAILURE;
  }

  const char *module = argv[1];

  if (0 == strcmp(module, "--test")) {
    return run_tests();
  }

  int ret = EXIT_FAILURE;

  struct ident_map im;
  ident_map_init(&im);
  ident_value ident_module = ident_map_intern(&im, module, strlen(module));
  if (!check_module(&im, &read_module_file, ident_module)) {
    goto cleanup_im;
  }


  DBG("Success?\n");
  ret = EXIT_SUCCESS;

 cleanup_im:
  ident_map_destroy(&im);
  return ret;
}

