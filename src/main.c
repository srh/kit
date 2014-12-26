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
  if (ret && ret2) {
    DBG("\nAll tests PASS!  :)\n");
  } else {
    DBG("\nA test FAILED.  You're a bad programmer!  :(\n");
  }
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

  struct identmap im;
  identmap_init(&im);
  ident_value ident_module = identmap_intern(&im, module, strlen(module));
  if (!check_module(&im, &read_module_file, ident_module)) {
    goto cleanup_im;
  }


  DBG("Success?\n");
  ret = EXIT_SUCCESS;

 cleanup_im:
  identmap_destroy(&im);
  return ret;
}

