#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "arith.h"
#include "ast.h"
#include "identmap.h"
#include "io.h"
#include "parse.h"
#include "platform.h"
#include "build.h"
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

void print_usage(const char *arg0) {
  fprintf(stderr, "Usage: %s <filename>\n", arg0);
  fflush(stderr);
}

int main(int argc, char **argv) {
  int test = 0;
  enum target_platform platform = TARGET_PLATFORM_WIN_32BIT;
  const char *module = NULL;
  for (int i = 1; i < argc; i++) {
    if (test || module) {
      print_usage(argv[0]);
      return EXIT_FAILURE;
    }
    if (0 == strcmp(argv[i], "--test")) {
      test = 1;
    } else if (0 == strcmp(argv[i], "--linux")) {
      platform = TARGET_PLATFORM_LINUX_32BIT;
    } else if (0 == strcmp(argv[i], "--osx")) {
      platform = TARGET_PLATFORM_OSX_32BIT;
    } else {
      module = argv[i];
    }
  }

  if (test) {
    return run_tests();
  }

  if (module) {
    int ret = EXIT_FAILURE;

    struct identmap im;
    identmap_init(&im);
    ident_value ident_module = identmap_intern(&im, module, strlen(module));
    if (!build_module(&im, platform, NULL, &read_module_file, ident_module)) {
      goto cleanup_im;
    }


    DBG("Success?\n");
    ret = EXIT_SUCCESS;

  cleanup_im:
    identmap_destroy(&im);
    return ret;
  }

  print_usage(argv[0]);
  return EXIT_FAILURE;
}

