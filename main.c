#include <stdio.h>
#include <stdlib.h>

#include "io.h"

int main(int argc, char **argv) {
  if (argc != 2) {
    fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
    return EXIT_FAILURE;
  }

  const char *path = argv[1];

  uint8_t *data;
  size_t size;
  if (!read_file(path, &data, &size)) {
    fprintf(stderr, "Could not read %s\n", path);
    return EXIT_FAILURE;
  }

  free(data);

  return 0;
}

