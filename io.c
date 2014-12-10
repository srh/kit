#include "io.h"

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "util.h"

int read_file(const char *path,
	      uint8_t **data_out,
	      size_t *size_out) {
  FILE *fp = fopen(path, "rb");
  if (!fp) {
    return 0;
  }

  int res = fseek(fp, 0, SEEK_END);
  if (res != 0) {
    fclose(fp);
    return 0;
  }

  long offset = ftell(fp);
  if (offset < 0) {
    fclose(fp);
    return 0;
  }

  res = fseek(fp, 0, SEEK_SET);
  if (res != 0) {
    fclose(fp);
    return 0;
  }

  STATIC_CHECK(LONG_MAX < SIZE_MAX);
  size_t size = offset;
  uint8_t *buf = malloc(size);
  CHECK(buf);
  size_t count = fread(buf, 1, size, fp);
  if (count < size) {
    fclose(fp);
    return 0;
  }

  *data_out = buf;
  *size_out = size;
  fclose(fp);
  return 1;
}
