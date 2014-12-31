#include "io.h"

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "util.h"

int read_file(const char *path,
              uint8_t **data_out,
              size_t *size_out) {
  int ret = 0;
  FILE *fp = fopen(path, "rb");
  if (!fp) {
    goto cleanup;
  }

  int res = fseek(fp, 0, SEEK_END);
  if (res != 0) {
    goto cleanup_fp;
  }

  long offset = ftell(fp);
  if (offset < 0) {
    goto cleanup_fp;
  }

  res = fseek(fp, 0, SEEK_SET);
  if (res != 0) {
    goto cleanup_fp;
  }

  STATIC_CHECK(LONG_MAX < SIZE_MAX);
  size_t size = offset;
  uint8_t *buf = malloc_mul(size, 1);
  CHECK(buf);
  size_t count = fread(buf, 1, size, fp);
  if (count < size) {
    goto cleanup_fp;
  }

  *data_out = buf;
  *size_out = size;
  ret = 1;
 cleanup_fp:
  fclose(fp);
 cleanup:
  return ret;
}

int write_file(const char *path,
               const void *buf,
               size_t buf_size) {
  int ret = 0;
  FILE *fp = fopen(path, "wb");
  if (!fp) {
    goto cleanup;
  }

  size_t nwrite = fwrite(buf, 1, buf_size, fp);
  if (nwrite != buf_size) {
    goto cleanup_fp;
  }

  ret = 1;
 cleanup_fp:
  if (fclose(fp) != 0) {
    ret = 0;
  }
 cleanup:
  return ret;
}
