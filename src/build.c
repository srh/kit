#include "build.h"

#include <stdlib.h>
#include <string.h>

#include "arith.h"
#include "checkstate.h"
#include "io.h"
#include "win/objfile.h"

int build_module(struct identmap *im, module_loader *loader,
                 ident_value name) {
  int ret = 0;
  struct checkstate cs;
  checkstate_init(&cs, im);

  if (!chase_modules_and_typecheck(&cs, loader, name)) {
    goto cleanup_checkstate;
  }

  /* TODO: _Actually_ do some building. */
  void *buf;
  size_t buf_size;
  if (!make_almost_blank_objfile(&buf, &buf_size)) {
    goto cleanup_checkstate;
  }

  const void *name_buf;
  size_t name_count;
  identmap_lookup(im, name, &name_buf, &name_count);
  char *path;
  size_t path_count;
  alloc_half_strcat(name_buf, name_count, ".obj",
                    &path, &path_count);
  ret = write_file(path, buf, buf_size);
  free(path);
  free(buf);

 cleanup_checkstate:
  checkstate_destroy(&cs);
  return ret;
}
