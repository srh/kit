#include "build.h"

#include <stdlib.h>
#include <string.h>

#include "arith.h"
#include "checkstate.h"
#include "io.h"
#include "win/objfile.h"

int build_module(struct identmap *im, module_loader *loader,
                 ident_value name) {
  struct checkstate cs;
  checkstate_init(&cs, im);

  int ret = chase_modules_and_typecheck(&cs, loader, name);

  if (ret) {
    /* TODO: _Actually_ do some building. */
    void *buf;
    size_t buf_size;
    ret = make_almost_blank_objfile(&buf, &buf_size);
    if (ret) {
      const void *name_buf;
      size_t name_count;
      identmap_lookup(im, name, &name_buf, &name_count);
      char *path = malloc(size_add(name_count, 5));
      CHECK(path);
      memcpy(path, name_buf, name_count);
      memcpy(path + name_count, ".obj", 5);
      ret = write_file(path, buf, buf_size);
      free(path);
      free(buf);
    }
  }

  checkstate_destroy(&cs);
  return ret;
}
