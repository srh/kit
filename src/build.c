#include "build.h"

#include "checkstate.h"

int build_module(struct identmap *im, module_loader *loader,
                 ident_value name) {
  struct checkstate cs;
  checkstate_init(&cs, im);

  int ret = chase_modules_and_typecheck(&cs, loader, name);

  /* TODO: Actually do some building. */

  checkstate_destroy(&cs);
  return ret;
}
